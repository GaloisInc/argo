package com.galois.cryptol.client.connection;

import java.util.*;
import java.io.*;
import java.net.*;
import java.util.concurrent.*;
import java.util.concurrent.atomic.*;
import java.util.function.*;

import com.eclipsesource.json.*;

import com.galois.cryptol.client.connection.*;
import com.galois.cryptol.client.connection.queue.*;

public class JsonConnection implements AutoCloseable {

    private static final String version = "2.0";

    private final ConcurrentMultiQueue<JsonValue, JsonResponse> responseQueue =
        new ConcurrentMultiQueue<>();
    private final AtomicInteger nextId =
        new AtomicInteger(0);
    private final Map<JsonValue, Call<?, ?>> pendingCalls =
        new ConcurrentHashMap<>();

    private final Pipe<JsonValue> pipe;

    private volatile boolean closed = false;

    private static class JsonResponse {
        private final JsonValue result;
        private final JsonRpcException error;

        JsonResponse(JsonValue result) {
            this.result = result;
            this.error = null;
        }

        JsonResponse(JsonRpcException error) {
            this.result = null;
            this.error = error;
        }

        public JsonValue result() throws JsonRpcException {
            if (this.result != null) {
                return this.result;
            } else {
                throw this.error;
            }
        }

        public static JsonResponse parse(JsonObject object) {
            try {
                JsonValue result  = object.get("result");  // might be null
                JsonValue error  = object.get("error");    // might be null
                if (result != null && error != null) {
                    var msg = "Both response and error fields are present";
                    throw new InvalidRpcResponseException(msg + ": " + object);
                } else if (error != null) {
                    return new JsonResponse(new JsonRpcException(error.asObject()));
                } else if (result != null) {
                    return new JsonResponse(result);
                } else {
                    var msg = "Neither response nor error fields are present";
                    throw new InvalidRpcResponseException(msg + ": " + object);
                }
            } catch (UnsupportedOperationException e) {
                var msg = "Error field is not an object";
                throw new InvalidRpcResponseException(msg + ": " + object, e);
            }
        }
    }

    private void sendCall(JsonValue id, Call<?, ?> call) {
        JsonValue message = Json.object()
            .add("jsonrpc", version)
            .add("id", id)
            .add("method", call.method())
            .add("params", call.params());
        try {
            pipe.send(message);
        } catch (Exception e) {
            throw new ConnectionException(e);
        }
    }

    private void sendNotification(Notification notification) {
        JsonValue message = Json.object()
            .add("jsonrpc", version)
            .add("method", notification.method())
            .add("params", notification.params());
        try {
            pipe.send(message);
        } catch (Exception e) {
            throw new ConnectionException(e);
        }
    }

    public JsonConnection(ConnectionManager<JsonValue> connectionManager,
                          Consumer<Throwable> handleException) {

        // When the connection dies, resend all pending calls to the new
        // connection -- that way, waiting calling threads don't block forever
        this.pipe = new ManagedPipe<>(connectionManager, () -> {
                pendingCalls.forEach(this::sendCall);
        });

        Thread checkResponses = new Thread(() -> {
            try {
                while (!pipe.isClosed()) {
                    JsonObject object;
                    try {
                        object = pipe.receive().asObject();
                    } catch (UnsupportedOperationException e) {
                        var msg = "Response is not an object";
                        var err = new InvalidRpcResponseException(msg, e);
                        throw err;
                    } catch (ConnectionException e) {
                        break; // The pipe cannot be re-created and is broken
                    } catch (NoSuchElementException e) {
                        break; // The pipe was permanently closed
                    }
                    JsonValue id = object.get("id");
                    JsonResponse response = JsonResponse.parse(object);
                    if (id != null) {
                        pendingCalls.remove(id); // call is no longer pending
                        responseQueue.send(id, response);
                    } else {
                        try {
                            JsonValue result = response.result();
                            var msg = "Non-error response had no id: " + object;
                            var err = new InvalidRpcResponseException(msg);
                            throw err;
                        } catch (JsonRpcException err) {
                            throw new UnhandledRpcException(err);
                        }
                    }
                }
            } catch (QueueClosedException e) {
                // This means responseQueue.send(id, response) discovered that
                // responseQueue was closed, which means someone called close()
                // on the connection
            } finally {
                // After exhausting the requests, close the multiqueue
                responseQueue.close();
            }
        });

        checkResponses.setUncaughtExceptionHandler((t, e) -> {
                handleException.accept(e);
            });

        // Start the background thread
        checkResponses.start();
    }

    public <O, E extends Exception> O call(Call<O, E> call) throws E {
        JsonValue id = Json.value(nextId.getAndIncrement());
        pendingCalls.put(id, call); // call is now pending
        this.sendCall(id, call);
        try {
            JsonValue response = responseQueue.request(id).result();
            O result = call.decode(response);
            if (result == null) {
                throw new InvalidRpcCallResultException(response);
            } else {
                return result;
            }
        } catch (JsonRpcException e) {
            E exception = call.handle(e);
            if (exception == null) {
            throw new UnhandledRpcException(e);
            } else {
                throw exception;
            }
        } catch (QueueClosedException e) {
            throw new ConnectionException("Connection closed");
        }
    }

    public void notify(Notification notification) {
        sendNotification(notification);
    }

    public synchronized void close() throws IOException {
        if (!this.closed) {
            this.responseQueue.close();
            this.pipe.close();
            this.closed = true;
        }
    }
}
