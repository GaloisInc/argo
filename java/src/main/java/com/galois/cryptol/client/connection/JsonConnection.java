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

public class JsonConnection {

    private static final String version = "2.0";

    private final Consumer<JsonValue> requests;
    private final ConcurrentMultiQueue<JsonValue, JsonResponse> responseQueue;
    private final AtomicInteger nextId;

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

    public JsonConnection(JsonConnection connection) {
        this.nextId = connection.nextId;
        this.requests = connection.requests;
        this.responseQueue = connection.responseQueue;
    }

    public JsonConnection(Consumer<JsonValue> requests,
                          Iterator<JsonValue> responses,
                          Function<Exception, Boolean> handleException) {
        this.nextId = new AtomicInteger(0);
        this.requests = requests;
        this.responseQueue = new ConcurrentMultiQueue<JsonValue, JsonResponse>();

        Thread checkResponses = new Thread(() -> {
            try {
                boolean ok = true;
                while (responses.hasNext() && ok) {
                    JsonObject object;
                    try {
                        object = responses.next().asObject();
                    } catch (UnsupportedOperationException e) {
                        var msg = "Response is not an object";
                        var err = new InvalidRpcResponseException(msg, e);
                        ok = handleException.apply(err);  // continue?
                        return;
                    }
                    JsonValue id = object.get("id");
                    JsonResponse response = JsonResponse.parse(object);
                    if (id != null) {
                        responseQueue.send(id, response);
                    } else {
                        try {
                            JsonValue result = response.result();
                            var msg = "Non-error response had no id: " + object;
                            var err = new InvalidRpcResponseException(msg);
                            ok = handleException.apply(err);   // continue?
                        } catch (Exception err) {
                            ok = handleException.apply(err);  // continue?
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

        // Start the background thread
        checkResponses.start();
    }

    public <O, E extends Exception> O call(String method,
                                           JsonValue params,
                                           Function<JsonValue, O> decode,
                                           Function<JsonRpcException, E> handle)
        throws E, ConnectionException {
        Call<O, E> call = new Call<O, E>(method, params, decode, handle);
        return this.call(call);
    }

    public <O, E extends Exception> O call(Call<O, E> call)
        throws E, ConnectionException {
        JsonValue id = Json.value(nextId.getAndIncrement());
        JsonValue message = Json.object()
            .add("jsonrpc", version)
            .add("id", id)
            .add("method", call.method())
            .add("params", call.params());
        try {
            synchronized(requests) {
                requests.accept(message);
            }
        } catch (Exception e) {
            throw new ConnectionException(e);
        }
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

    public void notify(String method, JsonValue params)
        throws ConnectionException {
        this.notify(new Notification(method, params));
    }

    public void notify(Notification notification)
        throws ConnectionException {
        JsonValue message = Json.object()
            .add("jsonrpc", version)
            .add("method", notification.method())
            .add("params", notification.params());
        try {
            synchronized(requests) {
                requests.accept(message);
            }
        } catch (Exception e) {
            throw new ConnectionException(e);
        }
    }

    public void close() {
        this.responseQueue.close();
    }
}
