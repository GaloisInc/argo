package com.galois.cryptol.client;

import java.util.*;
import java.io.*;
import java.net.*;
import java.util.concurrent.*;
import java.util.function.*;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.*;

class JsonConnection {

    private final String version = "2.0";

    private final IDSource ids;
    private final Consumer<JsonValue> requests;
    private final ConcurrentMultiQueue<JsonValue, JsonResponse> responses;
    private final Thread checkResponses;
    private volatile Throwable connectionException = null;

    public static class JsonRpcResponseException extends RuntimeException {
        public static final long serialVersionUID = 0;
        public JsonRpcResponseException(String e) { super(e); }
        public JsonRpcResponseException(Throwable e) { super(e); }
        public JsonRpcResponseException(String m, Throwable e) { super(m, e); }
    }

    public static class JsonRpcRequestException extends Exception {
        public static final long serialVersionUID = 0;
        public JsonRpcRequestException(String e) { super(e); }
        public JsonRpcRequestException(Throwable e) { super(e); }
        public JsonRpcRequestException(String m, Throwable e) { super(m, e); }
    }

    public static class JsonRpcConnectionException
        extends JsonRpcRequestException {
        public static final long serialVersionUID = 0;
        public JsonRpcConnectionException(String e) { super(e); }
        public JsonRpcConnectionException(Throwable e) { super(e); }
        public JsonRpcConnectionException(String m, Throwable e) { super(m, e); }
    }

    public static class JsonResponse {
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
                    throw new JsonRpcResponseException(msg + ": " + object);
                } else if (error != null) {
                    return new JsonResponse(new JsonRpcException(error.asObject()));
                } else if (result != null) {
                    return new JsonResponse(result);
                } else {
                    var msg = "Neither response nor error fields are present";
                    throw new JsonRpcResponseException(msg + ": " + object);
                }
            } catch (UnsupportedOperationException e) {
                var msg = "Error field is not an object";
                throw new JsonRpcResponseException(msg + ": " + object, e);
            }
        }
    }

    public JsonConnection(Consumer<JsonValue> output, Iterator<JsonValue> input) {
        this.ids = new IDSource();
        this.requests = output;
        this.responses = new ConcurrentMultiQueue<JsonValue, JsonResponse>();

        this.checkResponses = new Thread(() -> {
            try {
                input.forEachRemaining(value -> {
                        try {
                            JsonObject object = value.asObject();
                            JsonValue id = object.get("id");
                            JsonResponse response = JsonResponse.parse(object);
                            if (id != null) {
                                responses.send(id, response);
                            } else {
                                try {
                                    JsonValue result = response.result();
                                    var msg = "Non-error response had no id: " + object;
                                    throw new JsonRpcResponseException(msg);
                                } catch (JsonRpcException error) {
                                    // TODO: log unidentified response errors?
                                }
                            }
                        } catch (UnsupportedOperationException e) {
                            var msg = "Response is not an object";
                            throw new JsonRpcResponseException(msg, e);
                        }
                    });
            } finally {
                // After exhausting the input, close the multiqueue
                this.responses.close();
            }
        });

        // If the thread dies from an exception, write it out so we can re-throw
        this.checkResponses.setUncaughtExceptionHandler((thread, e) -> {
                // If there's an exception, close the queue so remaining
                // requests can be processed, but future ones will throw
                this.responses.close();
                this.connectionException = e;
            });

        this.checkResponses.start();
    }

    public JsonValue call(String method, JsonValue params)
        throws JsonRpcException,
               JsonRpcRequestException,
               JsonRpcConnectionException {
        JsonValue id = Json.value(ids.next());
        JsonValue message = Json.object()
            .add("jsonrpc", version)
            .add("id", id)
            .add("method", method)
            .add("params", params);
        try {
            requests.accept(message);
        } catch (Exception e) {
            throw new JsonRpcRequestException(e);
        }
        try {
            return responses.request(id).result();
        } catch (QueueClosedException e) {
            if (connectionException == null) {
                throw new JsonRpcConnectionException("Connection closed");
            } else {
                throw new JsonRpcResponseException(connectionException);
            }
        }
    }

    public void notify(String method, JsonValue params)
        throws JsonRpcRequestException {
        JsonValue message = Json.object()
            .add("jsonrpc", version)
            .add("method", method)
            .add("params", params);
        requests.accept(message);
    }

    public void close() throws InterruptedException {
        this.responses.close();
    }
}
