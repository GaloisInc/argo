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
    private final ConcurrentMultiQueue<JsonValue, JsonValue> responses;
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

    public static class JsonRpcConnectionException extends JsonRpcRequestException {
        public static final long serialVersionUID = 0;
        public JsonRpcConnectionException(String e) { super(e); }
        public JsonRpcConnectionException(Throwable e) { super(e); }
        public JsonRpcConnectionException(String m, Throwable e) { super(m, e); }
    }

    public JsonConnection(Consumer<JsonValue> output, Iterator<JsonValue> input) {
        this.ids = new IDSource();
        this.requests = output;
        this.responses = new ConcurrentMultiQueue<JsonValue, JsonValue>();

        this.checkResponses = new Thread(() -> {
            try {
                input.forEachRemaining(value -> {
                        try {
                            JsonValue id = value.asObject().get("id");
                            this.responses.send(id, value);
                        } catch (NullPointerException e) {
                            var msg = "Response object is missing id field";
                            throw new JsonRpcResponseException(msg, e);
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
            JsonObject response = responses.request(id).asObject();
            JsonValue result = response.get("result");
            if (result != null) {
                if (response.get("error") == null) {
                    return result;
                } else {
                    throw new JsonRpcResponseException("Both result and error");
                }
            } else {
                JsonObject error = response.get("error").asObject();
                try {
                    int code = error.get("code").asInt();
                    String errorMessage = error.get("message").asString();
                    JsonValue data = error.get("data");
                    throw new JsonRpcException(code, errorMessage, data);
                } catch (NullPointerException e) {
                    var msg = "Missing field in error response object";
                    throw new JsonRpcResponseException(msg, e);
                }
            }
        } catch (UnsupportedOperationException e) {
            throw new JsonRpcResponseException(e);
        } catch (NullPointerException e) {
            var msg = "Invalid format in response object";
            throw new JsonRpcResponseException(msg);
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
