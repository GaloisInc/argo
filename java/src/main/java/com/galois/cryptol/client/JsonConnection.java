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
    private final Function<JsonValue, Boolean> requests;
    private final ConcurrentMultiQueue<JsonValue, JsonValue> responses;
    private final Thread checkResponses;

    public JsonConnection(Function<JsonValue, Boolean> output, Iterator<JsonValue> input) {
        this.ids = new IDSource();
        this.requests = output;
        this.responses = new ConcurrentMultiQueue<JsonValue, JsonValue>();
        this.checkResponses = new Thread(() -> {
            try {
                input.forEachRemaining(value -> {
                        JsonValue id = value.asObject().get("id");
                        if (id != null) {
                            this.responses.send(id, value);
                        } else {
                            var err = "Response missing id field";
                            throw new UnsupportedOperationException(err);
                        }
                    });
            } catch (IllegalStateException e) {
                // The multiqueue was closed, so we stopped iterating
            }
            // After exhausting the input, close the multiqueue
            this.responses.close();
        });
        this.checkResponses.start();
    }

    public JsonValue call(String method, JsonValue params) throws IOException {
        JsonValue id = Json.value(ids.next());
        JsonValue message = Json.object()
            .add("jsonrpc", version)
            .add("id", id)
            .add("method", method)
            .add("params", params);
        boolean success = requests.apply(message);
        if (success) {
            return responses.request(id);
            // TODO: process this to actually return the internal result!
        } else {
            throw new IOException("Exception during method call");
        }
    }

    public void close() throws InterruptedException {
        this.responses.close();
        this.checkResponses.join();
    }
}
