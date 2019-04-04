package com.galois.cryptol.client;

import com.galois.cryptol.client.JsonConnection.*;
import com.eclipsesource.json.*;

class JsonRpcException extends Exception {
    public static final long serialVersionUID = 0;
    public final int code;
    public final String message;
    public final JsonValue data;

    public JsonRpcException(int code, String message, JsonValue data) {
        this.code = code;
        this.message = message;
        this.data = data;
    }

    public JsonRpcException(JsonObject error) throws InvalidRpcResponseException {
        try {
            this.code = error.get("code").asInt();
            this.message = error.get("message").asString();
            this.data = error.get("data");
        } catch (NullPointerException e) {
            var msg = "Missing field in error response object: " + error;
            throw new InvalidRpcResponseException(msg, e);
        } catch (UnsupportedOperationException e) {
            var msg = "Bad format for error response object: " + error;
            throw new InvalidRpcResponseException(msg, e);
        }
    }
}
