package com.galois.cryptol.client;

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
}
