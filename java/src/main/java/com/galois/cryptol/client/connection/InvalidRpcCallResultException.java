package com.galois.cryptol.client.connection;

import com.eclipsesource.json.*;

public class InvalidRpcCallResultException extends RuntimeException {
    public static final long serialVersionUID = 0;
    public final JsonValue invalidResult;
    public InvalidRpcCallResultException (JsonValue value) {
        super(value.toString());
        this.invalidResult = value;
    }
}
