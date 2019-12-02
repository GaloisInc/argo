package com.galois.cryptol.client.connection;

import com.eclipsesource.json.*;

public class JsonRpcNotification {
    // Method name as server knows it (should be constant function)
    public String method() { return method; }
    // The parameters to this particular invocation (should be constant function)
    public JsonValue params() { return params; }

    private final String method;
    private final JsonValue params;

    public JsonRpcNotification(String method, JsonValue params) {
        this.method = method;
        this.params = params;
    }
}
