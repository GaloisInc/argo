package com.galois.cryptol.client.connection;

import com.eclipsesource.json.*;

public interface Notification {
    // Method name as server knows it (should be constant function)
    public String method();
    // The parameters to this particular invocation (should be constant function)
    public JsonValue params();
}
