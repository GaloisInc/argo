package com.galois.cryptol.client.connection;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.connection.*;

public interface Call<O, E extends Exception> {
    // Method name as server knows it (should be constant function)
    public String method();
    // The parameters to this particular invocation (should be constant function)
    public JsonValue params();
    // How to convert the server response to the function call result; should
    // return null if it cannot be decoded
    public O decode(JsonValue o);
    // How to convert any server-returned exception to either a result, or a
    // custom exception -- returning null will cause the original
    // JsonRpcException to be rethrown as a runtime exception (this should only
    // be done if the server violates protocol and sends a truly unexpected
    // error not encompassed by E)
    public E handle(JsonRpcException e);
}
