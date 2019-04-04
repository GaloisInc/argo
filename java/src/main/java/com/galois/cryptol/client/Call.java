package com.galois.cryptol.client;

import com.eclipsesource.json.*;

interface Call<O, E extends Exception> {

    // To be thrown when handleException... can't handle it
    public static class UnexpectedRpcException extends Exception {
        public static final long serialVersionUID = 0;
        public UnexpectedRpcException() { }
    }

    // To be thrown when the JSON format of the result is invalid
    public static class UnexpectedRpcResultException extends Exception {
        public static final long serialVersionUID = 0;
        public UnexpectedRpcResultException() { }
    }

    // Method name as server knows it (should be constant function)
    public String method();
    // The parameters to this particular invocation (should be constant function)
    public JsonValue params();
    // How to convert the server response to the function call result
    public O decodeResult(JsonValue o)
        throws UnexpectedRpcResultException;
    // How to convert any server-returned exception to either a result, or a
    // custom exception -- throwing UnexpectedRpcException will cause the
    // original JsonRpcException to be rethrown as a runtime exception (this
    // should only be done if the server violates protocol and sends a truly
    // unexpected error not encompassed by E)
    public O handleException(JsonRpcException e)
        throws E, UnexpectedRpcException;
}
