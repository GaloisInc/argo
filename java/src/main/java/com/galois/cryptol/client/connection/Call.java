package com.galois.cryptol.client.connection;

import java.util.function.*;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.connection.*;

public class Call<O, E extends Exception> extends Notification {
    // How to convert the server response to the function call result; should
    // return null if it cannot be decoded
    public O decode(JsonValue o) { return this.decoder.apply(o); };
    // How to convert any server-returned exception to either a result, or a
    // custom exception -- returning null will cause the original
    // JsonRpcException to be rethrown as a runtime exception (this should only
    // be done if the server violates protocol and sends a truly unexpected
    // error not encompassed by E)
    public E handle(JsonRpcException e) { return this.handler.apply(e); }

    protected final Function<JsonValue, O> decoder;
    protected final Function<JsonRpcException, E> handler;

    public Call(String method, JsonValue params,
                Function<JsonValue, O> decode,
                Function<JsonRpcException, E> handle) {
        super(method, params);
        this.decoder = decode;
        this.handler = handle;
    }

    public Call(Notification notification,
                Function<JsonValue, O> decode,
                Function<JsonRpcException, E> handle) {
        super(notification.method(), notification.params());
        this.decoder = decode;
        this.handler = handle;
    }
}
