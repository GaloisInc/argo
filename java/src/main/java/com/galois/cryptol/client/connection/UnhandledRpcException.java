package com.galois.cryptol.client.connection;

import com.galois.cryptol.client.connection.JsonRpcException;

public class UnhandledRpcException extends RuntimeException {
    public static final long serialVersionUID = 0;
    public UnhandledRpcException(JsonRpcException e) { super(e); }
}
