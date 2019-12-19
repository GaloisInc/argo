package com.galois.cryptol.client.connection;

public class InvalidRpcResponseException extends RuntimeException {
    public static final long serialVersionUID = 0;
    public InvalidRpcResponseException(String e) { super(e); }
    public InvalidRpcResponseException(Throwable e) { super(e); }
    public InvalidRpcResponseException(String m, Throwable e) { super(m, e); }
}
