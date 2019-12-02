package com.galois.cryptol.client.connection;

public class ConnectionException extends RuntimeException {
    public static final long serialVersionUID = 0;
    public ConnectionException(String e) { super(e); }
    public ConnectionException(Throwable e) { super(e); }
    public ConnectionException(String m, Throwable e) { super(m, e); }
}
