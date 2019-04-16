package com.galois.cryptol.client.connection;

public interface Pipe<A> {
    public void send(A input);
    public A receive();
}
