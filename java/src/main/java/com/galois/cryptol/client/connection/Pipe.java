package com.galois.cryptol.client.connection;

import java.util.*;
import java.io.*;

public interface Pipe<A> extends AutoCloseable {
    public void send(A input);
    public A receive() throws NoSuchElementException;
    @Override
    public void close() throws IOException;
    public boolean isClosed();
}
