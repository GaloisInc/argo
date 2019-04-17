package com.galois.cryptol.client.connection;

import java.util.*;
import java.util.function.*;
import java.io.*;

public class ManagedPipe<A> implements Pipe<A> {

    private static int defaultMaxRetries = 1;

    private volatile Pipe<A> pipe;
    private final ConnectionManager<A> newPipe;
    private final int maxRetries;

    public ManagedPipe(ConnectionManager<A> newPipe) {
        this(newPipe, ManagedPipe.defaultMaxRetries);
    }

    public ManagedPipe(ConnectionManager<A> newPipe, int maxRetries) {
        this.newPipe = newPipe;
        this.maxRetries = maxRetries;
    }

    private void initPipe() {
        if (pipe == null) {
            pipe = newPipe.get();
        }
    }

    public void send(A input) {
        initPipe();
        int tries = maxRetries;
        while (true) {
            try {
                this.pipe.send(input);
                return;
            } catch (Exception e) {
                if (tries > 0) {
                    // System.err.println("Retrying send after error: " + e);
                    this.pipe = this.newPipe.get();
                    tries--;
                } else {
                    throw e;
                }
            }
        }
    }

    public A receive() {
        try {
            initPipe();
            int tries = maxRetries;
            while (true) {
                try {
                    return this.pipe.receive();
                } catch (Exception e) {
                    if (tries > 0) {
                        // System.err.println("Retrying receive after error: " + e);
                        this.pipe = this.newPipe.get();
                        tries--;
                    } else {
                        throw e;
                    }
                }
            }
        } catch (IllegalStateException e) {
            throw new NoSuchElementException();
        }
    }

    public void close() throws IOException {
        this.pipe.close();
    }

    public boolean isClosed() {
        return pipe != null ? this.pipe.isClosed() : false;
    }
}
