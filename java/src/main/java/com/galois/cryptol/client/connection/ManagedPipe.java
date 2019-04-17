package com.galois.cryptol.client.connection;

import java.util.*;
import java.util.function.*;

public class ManagedPipe<A> implements Pipe<A> {

    private static int defaultMaxRetries = 1;

    private volatile Pipe<A> pipe;
    private final Supplier<Pipe<A>> newPipe;
    private final int maxRetries;

    public ManagedPipe(Supplier<Pipe<A>> newPipe) {
        this(newPipe, ManagedPipe.defaultMaxRetries);
    }

    public ManagedPipe(Supplier<Pipe<A>> newPipe, int maxRetries) {
        this.newPipe = newPipe;
        this.maxRetries = maxRetries;
        this.pipe = newPipe.get();
    }

    public void send(A input) {
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
    }

}
