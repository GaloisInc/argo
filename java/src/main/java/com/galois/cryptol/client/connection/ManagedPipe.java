package com.galois.cryptol.client.connection;

import java.util.*;
import java.util.function.*;
import java.io.*;
import java.util.concurrent.atomic.*;

public class ManagedPipe<A> implements Pipe<A> {

    private static int defaultMaxRetries = 1;

    private final ConnectionManager<A> newPipe;
    private final Runnable onRetry;
    private final int maxRetries;

    private volatile Pipe<A> pipe;
    private volatile int remainingRetries;
    private volatile int totalRetryCount = 0;

    public ManagedPipe(ConnectionManager<A> newPipe) {
        this(newPipe, () -> { });
    }

    public ManagedPipe(ConnectionManager<A> newPipe, Runnable onRetry) {
        this(newPipe, onRetry, ManagedPipe.defaultMaxRetries);
    }

    public ManagedPipe(ConnectionManager<A> newPipe,
                       Runnable onRetry,
                       int maxRetries) {
        this.newPipe = newPipe;
        this.onRetry = onRetry;
        this.maxRetries = maxRetries;
        this.remainingRetries = maxRetries;
    }

    private void initPipe() throws IOException {
        if (pipe == null) {
            pipe = newPipe.get();
        }
    }

    @Override
    public void send(A input) {
        try {
            initPipe();
            while (true) {
                int retryCount = totalRetryCount;
                try {
                    this.pipe.send(input);
                    remainingRetries = maxRetries;
                    return;
                } catch (Exception e) {
                    // Only re-initialize pipe if nobody else has in the meantime
                    synchronized(this) {
                        if (retryCount == totalRetryCount) {
                            totalRetryCount++;
                            if (remainingRetries > 0) {
                                // System.err.println("\u001B[31m" + "Retrying send after error: " + e + "\u001B[0m");
                                this.pipe = this.newPipe.get();
                                remainingRetries--;
                            } else {
                                throw new ConnectionException(e);
                            }
                        }
                    }
                }
            }
        } catch (IOException e) {
            throw new ConnectionException(e);
        }
    }

    public A receive() {
        try {
            initPipe();
            while (true) {
                int retryCount = totalRetryCount;
                try {
                    A result = this.pipe.receive();
                    remainingRetries = maxRetries;
                    return result;
                } catch (Exception e) {
                    synchronized(this) {
                        if (retryCount == totalRetryCount) {
                            totalRetryCount++;
                            if (remainingRetries > 0) {
                                // System.err.println("\u001B[31m" + "Retrying receive after error: " + e + "\u001B[0m");
                                this.pipe = this.newPipe.get();
                                remainingRetries--;
                            } else {
                                throw new ConnectionException(e);
                            }
                        }
                    }
                }
            }
        } catch (IllegalStateException e) {
            throw new ConnectionException(e);
        } catch (IOException e) {
            throw new ConnectionException(e);
        }
    }

    public void close() throws IOException {
        this.newPipe.close();
        this.pipe.close();
    }

    public boolean isClosed() {
        return pipe != null ? this.pipe.isClosed() : false;
    }
}
