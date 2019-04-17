package com.galois.cryptol.client.connection;

import java.lang.Runtime.*;
import java.util.*;
import java.io.*;
import java.util.function.*;

public class ConnectionManager<A> implements Supplier<Pipe<A>> {

    private final PipeFactory<A> newPipe;
    private final ProcessBuilder builder;

    private volatile Process process;

    public static interface PipeFactory<A> {
        public Pipe<A> make(OutputStream in, InputStream out, InputStream err);
    }

    public Pipe<A> get() {

        // Destroy the old process
        if (process != null) {
            process.destroy();
        }

        // Create the new process
        try {
            process = builder.start();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        var in  = process.getOutputStream();
        var out = process.getInputStream();
        var err = process.getErrorStream();
        return newPipe.make(in, out, err);
    }

    public ConnectionManager(ProcessBuilder builder, PipeFactory<A> newPipe) {
        this.newPipe = newPipe;
        this.builder = builder;
    }

    public void stop() {
        if (process != null) {
            process.destroy();
        }
        process = null;
    }
}
