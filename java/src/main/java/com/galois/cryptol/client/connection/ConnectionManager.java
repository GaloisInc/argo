package com.galois.cryptol.client.connection;

import java.lang.Runtime.*;
import java.util.*;
import java.io.*;
import java.util.function.*;

public class ConnectionManager<A> implements Supplier<Pipe<A>> {

    private final BiFunction<InputStream, OutputStream, Pipe<A>> newPipe;
    private final ProcessBuilder builder;

    private volatile Process process;
    private volatile InputStream input;
    private volatile OutputStream output;

    public synchronized Pipe<A> get() {

        // Destroy the old process
        try {
            input.close();
            output.flush();
            output.close();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        process.destroy();

        // Create the new process
        try {
            process = builder.start();
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
        input = process.getInputStream();
        output = process.getOutputStream();
        return newPipe.apply(input, output);
    }

    public ConnectionManager(ProcessBuilder builder,
                             BiFunction<InputStream, OutputStream, Pipe<A>> newPipe) {
        this.newPipe = newPipe;
        this.builder = builder;
    }
}
