package com.galois.cryptol.client.connection.netstring;

import java.io.*;
import java.net.*;
import java.util.*;

import com.galois.cryptol.client.connection.Pipe;
import com.galois.cryptol.client.connection.netstring.*;

public class NetstringPipe implements Pipe<byte[]> {

    private final InputStream input;
    private final OutputStream output;
    private boolean closed;

    public NetstringPipe(InputStream input, OutputStream output) {
        this.input = input;
        this.output = output;
    }

    public void send(byte[] bytes) {
        try {
            Netstring.encodeTo(bytes, this.output);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public byte[] receive() {
        try {
            return Netstring.decodeFrom(this.input);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    public void close() throws IOException {
        if (!closed) {
            input.close();
            output.close();
            closed = true;
        }
    }

    public boolean isClosed() {
        return closed;
    }
}
