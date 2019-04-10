package com.galois.cryptol.client.connection.netstring;

import java.util.function.*;
import java.io.*;

import com.galois.cryptol.client.connection.netstring.*;

public class NetstringSink implements Consumer<byte[]> {

    private final OutputStream output;

    public NetstringSink(OutputStream output) {
        this.output = output;
    }

    public static class NetstringSinkException extends RuntimeException {
        public static final long serialVersionUID = 0;
        public NetstringSinkException(Throwable e) { super(e); }
    }

    public synchronized void accept(byte[] bytes) {
        try {
            Netstring.encodeTo(bytes, output);
        } catch (IOException e) {
            throw new NetstringSinkException(e);
        }
    }
}
