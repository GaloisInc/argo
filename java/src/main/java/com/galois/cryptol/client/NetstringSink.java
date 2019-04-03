package com.galois.cryptol.client;

import java.util.function.*;
import java.io.*;

import com.galois.cryptol.client.*;

class NetstringSink implements Function<byte[], Boolean> {

    private final OutputStream output;

    public NetstringSink(OutputStream output) {
        this.output = output;
    }

    public synchronized Boolean apply(byte[] bytes) {
        try {
            Netstring.encodeTo(bytes, output);
            return true;
        } catch (IOException e) {
            return false;
        }
    }
}
