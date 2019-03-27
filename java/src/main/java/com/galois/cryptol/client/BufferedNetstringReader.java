package com.galois.cryptol.client;

import java.io.*;
import java.net.*;
import java.util.*;

import com.galois.cryptol.client.Netstring;

class BufferedNetstringReader implements Iterator<byte[]> {

    private BufferedInputStream input;  // The stream of concatenated netstrings
    private boolean finished = false;   // Whether we have exhausted the stream

    public BufferedNetstringReader(InputStream input) {
        this.input = new BufferedInputStream(input);
    }

    public boolean hasNext() {
        return finished;
    }

    public byte[] next() {
        synchronized(this) {
            if (!this.finished) {
                try {
                    try {
                        return Netstring.decode(input);
                    } catch (EOFException e) {
                        this.finished = true;
                        this.input.close();
                        throw new NoSuchElementException();
                    }
                } catch (IOException e) {
                    this.finished = true;
                    throw new NoSuchElementException();
                }
            } else {
                throw new NoSuchElementException();
            }
        }
    }
}
