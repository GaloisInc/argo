package com.galois.cryptol.client;

import java.io.*;
import java.net.*;
import java.util.*;

import com.galois.cryptol.client.Netstring.*;

class NetstringIterator implements Iterator<byte[]> {

    private final InputStream input;    // The stream of concatenated netstrings
    private boolean finished = false;   // Whether we have exhausted the stream
    private byte[] nextResult = null;   // The next result to release, if any

    public NetstringIterator(InputStream input) {
        this.input = input;
    }

    // If and only if hasNext() returns true, nextResult will be non-null
    // following the call, as hasNext() will have populated it
    public synchronized boolean hasNext() {
        if (this.nextResult != null) {
            return true;
        } else if (finished) {
            return false;
        } else {
            try {
                try {
                    this.nextResult = Netstring.decodeFrom(this.input);
                    return true;
                } catch (EOFException e) {
                    this.finished = true;
                    this.input.close();
                    return false;
                }
            } catch (IOException e) {
                this.finished = true;
                return false;
            }
        }
    }

    public synchronized byte[] next() {
        // The conditional here calls hasNext(), which populates nextResult if
        // there is a next thing to return, so in the true branch, we know that
        // nextResult will not be null
        if (this.hasNext()) {
            var result = this.nextResult;
            this.nextResult = null;  // reset to null so next call pulls fresh
            return result;
        } else {
            throw new NoSuchElementException();
        }
    }

}
