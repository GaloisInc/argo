package com.galois.cryptol.client;

import java.io.*;
import java.net.*;
import java.util.*;

import com.eclipsesource.json.*;

import com.galois.cryptol.client.Netstring;
import com.galois.cryptol.client.InvalidNetstringException;

class JsonNetstringReader {

    private InputStream input;  // The input byte stream
    private boolean finished = false;

    public synchronized void close() throws IOException {
        if (!finished) {
            finished = true;
            input.close();
        }
    }

    public JsonValue next()
        throws IOException, EOFException, ParseException, InvalidNetstringException {
        byte[] bytes;
        synchronized(this) {
            if (this.finished) {
                throw new EOFException("End of JSON input stream");
            }
            try {
                bytes = Netstring.decode(input);
            } catch (EOFException e) {
                this.close();
                throw e;
            }
        }
        return Json.parse(Arrays.toString(bytes));
    }
}
