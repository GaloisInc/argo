package com.galois.cryptol.client.connection.json;

import java.util.*;
import java.util.function.*;
import java.io.*;

import com.eclipsesource.json.*;

import com.galois.cryptol.client.connection.Pipe;

public class JsonPipe implements Pipe<JsonValue> {

    private final Pipe<byte[]> bytePipe;

    public JsonPipe(Pipe<byte[]> bytePipe) {
        this.bytePipe = bytePipe;
    }

    // The "impossible" exceptions below really should be, because UTF-8 is
    // hard-coded, and according to the Java standard must be supported on all
    // platforms

    public void send(JsonValue value) {
        try {
            bytePipe.send(value.toString().getBytes("UTF-8"));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("Impossible: UTF-8 unsupported");
        }
    }

    public JsonValue receive() {
        try {
            return Json.parse(new String(bytePipe.receive(), "UTF-8"));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("Impossible: UTF-8 unsupported");
        }
    }

    public void close() throws IOException {
        this.bytePipe.close();
    }

    public boolean isClosed() {
        return this.bytePipe.isClosed();
    }
}
