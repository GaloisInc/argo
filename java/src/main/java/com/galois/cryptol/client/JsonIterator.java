package com.galois.cryptol.client;

import java.io.*;
import java.net.*;
import java.util.*;

import com.eclipsesource.json.*;

class JsonIterator implements Iterator<JsonValue> {

    // A wrapped byte array iterator, which we'll treat as UTF-8 text
    private Iterator<byte[]> byteArrays;

    public JsonIterator(Iterator<byte[]> byteArrays) {
        this.byteArrays = byteArrays;
    }

    public boolean hasNext() {
        return this.byteArrays.hasNext();
    }

    public JsonValue next() {
        try {
            return Json.parse(new String(byteArrays.next(), "UTF-8"));
        } catch (UnsupportedEncodingException e) {
            // This is impossible because UTF-8 is hard-coded above and
            // according to the Java standard must be supported on all platforms
            throw new RuntimeException("Impossible: UTF-8 unsupported");
        }
    }
}
