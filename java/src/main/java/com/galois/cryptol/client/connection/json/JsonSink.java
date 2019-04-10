package com.galois.cryptol.client.connection.json;

import java.util.*;
import java.util.function.*;
import java.io.UnsupportedEncodingException;

import com.eclipsesource.json.*;

public class JsonSink implements Consumer<JsonValue> {

    private Consumer<byte[]> withBytes;

    public JsonSink(Consumer<byte[]> withBytes) {
        this.withBytes = withBytes;
    }

    public void accept(JsonValue value) {
        try {
            withBytes.accept(value.toString().getBytes("UTF-8"));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("UTF-8 should always be supported");
        }
    }
}
