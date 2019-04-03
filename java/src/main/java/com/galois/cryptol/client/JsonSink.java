package com.galois.cryptol.client;

import java.util.*;
import java.util.function.*;
import java.io.UnsupportedEncodingException;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.*;

class JsonSink implements Function<JsonValue, Boolean> {

    private Function<byte[], Boolean> withBytes;

    public JsonSink(Function<byte[], Boolean> withBytes) {
        this.withBytes = withBytes;
    }

    public Boolean apply(JsonValue value) {
        try {
            return withBytes.apply(value.toString().getBytes("UTF-8"));
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("UTF-8 should always be supported");
        }
    }
}
