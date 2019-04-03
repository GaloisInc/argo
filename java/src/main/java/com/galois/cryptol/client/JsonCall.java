package com.galois.cryptol.client;

import java.util.*;
import java.io.*;
import java.net.*;
import java.util.concurrent.*;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.*;

class JsonCall {

    public final String method;      // The method name
    public final JsonObject params;  // The parameters

    public JsonCall(String method, JsonObject params) {
        this.method = method;
        this.params = params;
    }

    public int invoke(IDSource ids, OutputStream out) throws IOException {
        var id = ids.next();
        JsonValue message = Json.object()
            .add("jsonrpc", 2.0)
            .add("id", id)
            .add("method", method)
            .add("params", params);
        try {
            Netstring.encodeTo(message.toString().getBytes("UTF-8"), out);
            out.flush();
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException("UTF-8 should always be supported");
        }
        return id;
    }
}
