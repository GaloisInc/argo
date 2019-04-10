package com.galois.cryptol.client;

import java.util.*;
import java.util.function.*;
import java.io.*;

import com.eclipsesource.json.*;

import com.galois.cryptol.client.connection.*;
import com.galois.cryptol.client.connection.json.*;
import com.galois.cryptol.client.connection.netstring.*;

public class CryptolConnection {

    private final Connection connection;
    private final OutputStream output;
    private final InputStream input;

    private volatile boolean closed = false;

    public CryptolConnection(OutputStream output, InputStream input) {
        this.output = output;
        this.input = input;
        // Set up source and sink for connection
        Iterator<JsonValue> responses =
            new JsonIterator(new NetstringIterator(input));
        Consumer<JsonValue> requests =
            new JsonSink(new NetstringSink(output));
        // Set up exception handling to interrupt this thread
        Thread thisThread = Thread.currentThread();
        Function<Exception, Boolean> logAndQuit =
            e -> { System.err.println(e); return false; };
        // Initialize the connection
        connection =
            new Connection(requests, responses, logAndQuit);
    }

    // Close the connection
    public synchronized void close() throws IOException {
        if (closed == false) {
            output.close();
            input.close();
            connection.close();
            closed = true;
        }
    }

    // Since this runs on actual output/input streams, we know that connection
    // exceptions in this case are really IOExceptions, so we use this wrapper
    // to allow the caller to not need to see ConnectionExceptions
    private <O, E extends Exception> O call(Call<O, E> call)
        throws E, IOException {
        try {
            return connection.call(call);
        } catch (ConnectionException e) {
            throw new IOException(e);
        }
    }

    // How to make a new call:

    private abstract class CryptolCall<O> implements Call<O, IOException> {

        private String method;     // <--- fill me in!
        private JsonValue params;  // <--- fill me in!

        public abstract O decode(JsonValue v);  // <--- implement me!

        @Override public String method() { return method; }
        @Override public JsonValue params() { return params; }
        @Override
        public IOException handle(JsonRpcException e) {
            // FIXME
            return null;
        }
    }

    // The calls available:

    public void loadModule(String file) throws IOException {
        call(new CryptolCall<Unit>() {
                String method = "load module";
                JsonValue params =
                    Json.object().add("file", file);
                public Unit decode(JsonValue v) {
                    return new Unit();
                }
            });
    }
}
