package com.galois.cryptol.client;

import java.util.*;
import java.util.function.*;
import java.io.*;

import com.eclipsesource.json.*;

import com.galois.cryptol.client.JsonConnection.*;

class CryptolConnection {

    private final StatefulJsonConnection connection;
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
            new StatefulJsonConnection(requests, responses, logAndQuit);
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

    // The available methods:

    public void loadModule(String file) throws IOException {
        call(new Call<Void, FileNotFoundException>() {
                public String method() { return "load module"; }
                public JsonValue params() {
                    return Json.object().add("file", file);
                }
                public Void decodeResult(JsonValue v) {
                    return null;
                }
                public Void handleException(JsonRpcException e)
                    throws FileNotFoundException, UnexpectedRpcException {
                    if (e.code == 4) {
                        // TODO: improve this parsing, provide unified cryptol
                        // error message handler shared across all calls?
                        throw new FileNotFoundException(e.data.toString());
                    } else {
                        throw new UnexpectedRpcException();
                    }
                }
            });
    }
}