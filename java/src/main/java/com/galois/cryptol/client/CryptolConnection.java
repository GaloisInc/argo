package com.galois.cryptol.client;

import java.util.*;
import java.util.function.*;
import java.io.*;
import java.net.*;
import com.galois.cryptol.client.connection.ConnectionManager;

import com.eclipsesource.json.*;

import com.galois.cryptol.client.connection.*;
import com.galois.cryptol.client.connection.json.*;
import com.galois.cryptol.client.connection.netstring.*;

public class CryptolConnection implements AutoCloseable {

    private final Connection connection;

    private static void forLinesAsync(InputStream i, Consumer<String> c) {
        (new Thread(() -> {
                try {
                    (new BufferedReader(new InputStreamReader(i)))
                        .lines().forEach(c);
                } catch (Exception e) {
                    // Do nothing; the stream is gone, for some reason
                }
        })).start();
    }

    public CryptolConnection(String server, File dir) throws IOException {
        this.connection =
            new Connection(
                new ProcessBuilder(server, "--dynamic4").directory(dir),
                (_in, out, err) -> {
                    // The process will tell us what port to connect to...
                    int port = (new Scanner(out)).skip("PORT ").nextInt();
                    // Consume the remaining output and error
                    forLinesAsync(out, System.out::println);
                    forLinesAsync(err, System.err::println);
                    // Connect to the port
                    var s = new Socket("127.0.0.1", port);
                    var socketIn  = s.getInputStream();
                    var socketOut = s.getOutputStream();
                    // Make a JSON-netstring layer across the connection
                    return new JsonPipe(new NetstringPipe(socketIn, socketOut));
                },
                e -> {
                    System.err.println("Connection error:");
                    e.printStackTrace();
                });
    }

    // Close the connection
    public synchronized void close() throws IOException {
        connection.close();
    }

    // Since this runs on actual output/input streams, we know that connection
    // exceptions in this case are really IOExceptions, so we use this wrapper
    // to allow the caller to not need to see ConnectionExceptions
    private <O> O call(String method, JsonValue params,
                       Function<JsonValue, O> decode)
        throws IOException {
        Call<O, IOException> call =
            new Call<>(method, params, decode, e -> {
                    // handle Cryptol exceptions
                    return null; // FIXME, return structured Cryptol exceptions
            });
        return connection.call(call);
    }

    // The calls available:

    public void loadModule(String file) throws IOException {
        call("load module",
             Json.object().add("file", file),
             v -> new Unit());
    }

    public String evalExpr(String expr) throws IOException {
        return call("evaluate expression",
                    Json.object().add("expression", expr),
                    v -> v.get("value").toString());
    }
}
