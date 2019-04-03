package com.galois.cryptol.client;

import java.util.*;
import java.io.*;
import java.net.*;
import java.util.concurrent.*;
import java.util.function.*;
import com.galois.cryptol.client.JsonConnection.JsonRpcRequestException;
import com.galois.cryptol.client.JsonConnection.JsonRpcResponseException;
import com.galois.cryptol.client.JsonConnection.JsonRpcConnectionException;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.*;

class Main {
    public static void main(String[] args) {
        netJSON();
    }

    // Connect as a client to localhost:8080 and receive netstring-encoded JSON
    // objects until the server closes, printing each to stdout
    public static void netJSON() {
        try {
            // Acquire TCP streams from server
            Socket socket;
            while (true) {
                try {
                    socket = new Socket("127.0.0.1", 8080);
                    break;
                } catch (ConnectException e) {
                    try {
                        Thread.sleep(500);
                    } catch (InterruptedException f) {
                        throw new IOException(f);
                    }
                }
            }
            InputStream input = socket.getInputStream();
            OutputStream output = socket.getOutputStream();
            // Wrap these streams in a JsonConnection
            Iterator<JsonValue> responses =
                new JsonIterator(new NetstringIterator(input));
            Consumer<JsonValue> requests =
                new JsonSink(new NetstringSink(output));
            var connection = new JsonConnection(requests, responses);
            // Interact
            var userInput = new Scanner(System.in);
            while (true) {
                try {
                    System.out.print("Method name: ");
                    String method = userInput.nextLine();
                    System.out.print("Parameters (JSON object): ");
                        JsonObject params = Json.parse(userInput.nextLine()).asObject();
                    try {
                        System.out.println(connection.call(method, params));
                    } catch (JsonRpcException e) {
                        System.out.println("Error " + e.code + ": " + e.message +
                                        "\nError data: " + e.data);
                    } catch (JsonRpcConnectionException e) {
                        System.out.println("Server disconnected: " + e);
                        break;
                    } catch (JsonRpcRequestException e) {
                        System.out.println("Couldn't send to server: " + e);
                        break;
                    } catch (JsonRpcResponseException e) {
                        System.out.println("Server returned invalid response: " + e);
                        break;
                    }
                } catch (UnsupportedOperationException e) {
                    System.out.println("Not a JSON object!");
                } catch (ParseException e) {
                    System.out.println("Invalid JSON!");
                }
            }
        } catch (IOException e) {
            System.out.println("Error in TCP server connection: " + e);
        }
    }

    // Visual demo of concurrent keyed channel: run a receiving and a sending
    // thread per channel, each with random delay between send() and request()
    // calls, displaying the method calls in a table. The simulation lasts for
    // the timeout parameter, in seconds
    public static void multiQueueDemo(int channelCount, double meanDelay, int timeout) {
        // Channels
        var channels = new ConcurrentMultiQueue<Integer, Integer>();

        // Sending threads
        var sending = new ArrayList<Runnable>();
        for (int c = 0; c < channelCount; c++) {
            int channel = c;
            sending.add(() -> {
                    int message = 0;
                    while (true) {
                        long wait = (long)(2 * 1000 * meanDelay * Math.random());
                        try {
                            TimeUnit.MILLISECONDS.sleep(wait);
                        } catch (InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                        try {
                            channels.send(channel, message);
                            synchronized(System.out) {
                                for (int i = 0; i < channel; i++) System.out.print("\t\t\t\t");
                                System.out.println(channel + ": SEND " + message);
                            }
                            message++;
                        } catch (QueueClosedException e) {
                            break;
                        }
                    }
                    synchronized(System.out) {
                        for (int i = 0; i < channel; i++) System.out.print("\t\t\t\t");
                        System.out.println(channel + ": STOPPED");
                    }
                });
        }

        // Receiving threads
        var receiving = new ArrayList<Runnable>();
        for (int c = 0; c < channelCount; c++) {
            int channel = c;
            receiving.add(() -> {
                    while (true) {
                        long wait = (long)(2 * 1000 * meanDelay * Math.random());
                        try {
                            TimeUnit.MILLISECONDS.sleep(wait);
                        } catch (InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                        synchronized(System.out) {
                            for (int i = 0; i < channel; i++) System.out.print("\t\t\t\t");
                            System.out.println("\t\t" + channel + ": REQUEST ");
                        }
                        try {
                            var message = channels.request(channel);
                            synchronized(System.out) {
                                for (int i = 0; i < channel; i++) System.out.print("\t\t\t\t");
                                System.out.println("\t\t" + channel + ": RECEIVE " + message);
                            }
                        } catch (QueueClosedException e) {
                            break;
                        }
                    }
                    synchronized(System.out) {
                        for (int i = 0; i < channel; i++) System.out.print("\t\t\t\t");
                        System.out.println("\t\t" + channel + ": CANCELLED ");
                    }
                });
        }

        // Start all threads
        System.out.println();
        for (var f : sending)   (new Thread(f)).start();
        for (var f : receiving) (new Thread(f)).start();

        (new Thread(() -> {
                try {
                    TimeUnit.SECONDS.sleep(timeout);
                    synchronized(System.out) {
                        channels.close();
                        for (int i = 0; i < channelCount; i++) {
                            System.out.print("--------------------------------");
                        }
                        System.out.println();
                    }
                    channels.close();
                    synchronized(System.out) {
                        channels.close();
                        for (int i = 0; i < channelCount; i++) {
                            System.out.print("--------------------------------");
                        }
                        System.out.println();
                    }
                } catch (InterruptedException e) {
                }
        })).start();
    }
}
