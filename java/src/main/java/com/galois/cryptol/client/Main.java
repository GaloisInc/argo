package com.galois.cryptol.client;

import java.util.*;
import java.io.*;
import java.net.*;
import java.util.concurrent.*;
import java.util.function.*;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.*;
import com.galois.cryptol.client.connection.queue.*;

class Main {
    public static void main(String[] args) {
        netJSON();
    }

    public static void netJSON() {
        Scanner in = new Scanner(System.in);
        System.out.print("Cryptol server executable: ");
        var server = in.nextLine();
        System.out.print("Starting directory: ");
        var dir = new File(in.nextLine());

        try(CryptolConnection c = new CryptolConnection(server, dir)) {
            System.out.print("Load module: ");
            c.loadModule(in.nextLine());
            System.out.print("Evaluate: ");
            while (in.hasNextLine()) {
                var line = in.nextLine();
                System.out.println(c.evalExpr(line));
                System.out.print("Evaluate: ");
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
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
