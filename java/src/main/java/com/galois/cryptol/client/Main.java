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
        if (args.length == 2) {
            String cryptolServer = args[0];
            String startingDir = args[1];
            cryptolEval(cryptolServer, startingDir);
        } else {
            var error = "Please specify a server executable and starting directory";
            System.err.println(error);
            System.exit(1);
        }
    }

    public static void cryptolEval(String server, String dir) {
        try(CryptolConnection c = new CryptolConnection(server, new File(dir))) {
            Scanner in = new Scanner(System.in);

            boolean loaded = false;
            do {
                System.out.print("Load module: ");
                try {
                    c.loadModule(in.nextLine());
                    loaded = true;
                } catch (CryptolException e) {
                    System.out.println(e);
                }
            } while (!loaded);

            System.out.print("Evaluate: ");
            while (in.hasNextLine()) {
                var line = in.nextLine();
                try {
                    System.out.println(c.evalExpr(line));
                } catch (CryptolException e) {
                    System.out.println(e);
                }
                System.out.print("Evaluate: ");
            }
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

}
