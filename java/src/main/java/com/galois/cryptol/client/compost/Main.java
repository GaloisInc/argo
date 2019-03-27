package com.galois.cryptol.client.compost;

import java.util.Scanner;
import java.util.Arrays;

import com.galois.cryptol.client.Netstring;

class Main {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);
        System.out.println("Initialized...");
        while (input.hasNext()) {
            String line = input.nextLine();
            System.out.println("Input: " + line);
            byte[] encoded = Netstring.encode(line);
            System.out.println("Encoded: " + bytesToString(encoded));
            byte[] decoded = Netstring.decode(encoded).result;
            System.out.println("Decoded: " + bytesToString(decoded));
        }
    }

    private static String bytesToString(byte[] bytes) {
        StringBuilder builder = new StringBuilder();
        for (byte b : bytes) {
            builder.append((char)b);
        }
        return builder.toString();
    }
}
