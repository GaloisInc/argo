package com.galois.cryptol.client;

import java.util.*;
import java.io.*;
import java.net.*;

import com.eclipsesource.json.*;
import com.galois.cryptol.client.*;

class Main {
    public static void main(String[] args) {
        try {
            InputStream input =
                new Socket("127.0.0.1", 8080).getInputStream();
            Iterator<JsonValue> jsonInputs =
                new JsonByteArrayIterator(new NetstringStreamIterator(input));
            while (jsonInputs.hasNext()) {
                System.out.println(jsonInputs.next());
            }
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }
}
