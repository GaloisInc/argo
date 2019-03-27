package com.galois.cryptol.client;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class Netstring {

    // Construct a Netstring from an ASCII string
    // NOTE: This will not work properly for non-ASCII strings!
    public static byte[] encode(String string) {
        // Split into bytes
        byte[] contents = new byte[string.length()];
        for (int i = 0; i < string.length(); i++) {
            contents[i] = (byte)string.charAt(i);
        }
        // And then encode them
        return Netstring.encode(contents);
    }

    // Encode a byte array in netstring format
    public static byte[] encode(byte[] input) {
        // Calculate the array of length bytes
        String lengthString = (Integer.valueOf(input.length)).toString();
        ArrayList<Byte> lengthBytes = new ArrayList<Byte>();
        for (int j = 0; j < lengthString.length(); j++) {
            lengthBytes.add((byte)lengthString.charAt(j));
        }

        // Write out the result to an array
        byte[] result = new byte[lengthBytes.size() + input.length + 2];
        int i = 0;
        for (byte b : lengthBytes) {
            result[i] = b;
            i += 1;
        }
        result[i] = (byte)':';
        i += 1;
        for (byte b : input) {
            result[i] = b;
            i += 1;
        }
        result[i] = (byte)',';
        i += 1;

        // We should now have initialized the entire array
        assert i == result.length;
        return result;
    }

    // The result of decoding a netstring: the netstring, and the remainder of
    // the input which has yet to be decoded
    public static class DecodeResult {
        public final byte[] result;
        public final Iterator<Byte> remainder;
        public DecodeResult(byte[] result, Iterator<Byte> remainder) {
            this.result = result;
            this.remainder = remainder;
        }
    }

    // Given an iterator of bytes, decode it as a Netstring and return a
    // DecodeResult pairing the resultant Netstring with the remaining bytes
    public static DecodeResult decode(String string) {
        // Split into bytes
        ArrayList<Byte> bytes = new ArrayList<Byte>(string.length());
        for (int i = 0; i < string.length(); i++) {
            bytes.add((byte)string.charAt(i));
        }
        // Decode the bytes
        return Netstring.decode(bytes);
    }

    public static DecodeResult decode(byte[] array) {
        // Split into bytes
        ArrayList<Byte> bytes = new ArrayList<Byte>(array.length);
        for (int i = 0; i < array.length; i++) {
            bytes.add(array[i]);
        }

        // Decode the bytes
        return Netstring.decode(bytes);
    }

    public static DecodeResult decode(Iterable<Byte> bytes) {
        // Convert to an iterator and decode that
        return Netstring.decode(bytes.iterator());
    }

    public static DecodeResult decode(Iterator<Byte> bytes) {
        // Read digits representing the length of the string until a ':'
        StringBuilder lengthBytes = new StringBuilder();
        try {
            while (true) {
                Character c = (char)(byte)bytes.next();
                if (Character.isDigit(c)) {
                    lengthBytes.append(c);
                } else if (c == ':') {
                    // valid separator, signals end of length bytes
                    break; // exit loop
                } else {
                    throw new IllegalArgumentException("Malformed netstring, missing ':'");
                }
            }
        } catch (NoSuchElementException e) {
            throw new IllegalArgumentException("Malformed netstring, ran out of input, missing ':'");
        }

        // Parse the length bytes to determine how long the rest of the
        // netstring will be
        Integer length = Integer.parseInt(lengthBytes.toString());

        // Read length-many bytes of output
        byte[] result = new byte[length];
        try {
            for (int j = 0; j < length; j++) {
                result[j] = bytes.next();
            }
            if ((char)(byte)bytes.next() != ',') {
                throw new IllegalArgumentException("Malformed netstring, missing ','");
            }
        } catch (NoSuchElementException e) {
            throw new IllegalArgumentException("Malformed netstring, ran out of input, missing ','");
        }

        // Return the decoded netstring, and the remaining bytes
        return new DecodeResult(result, bytes);
    }
}
