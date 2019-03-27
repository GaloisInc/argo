package com.galois.cryptol.client;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.io.*;

import com.galois.cryptol.client.InvalidNetstringException;

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

    // Given an iterator of bytes, decode it as a Netstring and return a
    // DecodeResult pairing the resultant Netstring with the remaining bytes
    public static byte[] decode(String string) throws IOException, InvalidNetstringException {
        // Split into bytes
        byte[] bytes = new byte[string.length()];
        for (int i = 0; i < string.length(); i++) {
            bytes[i] = (byte)string.charAt(i);
        }
        // Decode the bytes
        return Netstring.decode(bytes);
    }

    public static byte[] decode(byte[] bytes) throws IOException, InvalidNetstringException {
        // Decode the bytes
        return Netstring.decode(new ByteArrayInputStream(bytes));
    }

    public static byte[] decode(InputStream bytes) throws IOException, InvalidNetstringException {
        // Read digits representing the length of the string until a ':'
        StringBuilder lengthBytes = new StringBuilder();
        while (true) {
            int thisByte = bytes.read();
            if (thisByte == -1) { // end of stream
                throw new EOFException("Malformed netstring, unexpected EOF in length block");
            }
            Character c = (char)thisByte;
            if (Character.isDigit(c)) {
                lengthBytes.append(c);
            } else if (c == ':') {
                // valid separator, signals end of length bytes
                break; // exit loop
            } else {
                throw new InvalidNetstringException("Malformed netstring, missing ':'");
            }
        }

        // Parse the length bytes to determine how long the rest of the
        // netstring will be
        Integer length = Integer.parseInt(lengthBytes.toString());

        // Read length-many bytes of output
        byte[] result = new byte[length];
        for (int j = 0; j < length; j++) {
            int thisByte = bytes.read();
            if (thisByte == -1) { // end of stream
                throw new EOFException("Malformed netstring, unexpected EOF in data block");
            }
            result[j] = (byte)thisByte;
        }

        // Expect a final comma
        int thisByte = bytes.read();
        if (thisByte == -1) { // end of stream
            throw new EOFException("Malformed netstring, unexpected EOF when expecting trailing comma");
        }
        if ((char)thisByte != ',') {
            throw new InvalidNetstringException("Malformed netstring, missing ','");
        }

        // Return the decoded netstring
        return result;
    }
}
