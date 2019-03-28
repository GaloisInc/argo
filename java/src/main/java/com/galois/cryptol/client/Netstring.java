package com.galois.cryptol.client;

import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.io.*;

import com.galois.cryptol.client.InvalidNetstringException;

public class Netstring {

    // Encode a byte array in netstring format and output to a stream
    public static void encodeTo(byte[] bytes, OutputStream output)
        throws IOException {
        // Calculate the array of length bytes
        String lengthString = (Integer.valueOf(bytes.length)).toString();
        ArrayList<Byte> lengthBytes = new ArrayList<Byte>();
        for (int j = 0; j < lengthString.length(); j++) {
            lengthBytes.add((byte)lengthString.charAt(j));
        }

        for (byte b : lengthBytes) {
            output.write(b);
        }
        output.write((byte)':');
        for (byte b : bytes) {
            output.write(b);
        }
        output.write((byte)',');
    }

    // Given an input stream of bytes, decode one netstring from it, and return
    // the resultant array of bytes
    public static byte[] decodeFrom(InputStream bytes)
        throws IOException {
        synchronized(bytes) {
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
}
