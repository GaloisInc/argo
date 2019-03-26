import java.util.ArrayList;
import java.util.ArrayDeque;
import java.util.Iterator;

public class Netstring {
    private final ArrayList<Byte> contents;

    // Construct a Netstring from an ASCII string
    public Netstring(String wrapped) {
        for (int i = 0; i < wrapped.length(); i++) {
            this.contents.add((byte)wrapped.charAt(i));
        }
    }

    // The result of decoding a netstring: the netstring, and the remainder of
    // the input which has yet to be decoded
    public static class DecodeResult {
        public final Netstring result;
        public final Iterator<Byte> remainder;
        public DecodeResult(Netstring result, ArrayList<Byte> remainder) {
            this.result = result;
            this.remainder = remainder;
        }
    }

    // Given an iterator of bytes, decode it as a Netstring and return a
    // DecodeResult pairing the resultant Netstring with the remaining bytes
    public static DecodeResult decodeFrom(Iterator<Byte> input) {
        Iterator<Byte> bytes = input.iterator();

        // Read digits representing the length of the string until a ':'
        String lengthBytes = new String();
        try {
            while (true) {
                Character c = Character.valueOf(bytes.next());
                if (c.isDigit()) {
                    lengthBytes += c;
                } else if (c == ':') {
                    // valid separator, signals end of length bytes
                    break; // exit loop
                } else {
                    throw new IllegalArgumentException("Malformed netstring, missing ':'");
                }
            }
        } catch (NoSuchElementException e) {
            throw new IllegalArgumentException("Malformed netstring, ran out of intput, missing ':'");
        }
        Integer length = Integer.parseInt(lengthBytes);

        // Read length-many bytes of output
        try {
            ArrayList<Byte> out = new ArrayList<Byte>();
            for (int j = 0; j < length; j++) { // j is unused here
                out.add(bytes.next(i));
            }
            if (bytes.next(i) != ',') {
                throw new IllegalArgumentException("Malformed netstring, missing ','");
            }
        } catch (NoSuchElementException e) {
            throw new IllegalArgumentException("Malformed netstring, ran out of input, missing ','");
        }

        // Return the decoded netstring, and the remaining bytes
        return new DecodeResult(new Netstring(out), bytes);
    }
}
