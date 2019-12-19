package com.galois.cryptol.client;

public class CryptolException extends Exception {
    final String message;
    public CryptolException(String message) {
        this.message = message;
    }
    @Override
    public String toString() {
        return this.message;
    }
}
