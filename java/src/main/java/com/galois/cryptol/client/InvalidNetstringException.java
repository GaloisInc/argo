package com.galois.cryptol.client;

class InvalidNetstringException extends RuntimeException {
    static final long serialVersionUID = 0;
    InvalidNetstringException() {
        super();
    }
    InvalidNetstringException(String s) {
        super(s);
    }
}
