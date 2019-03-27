package com.galois.cryptol.client;

class InvalidNetstringException extends Exception{
    static final long serialVersionUID = 0;
    InvalidNetstringException() {
        super();
    }
    InvalidNetstringException(String s) {
        super(s);
    }
}
