package com.galois.cryptol.client;

import java.util.Iterator;

class IDSource implements Iterator<Integer> {

    // The current fresh identifier
    private Integer fresh = 0;

    // Initialize
    public IDSource() { }

    // There is always a next identifier
    public boolean hasNext() {
        return true;
    }

    // We make sure it's thread safe by synchronizing on increment
    public Integer next() {
        synchronized(this) {
            return fresh++;
        }
    }
}
