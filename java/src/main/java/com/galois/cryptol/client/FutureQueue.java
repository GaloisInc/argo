package com.galois.cryptol.client;

import java.util.*;
import java.util.concurrent.*;

// Queue returning futures for popped items: if the queue is empty when you pop
// from it, the returned future will be fulfilled by a future insertion
// operation

class FutureQueue<E> {

    // Invariant: if credit is not empty, debt is empty, and vice-versa
    private final Queue<E> credit;
    private final Queue<CompletableFuture<E>> debt;
    private boolean closed = false;

    public FutureQueue() {
        this.credit = new ArrayDeque<E>();
        this.debt   = new ArrayDeque<CompletableFuture<E>>();
    }

    public FutureQueue(Collection<? extends E> c) {
        this.credit = new ArrayDeque<E>(c);
        this.debt   = new ArrayDeque<CompletableFuture<E>>();
    }

    public boolean isEmpty() {
        return this.balance() == 0;
    }

    public boolean isClosed() {
        return this.closed;
    }

    public int balance() {
        return credit.size() - debt.size();
    }

    // Put a value into the queue, either fulfilling a waiting promise, or
    // adding to the list of items yet to be dequeued
    public void put(E e) throws IllegalStateException {
        if (!closed) {
            if (debt.isEmpty()) {
                credit.add(e);
            } else {
                debt.remove().complete(e);
            }
        } else {
            throw new IllegalStateException();
        }
    }

    // Return a future corresponding to the next element of the queue, whether
    // or not that element has already been added to the queue. Once the
    // corresponding put() has been executed, the future will be fulfilled with
    // that value.
    public Future<E> takeFuture() {
        var e = new CompletableFuture<E>();
        if (credit.isEmpty()) {
            if (!closed) {
                debt.add(e);
            } else {
                e.cancel(false);
            }
        } else {
            e.complete(credit.remove());
        }
        return e;
    }

    // Closes the queue, so that future put() operations don't do anything, and
    // cancels all debt futures, including any produced by future calls to
    // takeFuture()
    // This is totally fine to run multiple times
    public boolean close() {
        // Cancel all existing debt, causing all waiting promises to throw
        // exceptions immediately
        for (var f : debt) f.cancel(false);
        this.debt.clear();
        boolean wasClosed = this.isClosed();
        this.closed = true;
        return wasClosed;
    }
}
