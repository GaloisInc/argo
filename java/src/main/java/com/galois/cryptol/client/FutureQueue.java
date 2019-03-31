package com.galois.cryptol.client;

import java.util.*;
import java.util.concurrent.*;

// Queue returning futures for popped items: if the queue is empty when you pop
// from it, the returned future will be fulfilled by a future insertion
// operation

class FutureQueue<E> {

    private Queue<E> credit;
    private Queue<CompletableFuture<E>> debt;
    private boolean closed = false;

    public FutureQueue() {
        this.credit = new ArrayDeque<E>();
        this.debt   = new ArrayDeque<CompletableFuture<E>>();
    }

    public FutureQueue(Collection<? extends E> c) {
        this.credit = new ArrayDeque<E>(c);
        this.debt   = new ArrayDeque<CompletableFuture<E>>();
    }

    public synchronized boolean isEmpty() {
        return credit.isEmpty() && debt.isEmpty();
    }

    public synchronized void send(E e) {
        if (!closed) {
            if (debt.isEmpty()) {
                credit.add(e);
            } else {
                debt.remove().complete(e);
            }
        }
    }

    public synchronized Future<E> request() {
        var e = new CompletableFuture<E>();
        if (!closed) {
            if (credit.isEmpty()) {
                debt.add(e);
                return e;
            } else {
                e.complete(credit.remove());
                return e;
            }
        } else {
            e.cancel(false);
            return e;
        }
    }

    public synchronized void shutdown() {
        if (!closed) {
            for (Future<E> f : debt) {
                f.cancel(false);
            }
            debt.clear();
            credit.clear();
            closed = true;
        }
    }

}
