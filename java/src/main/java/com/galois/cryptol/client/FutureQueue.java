package com.galois.cryptol.client;

import java.util.*;
import java.util.concurrent.*;

// Queue returning futures for popped items: if the queue is empty when you pop
// from it, the returned future will be fulfilled by a future insertion
// operation

class FutureQueue<E> {

    private BlockingQueue<E> credit;
    private BlockingQueue<CompletableFuture<E>> debt;

    public FutureQueue() {
        this.credit = new LinkedBlockingDeque<E>();
        this.debt   = new LinkedBlockingDeque<CompletableFuture<E>>();
    }

    public FutureQueue(Collection<? extends E> c) {
        this.credit = new LinkedBlockingDeque<E>(c);
        this.debt   = new LinkedBlockingDeque<CompletableFuture<E>>();
    }

    public boolean isEmpty() {
        return credit.isEmpty() && debt.isEmpty();
    }

    public void send(E e) {
        if (debt.isEmpty()) {
            credit.add(e);
        } else {
            debt.remove().complete(e);
        }
    }

    public Future<E> request() {
        var e = new CompletableFuture<E>();
        if (credit.isEmpty()) {
            debt.add(e);
            return e;
        } else {
            e.complete(credit.remove());
            return e;
        }
    }
}
