package com.galois.cryptol.client;

import java.util.*;
import java.util.function.*;
import java.util.concurrent.*;

import com.galois.cryptol.client.FutureQueue;

class ConcurrentKeyedChannel<C, M> {

    // A mapping from channel name to future queue for messages
    private final Map<C, FutureQueue<M>> channels;

    // Flag determining whether new messages will be accepted; monotonic
    private volatile boolean closed = false;

    public ConcurrentKeyedChannel() {
        channels = new ConcurrentHashMap<C, FutureQueue<M>>();
    }

    public void send(C channelName, M message) throws IllegalStateException {
        channels.compute(channelName, (_k, q) -> {
                if (closed) {
                    throw new IllegalStateException();
                } else {
                    // Otherwise, open up a new channel if there wasn't one
                    q = (q != null) ? q : new FutureQueue<M>();
                    // Send the message on the channel
                    q.put(message);
                    // Determine whether to keep the channel around
                    if (q.isEmpty()) {
                        // If the queue is inert now, remove it
                        // This prevents memory leaks
                        return null;
                    } else {
                        // Otherwise reinsert it
                        return q;
                    }
                }
            });
    }

    public M request(C channelName) throws CancellationException {
        // We'll communicate the queue's response through this side channel
        // The wrapper object hack is necessary to get around the restriction
        // that things touched inside lambdas must be "effectively final"; see:
        // <https://stackoverflow.com/a/30026897/568988>
        var response = new Object() { Future<M> future = null; };

        // Atomically operate over the channel
        channels.compute(channelName,
            (_k, q) -> {
                // Open up a new channel if there wasn't one
                if (q == null) {
                    q = new FutureQueue<M>();
                    // match the closed-ness of everything else
                    // this prevents a race condition during this.close()
                    if (closed) q.close();
                }
                // Get a response future from the channel
                // (and write it out to our side-channel)
                response.future = q.takeFuture();
                // Determine whether to keep the channel around -- removing
                // empty channels prevents memory leaks when they stop being
                // used
                if (q.isEmpty()) {
                    return null;
                } else {
                    return q;
                }
            });

        // Wait on the returned future (may throw a CancellationException)
        try {
            return response.future.get();
        // These cases should be impossible because nothing interrupts any of
        // the futures within the FutureQueue
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        } catch (ExecutionException e) {
            throw new RuntimeException(e);
        }
    }

    // Shuts down all channels: all new input from send() is discarded, and all
    // calls to request() become non-blocking, throwing CancellationExceptions
    // immediately if there is no data on the channel to receive. Blocks until
    // all messages have been removed, either by request() or clear().
    public void close() {
        closed = true; // no new channels will be formed after this
        channels.forEach((_k, q) -> q.close());  // close all existing channels
    }
}
