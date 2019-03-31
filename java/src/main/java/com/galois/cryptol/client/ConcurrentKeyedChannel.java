package com.galois.cryptol.client;

import java.util.*;
import java.util.function.*;
import java.util.concurrent.*;

import com.galois.cryptol.client.FutureQueue;

class ConcurrentKeyedChannel<C, M> {

    private Map<C, FutureQueue<M>> channels;
    private Boolean closed = false;

    public ConcurrentKeyedChannel() {
        this.channels = new ConcurrentHashMap<C, FutureQueue<M>>();
    }

    public void send(C channelName, M message) {
        channels.compute(channelName, (_k, channel) -> {
                if (channel == null) {
                    // Open up a new channel if there wasn't one
                    channel = new FutureQueue<M>();
                }
                // Send the message on the channel
                channel.send(message);
                // Determine whether to keep the channel around
                if (channel.isEmpty()) {
                    // If the queue is inert now, remove it
                    // This prevents memory leaks
                    return null;
                } else {
                    // Otherwise reinsert it
                    return channel;
                }
            });
    }

    public M request(C channelName)
        throws InterruptedException, ExecutionException {
        // If the channels have been shut down, throw an exception (same
        // behavior as if we got a future, then it got cancelled by a subsequent
        // shutdown before being completed, so things are consistent regardless
        // of ordering)
        synchronized(closed) {
            if (closed) {
                throw new CancellationException();
            }
        }

        // We'll communicate the response through this side channel
        // The wrapper object hack is necessary to get around the restriction
        // that things touched inside lambdas must be "effectively final"; see:
        // <https://stackoverflow.com/a/30026897/568988>
        var wrapper = new Object() { Future<M> response = null; };

        // Atomically operate over the channel
        channels.compute(channelName,
            (_k, channel) -> {
                if (channel == null) {
                    // Open up a new channel if there wasn't one
                    channel = new FutureQueue<M>();
                }
                // Get a response future from the channel
                // (and write it out to our side-channel)
                wrapper.response = channel.request();
                // Determine whether to keep the channel around -- removing
                // empty channels prevents memory leaks when they stop being
                // used
                if (channel.isEmpty()) {
                    return null;
                } else {
                    return channel;
                }
            });

        return wrapper.response.get();
    }

    public void shutdown() {
        synchronized(closed) {
            if (!closed) {
                closed = true;
                // Close things up and let everything quiesce
                while (!channels.isEmpty()) {
                    channels.forEach((_k, channel) -> channel.shutdown());
                    channels.clear();
                }
            }
        }
    }
}
