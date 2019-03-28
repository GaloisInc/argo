package com.galois.cryptol.client;

import java.util.*;
import java.util.concurrent.*;

import com.galois.cryptol.client.FutureQueue;

class ConcurrentKeyedChannel<C, M> {

    private ConcurrentHashMap<C, FutureQueue<M>> channels;

    public void send(C channelName, M message) {
        channels.compute(channelName,
            (_k, channel) -> {
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

    public Future<M> request(C channelName) {
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
                // Determine whether to keep the channel around
                if (channel.isEmpty()) {
                    return null;
                } else {
                    return channel;
                }
            });

        return wrapper.response;
    }
}
