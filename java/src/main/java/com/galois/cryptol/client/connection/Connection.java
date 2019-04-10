package com.galois.cryptol.client.connection;

import com.eclipsesource.json.*;
import java.util.function.*;
import java.util.*;

import com.galois.cryptol.client.connection.*;
import com.galois.cryptol.client.connection.json.*;

public class Connection extends JsonConnection {

    private volatile JsonValue currentState;

    public class State {
        private final JsonValue s;
        private State() { this.s = currentState; }
    }

    public State getState() { return new State(); }

    public void setState(State state) { currentState = state.s; }

    public Connection(
                Consumer<JsonValue> requests,
                Iterator<JsonValue> responses,
                Function<Exception, Boolean> handleException) {
        super(requests, responses, handleException);
        this.currentState = null;
    }

    // This is a partial cloning constructor: the local states may freely
    // diverge, but the underlying server connection is the same
    public Connection(Connection connection) {
        super(connection);
        this.currentState = connection.currentState;
    }

    @Override
    public <O, E extends Exception> O call(String method,
                                           JsonValue params,
                                           Function<JsonValue, O> decode,
                                           Function<JsonRpcException, E> handle)
        throws E, ConnectionException {
        Call<O, E> call = new Call<O, E>(method, params, decode, handle);
        return this.call(new StatefulCall<O, E>(call));
    }

    @Override
    public <O, E extends Exception> O call(Call<O, E> call)
        throws E, ConnectionException {
        return super.call(new StatefulCall<O, E>(call));
    }

    @Override
    public void notify(String method, JsonValue params)
        throws ConnectionException {
        this.notify(new StatefulNotification(new Notification(method, params)));
    }


    @Override
    public void notify(Notification notification)
        throws ConnectionException {
        super.notify(new StatefulNotification(notification));
    }

    private class StatefulNotification extends Notification {

        public StatefulNotification(Notification notification) {
            super(notification.method(), notification.params());
        }

        @Override
        public JsonValue params() {
            try {
                JsonObject params = super.params().asObject();
                if (currentState != null) {
                    params.add("state", currentState);
                }
                return Json.object().merge(params);
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }
    }

    private class StatefulCall<O, E extends Exception> extends Call<O, E> {

        public StatefulCall(Call<O, E> call) {
            // We inherit the special params() behavior from StatefulNotification
            super(new StatefulNotification(new Notification(call.method(), call.params())),
                  call.decoder, call.handler);
        }

        // And then we further override the decode() behavior to set the state
        @Override
        public O decode(JsonValue o) {
            try {
                JsonObject callResult = o.asObject();
                synchronized(Connection.this) {
                    currentState = callResult.get("state");  // sets the outer state!
                }
                callResult.remove("state");
                return super.decode(callResult);
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }
    }
}
