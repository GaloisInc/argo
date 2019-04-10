package com.galois.cryptol.client.connection;

import com.eclipsesource.json.*;
import java.util.function.*;
import java.util.*;

import com.galois.cryptol.client.connection.*;
import com.galois.cryptol.client.connection.json.*;

public class Connection extends JsonConnection {

    private JsonValue currentState;

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

    public Connection(Connection connection) {
        super(connection);
        this.currentState = connection.currentState;
    }

    @Override
    public <O, E extends Exception> O call(Call<O, E> call)
        throws E, ConnectionException {
        return super.call(new StatefulCall<O, E>(call));
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
                JsonValue params = super.params();
                if (currentState != null) {
                    params.asObject().set("state", currentState);
                }
                return params;
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }
    }

    private class StatefulCall<O, E extends Exception> extends Call<O, E> {

        public StatefulCall(Call<O, E> call) {
            super(call.method(), call.params(), call.decoder, call.handler);
        }

        @Override
        public O decode(JsonValue o) {
            try {
                JsonObject callResult = o.asObject();
                currentState = callResult.get("state");  // sets the outer state!
                callResult.remove("state");
                return super.decode(callResult);
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }
    }
}
