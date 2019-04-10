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

    public <O, E extends Exception> O call(Call<O, E> call)
        throws E, ConnectionException {
        return super.call(new StatefulCall<O, E>(call));
    }

    public void notify(Notification notification)
        throws ConnectionException {
        super.notify(new StatefulNotification(notification));
    }

    private class StatefulNotification implements Notification {

        private Notification notification;

        public StatefulNotification(Notification notification) {
            this.notification = notification;
        }

        public String method() { return notification.method(); }

        public JsonValue params() {
            try {
                JsonValue notificationParams = notification.params();
                if (currentState != null) {
                    notificationParams.asObject().set("state", currentState);
                }
                return notificationParams;
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }
    }

    private class StatefulCall<O, E extends Exception> implements Call<O, E> {

        private Call<O, E> call;

        public StatefulCall(Call<O, E> call) { this.call = call; }

        public String method() { return call.method(); }

        public JsonValue params() {
            try {
                JsonValue callParams = call.params();
                if (currentState != null) {
                    callParams.asObject().set("state", currentState);
                }
                return callParams;
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }

        public O decode(JsonValue o) {
            try {
                JsonObject callResult = o.asObject();
                currentState = callResult.get("state");  // sets the outer state!
                callResult.remove("state");
                return call.decode(callResult);
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }

        public E handle(JsonRpcException e) {
            return call.handle(e);
        }
    }
}
