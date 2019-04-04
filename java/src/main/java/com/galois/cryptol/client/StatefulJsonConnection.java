package com.galois.cryptol.client;

import com.eclipsesource.json.*;
import java.util.function.*;
import java.util.*;

class StatefulJsonConnection extends JsonConnection {

    private JsonValue state = null;

    //TODO: need to be able to copy state, support concurrent access to
    //connection, so it can't be an extension, needs to be a wrapper

    public static class State {
        private final JsonValue state;
        private State(JsonValue state) { this.state = new JsonValue(state); }
    }

    public StatefulJsonConnection(
                Consumer<JsonValue> requests,
                Iterator<JsonValue> responses,
                Consumer<JsonRpcException> handleUnidentified,
                Consumer<InvalidRpcResponseException> handleBadResponse,
                Consumer<Exception> handleOtherException) {
        super(requests, responses,
              handleUnidentified,
              handleBadResponse,
              handleOtherException);
    }

    public State getState() { return new State(state); }

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
                if (state != null) {
                    notificationParams.asObject().set("state", state);
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
                if (state != null) {
                    callParams.asObject().set("state", state);
                }
                return callParams;
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }

        public O decodeResult(JsonValue o) throws UnexpectedRpcResultException {
            try {
                JsonObject callResult = o.asObject();
                state = callResult.get("state");  // set the outer state!
                callResult.remove("state");
                return call.decodeResult(callResult);
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }

        public O handleException(JsonRpcException e)
            throws E, UnexpectedRpcException {
            return call.handleException(e);
        }
    }
}
