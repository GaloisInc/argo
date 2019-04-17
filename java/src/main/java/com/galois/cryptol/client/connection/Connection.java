package com.galois.cryptol.client.connection;

import com.eclipsesource.json.*;
import java.util.function.*;
import java.util.*;

import com.galois.cryptol.client.connection.*;
import com.galois.cryptol.client.connection.json.*;

public class Connection extends JsonConnection {

    private volatile JsonValue currentState;
    private final Pipe<JsonValue> pipe;
    private final Function<Exception, Boolean> handleException;

    public Connection(Pipe<JsonValue> pipe,
                      Function<Exception, Boolean> handleException) {
        super(pipe, handleException);
        this.pipe = pipe;
        this.handleException = handleException;
        this.currentState = null;
    }

    public Connection copy() {
        var c = new Connection(this.pipe, this.handleException);
        c.currentState = this.currentState;
        return c;
    }

    public <O, E extends Exception> O call(Call<O, E> call)
        throws E, ConnectionException {
        return super.call(new StatefulCall<O, E>(call));
    }

    public void notify(Notification notification)
        throws ConnectionException {
        super.notify(new StatefulNotification(notification.method(),
                                              notification.params()));
    }

    private class StatefulNotification extends Notification {

        public StatefulNotification(String method, JsonValue params) {
            super(method, params);
        }

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
            super(new StatefulNotification(call.method(), call.params()),
                  call.decoder, call.handler);
        }

        // And then we further override the decode() behavior to set the state
        public O decode(JsonValue o) {
            try {
                JsonObject callResult = o.asObject();
                synchronized(Connection.this) {
                    var newState = callResult.get("state");
                    // Update the current state if there has been an update
                    currentState = newState != null ? newState : currentState;
                }
                JsonValue answer = callResult.get("answer");
                if (answer != null) {
                    return super.decode(answer);
                } else {
                    throw new IllegalArgumentException("No answer field in stateful result");
                }
            } catch (UnsupportedOperationException e) {
                throw new IllegalArgumentException("Stateful call params not an object", e);
            }
        }
    }
}
