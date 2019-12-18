import http.server
import http.client
import socketserver
import socket
import random
import functools
import tempfile
import os
import signal
import types
import time
import hashlib
import math
from typing import Optional, Any, Type, Callable

DEFAULT_PORT = 27182
MAX_RETRY_ATTEMPTS = 3
DEFAULT_LOCATION_FILENAME = '.__LOCATION__'
DEFAULT_SERVER_TIMEOUT = 10

class TempDirHandler(http.server.SimpleHTTPRequestHandler):
    # Suppress logging output
    def log_message(*args : Any, **kwargs : Any) -> Any: pass

# Serving files from an arbitrary base path
# Source: https://stackoverflow.com/a/46332163/568988
def handle_dir(directory: str, timeout : Optional[int] = None) -> Any:
    return functools.partial(TempDirHandler, directory=directory)

class NoTempDirResponse(Exception):
    def __init__(self, status : int):
        super().__init__("No temp directory location info found")
        self.status = status

# Called when we need to interrupt the server to kill it when it's been idle
class TimeoutException(Exception): pass

def timeout_handler(signum : int, frame : Optional[types.FrameType]) -> None:
    raise TimeoutException("Time is up!")

def add_to_tempdir(tempdir : str,
                   path : str,
                   content : str) -> None:
    temp_path = os.path.join(tempdir, path)
    (dirs, filename) = os.path.split(temp_path)
    os.makedirs(dirs, exist_ok=True)
    print(content, end='', flush=True, file=open(temp_path, 'w'))

def serve_temp(path : str,
               content : str,
               port : int = DEFAULT_PORT,
               timeout : int = DEFAULT_SERVER_TIMEOUT,
               location_filename : str = DEFAULT_LOCATION_FILENAME) -> None:
    attempts = 0
    while attempts <= MAX_RETRY_ATTEMPTS:
        attempts += 1
        try:
            # See if a server is running at our port and ask it where its
            # tempdir is located
            conn = http.client.HTTPConnection("127.0.0.1", port=port)
            conn.request("GET", location_filename)
            response = conn.getresponse()
            status = response.status

            # Any running version of our server should tell us where its
            # tempdir is
            if status != 200: raise NoTempDirResponse(status)
            tempdir = response.read().decode("utf-8")

            # Write the string to the specified path, relativized to
            # the tempdir's location
            add_to_tempdir(tempdir, path, content)

            return # don't loop back to start again

        # Seems like the server isn't started yet...
        except ConnectionRefusedError as e:
            # Try to serve the server on a forked process
            newpid = os.fork()
            if newpid != 0: return
            try:
                # Make a new temporary directory to serve all our files from
                with tempfile.TemporaryDirectory() as tempdir:
                    with socketserver.ThreadingTCPServer(("", port), handle_dir(tempdir)) as httpd:

                        # Store the location of the tempdir in itself
                        print(tempdir, end='', flush=True,
                              file=open(os.path.join(tempdir, location_filename), 'w'))

                        # Write the string to the specified path, relativized to
                        # the tempdir's location
                        add_to_tempdir(tempdir, path, content)

                        # Serve all files in the temporary directory:
                        while True:
                            signal.signal(signal.SIGALRM, timeout_handler)
                            signal.alarm(timeout)
                            try:
                                httpd.handle_request()
                            except TimeoutException:
                                break
                            signal.alarm(0)

                        return # don't loop back to start again

            except OSError as e:
                if e.errno != 48 and e.errno != 61: raise e
                # Someone else must have just started the server while we were
                # executing the logic above... let's pause for a split sec and
                # then try connecting to it...
                time.sleep(0.1 * math.pow(2, attempts)) # This sleep is important! Without
                # the sleep above, we might end up in a tight loop for
                # contention on the network interface, and fork-bomb the system
                continue

def self_refreshing_html(title : str, body_path : str) -> str: \
return """
<!DOCTYPE html>
<html>
    <head>
        <meta charset="UTF-8">
        <title>""" + title + """</title>
    </head>

    <body>
    </body>

    <script type="text/javascript">
     window.onload = function() {
         reload();
     }

     function reload() {
         var xhttp = new XMLHttpRequest();
         xhttp.onreadystatechange = function() {
             if (this.readyState == 4 && this.status == 200) {
                 var response = xhttp.responseText;
                 // console.log(response);
                 document.body.innerHTML = response;
                 var content = document.getElementById('content')
                 var title = content.getAttribute('data-page-title');
                 var nextRefresh = content.getAttribute('data-next-refresh');
                 document.title = title;
                 setTimeout(reload, parseInt(nextRefresh));
             }
         };

         // Assembling the request URL:
         var port = window.location.port;
         var colon = (port == "") ? "" : ":";
         var url = "http://localhost".concat(colon, port, "/", \"""" + body_path + """\");
         // console.log(url);

         // Sending the request:
         xhttp.open("GET", url, true);
         xhttp.send();
     }
    </script>
</html>
"""

def serve_self_refreshing(path : str,
                          title : str,
                          content : str,
                          refresh_interval : float = 1,
                          page_timeout : int = 10) -> None:
    refresh_interval_millis = math.floor(refresh_interval * 1000)
    body_path = hashlib.sha256(path.encode()).hexdigest() + ".html"
    html_frame = self_refreshing_html(title, body_path)
    wrapped_content = \
        """<div id="content" data-next-refresh=\"""" \
        + str(refresh_interval_millis) \
        + """\" data-page-title=\"""" \
        + title + """\">""" \
        + content \
        + """</div>"""
    serve_temp(body_path, wrapped_content, timeout=page_timeout)
    serve_temp(path, html_frame, timeout=page_timeout)

def main() -> None:
    i = 0
    while True:
        i = i + 1
        serve_self_refreshing("test/this/thing.html", str(i), "<h1>" + str(i) + "</h1>", refresh_interval=0.25)
        time.sleep(1)

if __name__ == "__main__":
    main()
