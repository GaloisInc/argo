import http.server
import http.client
import socketserver
import socket
import random
import functools
import tempfile
import os
import sys
import signal
import types
import time
import hashlib
import math
import subprocess
import multiprocessing
from typing import Optional, Any, Type, Callable

DEFAULT_PORT = 27182
DEFAULT_LOCATION_FILENAME = '.__LOCATION__'
DEFAULT_CONTENT_DIRECTORY= '.__CONTENT__'

class TempDirHandler(http.server.SimpleHTTPRequestHandler):
    # Suppress logging output
    def log_message(*args : Any, **kwargs : Any) -> Any:
        pass

# Serving files from an arbitrary base path
# Source: https://stackoverflow.com/a/46332163/568988
def handle_dir(directory: str) -> Any:
    return functools.partial(TempDirHandler, directory=directory)

def run_dashboard(port : int = DEFAULT_PORT,
                  location_filename : str = DEFAULT_LOCATION_FILENAME):
    if os.fork() != 0: return
    with tempfile.TemporaryDirectory() as tempdir:
        with socketserver.TCPServer(("", port), handle_dir(tempdir)) as httpd:
            # Store the location of the tempdir in itself
            print(tempdir, end='', flush=True,
                  file=open(os.path.join(tempdir, location_filename), 'w'))
            # Serve all files in the temporary directory:
            httpd.serve_forever()

class NoTempDirResponse(Exception):
    def __init__(self, status : int):
        super().__init__("No temp directory location info found")
        self.status = status

def add_to_tempdir(tempdir : str,
                   path : str,
                   content : str) -> None:
    temp_path = os.path.join(tempdir, path)
    (dirs, filename) = os.path.split(temp_path)
    os.makedirs(dirs, exist_ok=True)
    print(content, end='', flush=True, file=open(temp_path, 'w'))

def serve_temp(path : str, content : str,
               port : int = DEFAULT_PORT,
               location_filename : str = DEFAULT_LOCATION_FILENAME) -> None:
    attempted_server = False
    success = False
    while not success:
        # See if a server is already running at our port
        try:
            conn = http.client.HTTPConnection("127.0.0.1", port=port)
            conn.request("GET", location_filename)
            response = conn.getresponse()
            status = response.status

            # Any running version of our server should tell us where its tempdir is
            if status != 200: raise NoTempDirResponse(status)
            tempdir = response.read().decode("utf-8")

            # Write the string to the specified path in the tempdir
            add_to_tempdir(tempdir, path, content)

            # It worked!
            success = True

        # If not, start a server
        except ConnectionRefusedError:
            if not attempted_server:
                # Serve all files in the temporary directory, forever:
                proc = multiprocessing.Process(target=run_dashboard,
                                               args=(port, location_filename))
                proc.start()

                # Try to connect to new server after a slight pause
                attempted_server = True
                time.sleep(0.1)
            else:
                raise ValueError("Could not start dashboard server")

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

     var refreshTimer;

     function reload() {
         var xhttp = new XMLHttpRequest();
         xhttp.onreadystatechange = function() {
             if (this.readyState == 4 && this.status == 200) {
                 var response = xhttp.responseText;
                 // console.log(response);
                 document.body.innerHTML = response;
                 var content = document.getElementById('content')
                 var title = content.getAttribute('data-page-title');
                 var nextRefresh = parseInt(content.getAttribute('data-next-refresh'));
                 if (nextRefresh != NaN) {
                     clearInterval(refreshTimer);
                     refreshTimer = setInterval(reload, nextRefresh);
                 }
                 document.title = title;
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

def hash_str(s : str) -> str:
    return hashlib.sha256(s.encode()).hexdigest()

def serve_self_refreshing(path : str,
                          title : str,
                          content : str,
                          refresh_interval : float = 0.2,
                          content_directory : str = DEFAULT_CONTENT_DIRECTORY) -> None:
    refresh_interval_millis = math.floor(refresh_interval * 1000)
    body_path = os.path.join(content_directory, hash_str(path))
    html_frame = self_refreshing_html(title, body_path)
    wrapped_content = \
        """<div id="content" data-next-refresh=\"""" \
        + str(refresh_interval_millis) \
        + """\" data-page-title=\"""" \
        + title + """\">""" \
        + content \
        + """</div>"""
    path = os.path.join(path, "index.html")
    serve_temp(body_path, wrapped_content)
    serve_temp(path, html_frame)

if __name__ == "__main__":
    try: port = int(sys.argv[1])
    except IndexError: port = None
    run_dashboard(port=port)
