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
from pathlib import Path
import requests
from typing import Optional, Any, Type, Callable

MYXINE_PORT = 1123

def serve_self_refreshing(path : str, title : str, content : str) -> None:
    url = 'http://localhost:' + str(MYXINE_PORT) + '/' + path.strip('/')
    wrapped_content = '<div id="content">' + content + '</div>'
    requests.post(url = url,
                  params = { 'title': title },
                  data = wrapped_content.encode("utf-8"))
