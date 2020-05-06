import requests

MYXINE_PORT = 1123

# Loading SVG is licensed for free reuse from https://icons8.com/preloaders/
LOADING_SVG = \
    """<svg xmlns:svg="http://www.w3.org/2000/svg"
    xmlns="http://www.w3.org/2000/svg"
    xmlns:xlink="http://www.w3.org/1999/xlink"
    version="1.0" width="35pt" height="35pt" viewBox="0 0 128 128"
    xml:space="preserve">
    <g><path d="M59.6 0h8v40h-8V0z" fill="#000"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(30 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(60 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(90 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#ccc" transform="rotate(120 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#b2b2b2" transform="rotate(150 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#999" transform="rotate(180 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#7f7f7f" transform="rotate(210 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#666" transform="rotate(240 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#4c4c4c" transform="rotate(270 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#333" transform="rotate(300 64 64)"/>
    <path d="M59.6 0h8v40h-8V0z" fill="#191919" transform="rotate(330 64 64)"/>
    <animateTransform attributeName="transform" type="rotate"
    values="0 64 64;30 64 64;60 64 64;90 64 64;120 64 64;150 64 64;180 64 64;210 64 64;240 64 64;270 64 64;300 64 64;330 64 64"
    calcMode="discrete" dur="1320ms" repeatCount="indefinite">
    </animateTransform></g></svg>"""


def serve_self_refreshing(path: str, title: str, content: str) -> None:
    url = 'http://localhost:' + str(MYXINE_PORT) + '/' + path.strip('/')
    wrapped_content = '<div id="content">' + content + '</div>'
    requests.post(url=url,
                  params={'title': title},
                  data=wrapped_content.encode("utf-8"))


# class AllVerificationResults:
#     __results: Dict[uuid.UUID, VerificationResult]
#     __qed_called: bool
#     __proceeding_normally: bool

#     def __init__(self) -> None:
#         self.__results = {}
#         self.__update_dashboard__()
#         self.__qed_called = False
#         self.__proceeding_normally = True

#     def __add_result__(self, result: VerificationResult) -> None:
#         self.__results[result._unique_id] = result
#         self.__update_dashboard__()

#     def dot_graph(self) -> str:
#         out = "digraph { \n"
#         for _, result in self.__results.items():
#             # Determine the node color
#             if result:
#                 color = "green"
#                 bgcolor = "lightgreen"
#             else:
#                 color = "red"
#                 bgcolor = "lightpink"
#             # Determine the node attributes
#             node_attrs: Dict[str, str] = {
#                 'label': result.contract.__class__.__name__,
#                 'color': color,
#                 'bgcolor': bgcolor,
#                 'fontname': "Courier",
#                 'shape': 'rect',
#                 'penwidth': '2',
#             }
#             # Render the attributes
#             node_attr_string = ""
#             for key, val in node_attrs.items():
#                 node_attr_string += key + " = \"" + val + "\"; "
#             # Render this node line
#             out += '    "' + str(result._unique_id) \
#                 + '" [' + node_attr_string.rstrip('; ') + "];\n"
#             # Render each of the assumption edges
#             for assumption in result.assumptions[:]:
#                 edge_attrs: Dict[str, str] = {
#                     'penwidth': '2',
#                     'arrowType': 'open',
#                 }
#                 edge_attr_string = ""
#                 for key, val in edge_attrs.items():
#                     edge_attr_string += key + " = \"" + val + "\"; "
#                 out += '    "' \
#                     + str(assumption._unique_id) \
#                     + '" -> "' \
#                     + str(result._unique_id) \
#                     + '" [' + edge_attr_string.rstrip('; ') + '];\n'
#         out += "}"
#         # print(out)
#         return out

#     def svg_graph(self) -> str:
#         # Generate a GraphViz DOT representation
#         dot_repr = self.dot_graph()
#         # Write out & render the DOT file and open it in a web browser
#         svg = subprocess.check_output(["dot", "-T", "svg"],
#                                       input=dot_repr,
#                                       text=True)
#         return svg

#     def errors_html(self) -> str:
#         # Generate an HTML representation of all the errors so far
#         out = '<div style="padding: 20pt; '\
#             'font-family: Courier; text-align: left">'
#         if not self.all_ok():
#             out += '<h2>Errors:</h2>'
#         for _, result in self.__results.items():
#             if isinstance(result, VerificationFailed):
#                 out += '<p style="font-size: 16pt">'
#                 out += '<b>' + result.contract.__class__.__name__ + ': </b>'
#                 out += '<span style="color: firebrick">'
#                 out += str(result.exception)
#                 out += '</span>'
#                 out += '</p>'
#         out += '</div>'
#         return out

#     def dashboard_html(self) -> str:
#         progress : str
#         progress = '<div style="font-weight: normal; font-family: Courier; font-size: 20pt; padding: 20pt">'
#         if self.__qed_called:
#             if self.all_ok():
#                 progress += '✅ <span style="font-size: 25pt">(successfully verified!)</span>'
#             elif self.__proceeding_normally:
#                 progress += '🚫 <span style="font-size: 25pt">(failed to verify)</span>'
#             else:
#                 progress += '🚫 <span style="font-size: 25pt">(incomplete: exception during proving)</span>'
#             progress += '</div>'
#         else:
#             progress += dashboard.LOADING_SVG \
#                 + '<br/><span style="font-size: 25pt"><i style="font-weight: normal">(running...)</i></span>'
#         progress += "</div>"
#         if designated_dashboard_path is not None:
#             proof_name: str = os.path.basename(designated_dashboard_path)
#             return \
#                 '<center><h1 style="font-family: Courier">' \
#                 + proof_name \
#                 + """</h1><div height="100%>""" \
#                 + self.svg_graph() \
#                 + "</div>" \
#                 + "<div>" \
#                 + progress \
#                 + self.errors_html() \
#                 + "</div></center>"
#         else:
#             raise ValueError("Can't render dashboard HTML before dashboard is initialized")

#     def __update_dashboard__(self) -> None:
#         if designated_dashboard_path is not None:
#             dashboard.serve_self_refreshing(designated_dashboard_path,
#                                             os.path.basename(designated_dashboard_path),
#                                             self.dashboard_html())

#     def all_ok(self) -> bool:
#         # Iterate through all lemmata to determine if everything is okay
#         # Builds a graph of all dependencies
#         ok = True
#         for _, result in self.__results.items():
#             if not result:
#                 ok = False
#         return ok

#     def __qed__(self, complete: bool) -> None:
#         self.__qed_called = True
#         self.__proceeding_normally = \
#             self.__proceeding_normally and complete
#         self.__update_dashboard__()
#         if not self.all_ok():
#             print("Verification did not succeed.", file=sys.stderr)
#             sys.exit(1)
#         else:
#             print("Verification succeeded.", file=sys.stderr)
#             sys.exit(0)

# # Set up the dashboard path
# if designated_dashboard_path is None:
#     if dashboard_path is None:
#         current_frame = inspect.currentframe()
#         if current_frame is None:
#             raise ValueError("Cannot automatically assign a dashboard URL"
#                              " outside a file; use the explicit option"
#                              " `dashboard_path = \"...\"` when calling "
#                              "`connect()`")
#         else:
#             f_back = current_frame.f_back
#             if f_back is not None:
#                 filename = os.path.realpath(inspect.getfile(f_back))
#                 dashboard_path = \
#                     re.sub(r'\.py$', '',
#                            posixpath.join(*filename.split(os.path.sep))) \
#                       .replace('^/', '')
#     designated_dashboard_path = dashboard_path
# else:
#     raise ValueError("There is already a designated dashboard URL."
#                      " Did you call `connect()` more than once?")

# # Print the dashboard path
# print("Dashboard:", get_designated_url(), file=sys.stderr)

# def get_designated_url() -> str:
#     global designated_dashboard_path
#     if designated_dashboard_path is None:
#         raise ValueError("There is not yet a designated dashboard URL")
#     else:
#         return "http://localhost:" + str(dashboard.MYXINE_PORT) \
#             + "/" + designated_dashboard_path
