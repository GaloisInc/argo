module Argo.PythonBindings where

import Paths_argo_python

getArgoPythonFile :: FilePath -> IO FilePath
getArgoPythonFile file = getDataFileName file
