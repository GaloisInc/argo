{-# LANGUAGE OverloadedStrings #-}
module Argo.Doc.Protocol (protocolDocs) where

import Data.List.NonEmpty (NonEmpty(..))

import Argo.Doc


-- | Documentation for the details shared by all servers that use this library.
protocolDocs :: Block
protocolDocs =
  Section "Fundamental Protocol" $
   [ Paragraph
     [ Text "This application is a "
     , Link (URL "https://www.jsonrpc.org/specification") "JSON-RPC"
     , Text $
       " server. Additionally, it maintains a persistent cache " <>
       "of application states and explicitly indicates the state " <>
       "in which each command is to be carried out."]
   , Section "Transport"
     [ Paragraph [Text "The server supports three transport methods:"]
     , DescriptionList
       [ ( Literal "stdio" :| []
         , Paragraph [ Text "in which the server communicates over "
                     , Literal "stdin", Text " and ", Literal "stdout"
                     , Text " using ", Link (URL "http://cr.yp.to/proto/netstrings.txt") "netstrings."]
         )
       , ( Text "Socket" :| []
          , Paragraph [ Text "in which the server communicates over a socket using "
                      , Link (URL "http://cr.yp.to/proto/netstrings.txt") "netstrings."
                      ]
          )
       , ( Text "HTTP" :| []
         , Paragraph [Text "in which the server communicates over a socket using HTTP."]
         )
       ]
     ]
   , Section "Application State"
     [ Paragraph
       [ Text "According to the JSON-RPC specification, the ", Literal "params", Text " field in a "
       , Text "message object must be an array or object. In this protocol, it is "
       , Text "always an object. While each message may specify its own arguments, "
       , Text "every message has a parameter field named ", Literal "state", Text "."]
     , Paragraph
       [ Text "When the first message is sent from the client to the server, the "
       , Literal "state", Text " parameter should be initialized to the JSON null value "
       , Literal "null", Text ". Replies from the server may contain a new state that should "
       , Text "be used in subsequent requests, so that state changes executed by the "
       , Text "request are visible. Prior versions of this protocol represented the "
       , Text "initial state as the empty array ", Literal "[]", Text ", but this is now deprecated "
       , Text "and will be removed."
       ]
     , Paragraph
       [ Text "In particular, per JSON-RPC, non-error replies are always a JSON "
       , Text "object that contains a ", Literal "result", Text " field. The result field always "
       , Text "contains an ", Literal "answer", Text " field and a ", Literal "state"
       , Text " field, as well as ", Literal "stdout", Text " and ", Literal "stderr", Text "."
       ]
     , DescriptionList
       [ (k, Paragraph v)
       | (k, v) <-
         [ ( Literal "answer" :| []
           , [ Text "The value returned as a response to the request "
             , Text "(the precise contents depend on which request was sent)"
             ]
           )
         , ( Literal "state" :| []
           , [ Text "The state, to be sent in subsequent requests. If the server did not "
             , Text "modify its state in response to the command, then this state may be "
             , Text "the same as the one sent by the client."
             ]
           )
         , ( Literal "stdout" :| [Text " and ", Literal "stderr"]
           , [ Text "These fields contain the contents of the Unix ", Literal "stdout", Text " and "
             , Literal "stderr", Text " file descriptors. They are intended as a stopgap measure "
             , Text "for clients who are still in the process of obtaining structured "
             , Text "information from the libraries on which they depend, so that "
             , Text "information is not completely lost to users. However, the server may "
             , Text "or may not cache this information and resend it. Applications are "
             , Text "encouraged to used structured data and send it deliberately as the answer."
             ]
           )
         ]
       ]
       , Paragraph
         [ Text "The precise structure of states is considered an implementation detail "
         , Text "that could change at any time. Please treat them as opaque tokens that "
         , Text "may be saved and re-used within a given server process, but not "
         , Text "created by the client directly."
         ]
       ]
   ]
