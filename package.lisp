(defpackage :ircl
  (:use :cl :usocket)
  (:export message received-message
           received command parameters
           server-prefix user-prefix
           nick username host
           connect disconnect
           make-message parse-message message->string prefix->string
           get-message send-message send-raw))