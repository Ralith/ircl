(defpackage :ircl
  (:use :cl :usocket)
  (:export message received-message
           server-prefix user-prefix
           connect disconnect
           make-message parse-message message->string prefix->string
           get-message send-message send-raw
           received command parameters
           nick username host))