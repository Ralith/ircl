(defpackage :ircl
  (:use :cl :usocket)
  (:export message
           connect disconnect
           make-message parse-message message->string prefix->string
           get-message send-message send-raw
           prefix command parameters
           nick username host))