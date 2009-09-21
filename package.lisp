(defpackage :ircl
  (:use :cl :usocket)
  (:export message
           connect disconnect
           parse-message get-message send-message))