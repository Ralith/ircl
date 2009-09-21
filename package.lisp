(defpackage :ircl
  (:use :cl :usocket)
  (:export message
           connect disconnect
           make-message parse-message
           get-message send-message
           prefix command parameters
           nick username host))