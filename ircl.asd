(asdf:defsystem :ircl
  :description "A minimalist IRC library"
  :depends-on (:usocket)
  :components
  ((:file "ircl")))