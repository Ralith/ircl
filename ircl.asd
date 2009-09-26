(asdf:defsystem :ircl
  :description "A minimalist IRC library"
  :depends-on (:usocket)
  :components
  ((:file "package")
   (:file "ircl" :depends-on ("package"))))