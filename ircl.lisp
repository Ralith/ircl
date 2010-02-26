(in-package :ircl)

(defclass server ()
  ((host :initarg :host :accessor host)))

(defclass user (server)
  ((nick :initarg :nick :accessor nick)
   (username :initarg :username :accessor username)))

(defclass message ()
  ((command :initarg :command :accessor command)
   (parameters :initarg :parameters :accessor parameters
               :initform nil)))

(defclass received-message (message)
  ((received :initform (get-universal-time) :reader received)
   (prefix :initarg :prefix :accessor prefix)))

(defun make-message (command &rest parameters)
  "Returns a new MESSAGE the given COMMAND and PARAMETERS."
  (make-instance 'message :command command :parameters parameters))

(defun connect (host &key (port 6667) ssl)
  "Establish a connection to the IRC server at HOST."
  (if ssl
      (error "SSL support is TODO")
      (socket-connect host port)))

(defun disconnect (connection)
  (socket-close connection))

(defun take-until (terminators string)
  "Returns a subsequence of STRING ending at one of the strings in TERMINATORS.  A NIL value in TERMINATORS matches the end of the string."
  (loop for point from 0 to (1- (length string)) do
       (mapcar (lambda (x)
                 (if (null x)
                     (when (= point (1- (length string)))
                       (return string))
                     (let ((end (+ point (length x))))
                       (when (and (not (> end (length string)))
                                  (string= x (subseq string point end)))
                         (return (values (subseq string 0 point) point))))))
               terminators)))

(defmacro parse-until (string terminators point)
  "Collect from string starting at POINT until reaching a string in TERMINATORS, returning that sequence and setting POINT to the first point not returned in STRING, or NIL if a subsequence including the end of the string has been returned.  A NIL value in TERMINATORS matches the end of the string."
  (let ((output (gensym))
        (offset (gensym)))
    `(multiple-value-bind (,output ,offset)
         (take-until ,terminators (subseq ,string ,point))
       (setf ,point (if ,offset
                        (+ ,point ,offset)
                        nil))
       ,output)))

(defun parse-prefix (string)
  "Parses IRC prefix STRING into a USER or a SERVER.  STRING must omit the leading colon."
  (let ((point 0)
        (nick) (username) (host))
    (cond
      ((position #\! string)
       (setf nick (parse-until string '("!") point))
       (if (position #\@ string)
           (progn
             (incf point)
             (setf username (parse-until string '("@") point))
             (setf host (subseq string (1+ point))))
           (setf username (subseq string point))))
      ((position #\@ string)
       (setf nick (parse-until string '("@") point))
       (setf host (subseq string (1+ point))))
      (t
       (setf host string)))
    (if nick
        (make-instance 'user
                       :nick nick :username username :host host)
        (make-instance 'server :host host))))

(defun parse-message (string)
  "Parses raw IRC message STRING into a RECEIVED-MESSAGE.  Should only be used for messages received directly from the server, at the time that they are received."
  (let ((point 0)
        (result (make-instance 'received-message)))
    (if (char= #\: (aref string 0))
      (progn
        (incf point)
        (setf (prefix result) (parse-prefix (parse-until string '(" ") point)))
        (incf point))
      (setf (prefix result) nil))
    (setf (command result) (parse-until string '(" ") point))
    (loop while (and point
                     (< point (1- (length string)))) do
         (incf point)
         (if (char= #\: (aref string point))
             (progn
               (push (subseq string (1+ point)) (parameters result))
               (return))
             (push (parse-until string '(" " nil) point) (parameters result))))
    (setf (parameters result) (nreverse (parameters result)))
    result))

(defun get-message (connection &optional timeout)
  "Reads a RECEIVED-MESSAGE from SOCKET, blocking until a full message can be read or optionally until TIMEOUT expires."
  (when (if timeout
            (wait-for-input connection :timeout timeout)
            (wait-for-input connection))
    (let ((raw (read-line (socket-stream connection))))
      (parse-message (subseq raw 0 (1- (length raw)))))))

(defun prefix->string (prefix)
  "Converts PREFIX of type USER or SERVER into the IRC protocol standard string representation."
  (if (typep prefix 'user)
      (format nil "~a!~a@~a"
              (nick prefix) (username prefix) (host prefix))
      (host prefix)))

(defun message->string (message)
  "Converts MESSAGE to IRC protocol string representation."
  (apply #'concatenate 'string
         (when (and (typep message 'received-message)
                    (prefix message))
           (concatenate 'string ":" (prefix->string (prefix message)) " "))
         (command message)
         (mapcar (lambda (x)
                   (when (> (length x) 0)
                     (if (or (position #\Space x)
                             (char= #\: (aref x 0)))
                         (concatenate 'string " :" x)
                         (concatenate 'string " " x))))
                 (parameters message))))

(defun send-message (connection message)
  "Sends IRC MESSAGE over CONNECTION."
  (send-raw connection (concatenate 'string (message->string message))))

(defun send-raw (connection string)
  "Sends raw IRC protocol STRING over SOCKET."
  (format (socket-stream connection) "~a~a~a" string (code-char 13) (code-char 10))
  (force-output (socket-stream connection)))
