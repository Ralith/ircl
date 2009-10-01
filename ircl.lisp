(in-package :ircl)

(defclass server-prefix ()
  ((host :initarg :host :accessor host)))

(defclass user-prefix (server-prefix)
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
  (make-instance 'message :command command :parameters parameters))

(defun connect (host &key (port 6667) ssl)
  (if ssl
      (error "SSL support is TODO")
      (socket-connect host port)))

(defun disconnect (socket)
  (socket-close socket))

;; nil terminator is end-of-string
(defun take-until (terminators string)
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
  (let ((output (gensym))
        (offset (gensym)))
    `(multiple-value-bind (,output ,offset)
         (take-until ,terminators (subseq ,string ,point))
       (setf ,point (if ,offset
                        (+ ,point ,offset)
                        nil))
       ,output)))

(defun parse-prefix (string)
  (let ((point 0)
        (nick) (username) (host))
    (cond
      ((position #\! string)
       (incf point)
       (setf nick (parse-until string '("!") point))
       (if (position #\@ string)
           (progn
             (setf username (parse-until string '("@") point))
             (setf host (subseq string (1+ point))))
           (setf username (subseq string point))))
      ((position #\@ string)
       (incf point)
       (setf nick (parse-until string '("@") point))
       (setf host (subseq string (1+ point))))
      (t
       (setf host string)))
    (if nick
        (make-instance 'user-prefix
                       :nick nick :username username :host host)
        (make-instance 'server-prefix :host host))))

(defun parse-message (message)
  (let ((point 0)
        (result (make-instance 'received-message)))
    (if (char= #\: (aref message 0))
      (progn
        (incf point)
        (setf (prefix result) (parse-prefix (parse-until message '(" ") point)))
        (incf point))
      (setf (prefix result) nil))
    (setf (command result) (parse-until message '(" ") point))
    (loop while (and point
                     (< point (1- (length message)))) do
         (incf point)
         (if (char= #\: (aref message point))
             (progn
               (push (subseq message (1+ point)) (parameters result))
               (return))
             (push (parse-until message '(" " nil) point) (parameters result))))
    (setf (parameters result) (nreverse (parameters result)))
    result))

;; Blocks until a complete message is available, then returns parsed form
(defun get-message (socket &optional timeout)
  (when (if timeout
            (wait-for-input socket :timeout timeout)
            (wait-for-input socket))
    (let ((raw))
      (setf raw (read-line (socket-stream socket)))
      (setf raw (subseq raw 0 (1- (length raw))))
      (parse-message raw))))

(defun prefix->string (prefix)
  (let ((elems))
    (when (slot-boundp prefix 'host)
      (push (host prefix) elems))
    (if (slot-boundp prefix 'username)
        (progn (push "@" elems)
               (push (username prefix) elems)
               (when (slot-boundp prefix 'nick)
                 (push "!" elems)
                 (push (nick prefix) elems)))
        (when (slot-boundp prefix 'nick)
          (push "@" elems)
          (push (nick prefix) elems)))
    (apply #'concatenate 'string elems)))

(defun message->string (message)
  (let ((params (copy-list (parameters message))))
    (setf params (mapcar (lambda (x)
                           (when (> (length x) 0)
                             (if (or (position #\Space x)
                                     (char= #\: (aref x 0)))
                                 (concatenate 'string " :" x)
                                 (concatenate 'string " " x))))
                         params))
    (apply #'concatenate 'string
           (when (typep message 'received-message)
               (concatenate 'string ":" (prefix->string (prefix message)) " "))
           (command message)
           params)))

(defun send-message (socket message)
  (send-raw socket (concatenate 'string (message->string message)))
  (force-output (socket-stream socket)))

(defun send-raw (socket string)
  (format (socket-stream socket) "~a~a~a" string (code-char 13) (code-char 10))
  (force-output (socket-stream socket)))