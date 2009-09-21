(in-package :ircl)

(defclass prefix ()
  ((nick :initarg :nick :accessor nick)
   (username :initarg :username :accessor username)
   (host :initarg :host :accessor host)))

(defclass message ()
  ((prefix :initarg :prefix :accessor prefix)
   (command :initarg :command :accessor command)
   (parameters :initarg :parameters :accessor parameters
               :initform nil)))

(defun make-message (command parameters)
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
        (result (make-instance 'prefix)))
    (cond
      ((position #\! string)
       (setf (nick result) (parse-until string '("!") point))
       (incf point)
       (if (position #\@ string)
           (progn
             (setf (username result) (parse-until string '("@") point))
             (setf (host result) (subseq string (1+ point))))
           (setf (username result) (subseq string point))))
      ((position #\@ string)
       (setf (nick result) (parse-until string '("@") point))
       (setf (host result) (subseq string (1+ point))))
      (t
       (setf (host result) string)))
    result))

(defun parse-message (message)
  (let ((point 0)
        (result (make-instance 'message)))
    (when (char= #\: (aref message 0))
      (incf point)
      (setf (prefix result) (parse-prefix (parse-until message '(" ") point)))
      (incf point))
    (setf (command result) (parse-until message '(" ") point))
    (loop while point do
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

(defun contains-space (string)
  (loop for i from 0 to (1- (length string)) do
       (when (char= #\Space (aref string i))
         (return t))))

(defun message->string (message &optional (include-prefix t))
  (let ((params (copy-list (parameters message))))
    (setf params (mapcar (lambda (x)
                           (if (contains-space x)
                               (concatenate 'string " :" x)
                               (concatenate 'string " " x)))
                         params))
    (apply #'concatenate 'string
           (if (and include-prefix (slot-boundp message 'prefix))
               (concatenate 'string ":" (prefix message) " ")
               "")
           (command message)
           params)))

(defun send-message (socket message)
  (write-string (message->string message nil)
                (socket-stream socket)))

(defun send-raw (socket string)
  (write-string string (socket-stream socket)))