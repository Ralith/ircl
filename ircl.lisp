(in-package :ircl)

(defclass message ()
  ((prefix :initarg :prefix :accessor prefix)
   (command :initarg :command :accessor command)
   (parameters :initarg :parameters :accessor parameters)))

(defun connect (host &key (port 6667) ssl)
  (if ssl
      (error "SSL support is TODO")
      (socket-connect host port)))

(defun disconnect (connection)
  (socket-close connection))

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

(defun parse-message (message)
  (let ((point 0)
        (prefix) (command) (parameters))
    (when (char= #\: (aref message 0))
      (incf point)
      (setf prefix (parse-until message '(" ") point))
      (incf point))
    (setf command (parse-until message '(" ") point))
    (loop while point do
         (incf point)
         (if (char= #\: (aref message point))
             (progn
               (push (subseq message (1+ point)) parameters)
               (return))
             (push (parse-until message '(" " nil) point) parameters)))
    (make-instance 'message :prefix prefix :command command :parameters (nreverse parameters))))

;; Blocks until a complete message is available, then returns parsed form
(defun get-message (connection &optional timeout)
  (when (if timeout
            (wait-for-input connection :timeout timeout)
            (wait-for-input connection))
    (let ((raw))
      (setf raw (read-line (socket-stream connection)))
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
           (if (and include-prefix (prefix message))
               (concatenate 'string ":" (prefix message) " ")
               "")
           (command message)
           params)))

(defun send-message (connection message)
  (write-string (message->string message nil)
                (socket-stream connection)))

(defun send-raw (connection string)
  (write-string string (socket-stream connection)))