;;; blog package for un-common-lisp.com

(in-package :com.un-common-lisp.blog)

(defvar *h-server*) 
(defvar *posts* ())
(defvar *saved-posts* ())
(defvar *sample-post-data*)
(defvar *post-count* 0)

;; Save and Restore and Flush Database
(defun save-db ()
  (progn
    (setf *saved-posts* *posts*)
    (with-open-file (stream "/srv/blog-db/posts.db" :direction :output :if-exists :supersede)
      (print (ms:marshal *posts*) stream))) )

(defun load-db ()
  (with-open-file (s "/srv/blog-db/posts.db")
    (setf *posts* (loop with eof = (list nil)
                        for form = (read s nil eof)
                        until (eq form eof)
                        collect form into forms
                        counting t into form-count
                        finally (return (apply #'ms:unmarshal (values forms form-count)))))))
    
(defun restore-db ()
  (setf *posts* *saved-posts*))

(defun flush-db ()
  (progn
    (save-db)
    (setf *post-count* 0)
    (setf *posts* ())))

(defun reset-db ()
  (progn
    (save-db)
    (flush-db)
    (populate-db 5)))


;; Class method and definitions and utilities

(defun get-post-id ()
  (progn
    (incf *post-count*)
    *post-count*))

(defclass post ()
  ((title :initarg :title :accessor title :initform (random-word-s 5))
   (post-id :accessor post-id :initform (get-post-id))
   (post-time :initform (timestamp-to-unix (now)))
   (author :initarg :author :accessor author :initform (random-word-s 2))
   (summary :initarg :summary :accessor summary :initform (random-word-s 25))
   (text :initarg :text :accessor text :initform (random-word-s 200))))

(defmethod ms:class-persistant-slots ((self post))
  '(title post-id post-time author summary text))

(defmethod print-object ((object post) stream)
    (print-unreadable-object (object stream :type t)
          (with-slots (title post-id) object
                  (format stream "~%title: ~a~%post-id:~a" title post-id))))

(defun create-post (&key title summary text author)
  (progn
    (push (make-instance 'post :title title :summary summary :text text :author author) *posts*)
    (save-db)))
  
(defun create-random-post ()
  (push (make-instance 'post) *posts*))
  
(defun populate-db (length)
  (progn
    (dotimes (n length n) (create-random-post))))
  
(defun set-post-timestamp (id timestamp)
  (dolist (lst *posts* lst)
    (with-slots (post-id post-time) lst
      (if (eq post-id id) (setf post-time timestamp)))))
  
;; Random post generation utilities

(defun random-word-s (length)
  (let ((accum-words "")
        (w-list (loop repeat (/ length 2) collect (two-words) into word-list
                      finally (return word-list))))
    (dolist (word w-list accum-words)
      (setf accum-words (concatenate 'string accum-words word)))))

(defun two-words () 
  (concatenate 'string (random-word (max 3 (random 8))) " " (random-word (max 3 (random 8)))))

 
(hunchentoot:define-easy-handler (some-handler-post :uri "/rest/all-posts") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((request-type (hunchentoot:request-method hunchentoot:*request*)))
    (cond ((eq request-type :get)
           (send-json *posts*)))))

(hunchentoot:define-easy-handler (some-handler :uri "/rest/create-post") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((request-type (hunchentoot:request-method hunchentoot:*request*)))
    (cond  ((eq request-type :post)
           (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                  (json-obj data-string))
             (setf *sample-post-data* (json:decode-json-from-string data-string))
             (apply #'create-post (post-to-plist (json:decode-json-from-string data-string)))
             json-obj)))))

;; Server io utilities 

(defun send-json (x)
    (json:encode-json-to-string x))

(defun post-to-plist (post-data)
  (let ((lst (list)))
  (dolist (i post-data)
    (push (car i) lst)
    (push (cdr i) lst))
  (nreverse lst)))

;; Application utilities

(defun start-server() 
  (setf *h-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080 
                                                                                :access-log-destination nil))))
  
(defun stop-server () 
  (hunchentoot:stop *h-server*))
  
