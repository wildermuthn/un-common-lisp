;;; blog package for un-common-lisp.com

(in-package :com.un-common-lisp.blog)

(defvar *h-server*) 
(defvar *posts* ())
(defvar *saved-posts* ())

;; postgres class methods and definitions and utilities

(defclass mpost ()
  ((title :col-type string :initarg :title :accessor title :initform (random-word-s 5))
   (id :col-type serial :accessor id)
   (post-time :col-type integer :initform (timestamp-to-unix (now)))
   (author :col-type string :initarg :author :accessor author :initform (random-word-s 2))
   (comments :initarg :comments :accessor comments :initform '())
   (summary :col-type string :initarg :summary :accessor summary :initform (random-word-s 25))
   (post-text :col-type string :initarg :post-text :accessor post-text :initform (random-word-s 200)))
   (:metaclass dao-class)
   (:keys id))

(defclass mcomment ()
   ((id :col-type serial :accessor id)
    (post-id :col-type integer :initarg :post-id :accessor post-id)
    (comment-time :col-type integer :initform (timestamp-to-unix (now)))
    (author :col-type string :initarg :author :accessor author :initform "anonymous")
    (post-text :col-type string :initarg :post-text :accessor post-text :initform ""))
   (:metaclass dao-class)
   (:keys id post-id))

;; Save and Restore and Flush Database

(defun save-db ()
  (progn
    (setf *saved-posts* *posts*)))
   
(defun restore-db ()
  (setf *posts* *saved-posts*))

(defun flush-db ()
  (progn
    (save-db)
    (setf *posts* ())))

(defun reset-db (&key (random nil))
  (progn
    (save-db)
    (flush-db)
    (reset-mcomment-table)
    (reset-mpost-table)
    (if random
      (postgres-populate-db random))
    (load-postgres-db)
    (assign-post-comments)))

(defun postgres-populate-db (a)
  (progn
    (dotimes (i a)
      (insert-dao (make-instance 'mpost)))
    (dolist (x (select-dao 'mpost 
                           (:> 'id  0)))
      (format t "~a~%" (id x)))
    (dotimes (i a)
      (insert-dao (make-instance 'mcomment :post-id i)))
    (dolist (x (select-dao 'mcomment 
                           (:> 'id  0)))
      (format t "~a~%" (id x)))))

(defun load-postgres-db ()
    (setf *posts* (select-dao 'mpost (:> 'id 0))))

(defun reset-mcomment-table ()
  (progn
    (query (:drop-table 'mcomment))
    (execute (dao-table-definition 'mcomment))))

(defun reset-mpost-table ()
  (progn
    (query (:drop-table 'mpost))
    (execute (dao-table-definition 'mpost))))

;; Postgres utilities

(defun get-post-comments (id)
  (let ((lst (list)))
    (dolist (i 
              (query (:select 'mcomment.id :from 'mcomment
                      :inner-join 'mpost :on (:= 'mpost.id 'mcomment.post-id)
                      :where (:= 'mcomment.post-id id))))
      (push (car (select-dao 'mcomment (:= 'id (car i)))) lst))
    lst))

(defun assign-post-comments ()
    (dolist (i *posts*)
      (setf (comments i) (get-post-comments(id i)))))

;; Class method and definitions and utilities

(defun create-mpost (&key title summary post-text author)
  (push (make-dao 'mpost :title title :summary summary :post-text post-text :author author) *posts*))

(defun create-mcomment (&key author post-text post-ref)
  (progn
    (make-dao 'mcomment :author author :post-text post-text :post-id post-ref)
    (assign-post-comments)))

(defun change-title (new-title post)
  (setf (title post) new-title))


;; Random post generation utilities

(defun random-word-s (length)
  (let ((accum-words "")
        (w-list (loop repeat (/ length 2) collect (two-words) into word-list
                      finally (return word-list))))
    (dolist (word w-list accum-words)
      (setf accum-words (concatenate 'string accum-words word)))))

(defun two-words () 
  (concatenate 'string (random-word (max 3 (random 8))) " " (random-word (max 3 (random 8)))))

;; Server

(hunchentoot:define-easy-handler (some-handler-post :uri "/rest/all-posts") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((request-type (hunchentoot:request-method hunchentoot:*request*)))
    (cond ((eq request-type :get)
           (send-json *posts*)))))

(hunchentoot:define-easy-handler (create-post-handler :uri "/rest/create-post") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((request-type (hunchentoot:request-method hunchentoot:*request*)))
    (cond  ((eq request-type :post)
           (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                  (json-obj data-string))
             (setf *sample-post-data* (json:decode-json-from-string data-string))
             (apply #'create-mpost (post-to-plist (json:decode-json-from-string data-string)))
             json-obj)))))

(hunchentoot:define-easy-handler (create-comment-handler :uri "/rest/create-comment") ()
  (setf (hunchentoot:content-type*) "application/json")
  (let* ((request-type (hunchentoot:request-method hunchentoot:*request*)))
    (cond  ((eq request-type :post)
           (let* ((data-string (hunchentoot:raw-post-data :force-text t))
                  (json-obj data-string))
             (print (post-to-plist (json:decode-json-from-string data-string)))
             (apply #'create-mcomment (post-to-plist (json:decode-json-from-string data-string)))
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

;; Mail function  

(defun send-test-email ()
  (sendmail:with-email (mail-stream "nate@501creative.com" :from "admin@un-common-lisp.com" :subject "test")
    (format mail-stream "Hi there!")))

;; Application utilities

(defun start-server() 
  (setf *h-server* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8080))))
  
(defun stop-server () 
  (hunchentoot:stop *h-server*))

(defun start-database ()
  (connect-toplevel "blog" "postgres" "password" "localhost"))

(defun stop-database ()
  (disconnect-toplevel))
