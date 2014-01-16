;;; Blog Package for un-common-lisp.com

(in-package :com.un-common-lisp.blog)

(defvar *h-server*) 
(defvar *posts* ())
(defvar *saved-posts* ())
(defvar *sample-post-data*)
(defvar *post-count* 0)

;; Save and Restore and Flush Database

(defun save-db ()
  (setf *saved-posts* *posts*))

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

(defmethod print-object ((object post) stream)
    (print-unreadable-object (object stream :type t)
          (with-slots (title post-id) object
                  (format stream "~%title: ~a~%post-id:~a" title post-id))))

(defun create-post (&key title summary text author)
  (push (make-instance 'post :title title :summary summary :text text :author author) *posts*) 
  
(defun create-random-post ()
  (push (make-instance 'post) *posts*) 
  
(defun populate-db (length)
  (progn
    (dotimes (n length n) (create-random-post))
    (make-first-post)) 
  
;; Random post generation utilities

(defun random-word-s (length)
  (let ((accum-words "")
        (w-list (loop repeat (/ length 2) collect (two-words) into word-list
                      finally (return word-list))))
    (dolist (word w-list accum-words)
      (setf accum-words (concatenate 'string accum-words word)))))

(defun two-words () 
  (concatenate 'string (random-word (max 3 (random 8))) " " (random-word (max 3 (random 8)))))

;; Hard-coded First Post

(defun make-first-post ()
  (create-post :title "Hard-coded, Hard-coding" 
               :author "Nate Wildermuth"
               :summary "Common Lisp has always intruiged me. Its been called the most powerful language that exists. But is it true? This blog explores that claim. It is driven by a LISP-powered REST server. The front-end is AngularJS."
               :text "This post is hard-coded into a variable. Is the word 'variable' even the right word to use? It's a property list. This paragraph comes from the :text property on a list. The list is transformed into a hash that the CL-JSON library then transforms into JSON that the AngularJS is capable of transforming into a post. But these are technicalities. Why LISP? Why CL? Why not PHP, Python, Ruby, Nodejs, or any other proper language? Why an old language that no one seems to use? For me, two words suffice: <a href=\"http://www.paulgraham.com/avg.html\">Paul Graham.</a>"))

 
;; Hunchentoot Definitions 
 
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

(defun plist-to-hash (x)
  (let ((y (make-hash-table)))
    (do ((property-name (car x) (car x))
         (property-value (cadr x) (cadr x)))
      ((equal nil (car x)) y)
      (setf (gethash property-name y) property-value)
      (setf x (cddr x)))))
 
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
  
