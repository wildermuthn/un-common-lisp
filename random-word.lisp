(in-package :com.un-common-lisp.random-word)

(defparameter *valid-chars* '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
 
(defparameter *table* nil)
(defparameter *norm-table* nil)
 
(defun random-choice (list)
  (let ((r (random (length list))))
    (nth r list)))
 
(defun random-word (length &key (start-char nil))
  (unless start-char
    (setf start-char (random-choice *valid-chars*)))
  (coerce   
   (cons start-char
         (loop for i below (1- length) collecting        
              (let ((next-char (random-choice *valid-chars*)))
                (setf start-char next-char))))
   'string))
