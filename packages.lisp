(in-package :cl-user)

(ql:quickload "hunchentoot")
(ql:quickload "cl-json")
(ql:quickload "local-time")
(ql:quickload "do-urlencode")
(ql:quickload :marshal)

(defpackage :com.un-common-lisp.random-word
  (:use :common-lisp)
  (:export :random-word)) 

(defpackage :com.un-common-lisp.blog
  (:use :common-lisp 
        :hunchentoot 
        :cl-json 
        :com.un-common-lisp.random-word 
        :local-time
        :do-urlencode))
