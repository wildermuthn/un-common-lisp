(in-package :cl-user)

(setf asdf:*central-registry*
      '(*default-pathname-defaults*
      #p"/srv/lisp/un-common-lisp/lisp/cl-cms/")) ;; This path will depend on your own system, be sure to end in a slash

(ql:quickload :hunchentoot)
(ql:quickload :cl-json)
(ql:quickload :ironclad)
(ql:quickload :babel)
(ql:quickload :local-time)
(ql:quickload :cl-utilities)
(ql:quickload :cl-sendmail)
(asdf:load-system :cl-cms) ;; TODO: make cl-cms useful enough for inclusion in QuickLisp

(defpackage :com.un-common-lisp.main
  (:use :common-lisp
        :cl-cms
        :cl-sendmail))
