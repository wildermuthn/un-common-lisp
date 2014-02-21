;;; CL-CMS Blog 

(in-package :com.un-common-lisp.main)

(defun start-blog ()
  (cl-cms::start-server "blog" 8070))

(defun stop-blog ()
  (cl-cms::stop-server))

(setf sendmail::*SENDMAIL* "/usr/sbin/sendmail")

(defun send-test-email ()
  (sendmail:with-email (mail-stream "nate@501creative.com" :from "admin@un-common-lisp.com" :subject "Test Subject")
    (format mail-stream "Test Body")))
