;;; -*- Mode: Lisp -*-

(defsystem weblocks-site
  :depends-on ("weblocks" "cl-who" "f-underscore")
  :components ((:file "defs")
	       (:file "site" :depends-on ("defs"))))

