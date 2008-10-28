(asdf:oos 'asdf:load-op 'weblocks)

(defpackage #:weblocks-site (:use :cl :weblocks))

(in-package :weblocks-site)

(defwebapp weblocks-site
           :description "Weblocks web application framework"
           :prefix "/")

(defun make-welcome-page ()
  (with-html
    (:p "Welcome to Weblocks")))

(defun make-features-page ()
  (with-html
    (:p "Features")))

(defun make-installation-page ()
  (with-html
    (:p "Getting started")))

(defun make-documentation-page ()
  (with-html
    (:p "Documentation")))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
        (list (make-navigation "Main"
                               "Welcome" (make-welcome-page)
                               "Features" (make-features-page)
                               "Getting started" (make-installation-page)
                               "Documentation" (make-documentation-page)))))

