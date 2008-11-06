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
    (:h2 "Features")

    (:h3 "Thin JavaScript layer")
    (:p "Thanks to Weblocks' thin JavaScript layer your content degrades gracefully
        on clients that don't have JavaScript enabled.")
    (:p "Features like fully sortable tables use AJAX when available but
        also offer the same user experience without AJAX by just reloading
        the page.")
    (:p "All this happens automatically so you don't have to worry
        about it.")
    
    (:h3 "Powerful modular dispatcher")
    (:p "User-defined Dispatchers based on string and regex matchers are present
        in every major web framework.")
    (:p "The usual way to go about it is having a centralized dispatcher definition
        (usually declarative or functional). Weblocks takes it one step further
        and offers completely modular and customizable dispatchers that can consume
        any amount of URL parts and invoke other dispatchers.")))

(defun make-installation-page ()
  (with-html
    (:p "Getting started")))

(defun make-documentation-page ()
  (with-html
    (:p "Documentation")))

(defun make-contributing-page ()
  (with-html
    (:p "Contributing")))

(defun make-support-page ()
  (with-html
    (:p "Support")))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
        (list (make-navigation "Main"
                               "Welcome" (make-welcome-page)
                               "Features" (make-features-page)
                               "Getting started" (make-installation-page)
                               "Documentation" (make-documentation-page)
                               "Support" (make-support-page)
                               "Contributing" (make-contributing-page)
                               ))))

