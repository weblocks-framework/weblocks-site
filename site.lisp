(push "/home/sky/weblocks/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'weblocks)

(defpackage #:weblocks-site (:use :cl :weblocks :cl-who))

(in-package :weblocks-site)

(defwebapp weblocks-site
           :description "Weblocks web application framework"
           :prefix "/"
           :public-files-path "./pub")

(defmacro make-page (title &body body)
  `(lambda ()
     (with-html
       (:h2 (esc ,title))
       ,@body)))

(defun make-welcome-page ()
  (make-page "Welcome to Weblocks"))

(defun make-features-page ()
  (make-page "Features"
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
  (make-page "Getting started"))

(defun make-documentation-page ()
  (make-page "Documentation"
    (:h3 "Tutorials and blog posts")))

(defun make-contributing-page ()
  (make-page "Contributing"))

(defun make-support-page ()
  (make-page "Support"))

(defmethod render-widget-body ((obj navigation) &rest args)
  ;; we just cheat a bit until the new dispatching/rendering separation
  ;; is ready for production
  (let ((body-html (with-html-output-to-string (*weblocks-output-stream*)
                     (:div :class "navigation-body"
                           (call-next-method)))))
  (apply #'render-navigation-menu obj args)
  (format *weblocks-output-stream* body-html)))

(defun init-user-session (comp)
  (setf (composite-widgets comp)
        (list (make-navigation "Main"
                               "Welcome" (make-welcome-page)
                               "Features" (make-features-page)
                               "Getting started" (make-installation-page)
                               "Documentation" (make-documentation-page)
                               "Support" (make-support-page)
                               "Contributing" (make-contributing-page)))))

