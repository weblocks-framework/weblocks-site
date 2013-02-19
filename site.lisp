
(in-package :weblocks-site)

(defparameter *doc-uri* "http://weblocks.net/docs/gen/weblocks-package/")

(defwebapp weblocks-site
           :description "Weblocks web application framework"
           :prefix "/"
           :public-files-path "./pub/"
           :dependencies (list (make-instance 'stylesheet-dependency :url "/pub/stylesheets/main.css")))

(defmacro make-page (title &body body)
  `(make-widget
     (lambda (&rest stuff)
       (declare (ignore stuff))
       (with-html
	 (:h2 (esc ,title))
	 ,@body))))

(defun make-welcome-page ()
  (make-page "Welcome to Weblocks"
    (:p "Weblocks is an advanced web framework written in Common Lisp.")
    (:p "It is designed to make Agile web application development as
        effective and simple as possible.")

    (:h3 "Is it usable? Can I see some demos?")
    (:p "Weblocks is well-tested and has proven its worth in daily usage. It is used
        by a community of developers all over the world.")
    (:p "Public applications running Weblocks include "
        (:a :href "http://www.lamsight.org" "LAMSIGHT2") ", "
        (:a :href "http://www.thanandar.de" "Thanandar") ", and "
	(:a :href "https://bountyoss.com/" "BountyOSS") ".")

    (:h3 "Why yet another web framework?")
    (:p "This is not your ordinary run-of-the-mill web framework in PHP, Python or Ruby.")
    (:p "Weblocks uses powerful Lisp features like multiple dispatch, the metaobject protocol,
        lexical closures, keyword arguments, and macros to build abstractions that make web
        development easy, intuitive, and free of boilerplate. In addition, control flow is easily
        expressed using continuations.")
    (:p "Things that are hard or mundane in other frameworks become easy and fun in
        Weblocks.")

    (:h3 "In Common Lisp?")
    (:p "Common Lisp is a powerful standardized language with high-performance implementations.")
    (:p "Weblocks makes use of several advanced features that cannot be found
        in most other programming languages.")
    (:p "Moreover, Common Lisp itself is ideally suited to modern pragmatic and
        Agile programming.")))

(defun make-features-page ()
  (let (features titles)
    (macrolet ((add-feature (title &body body)
                   `(progn
                      (push (with-html-to-string
                               (:a :name ,(attributize-name title) (:h3 ,title))
                              ,@body)
                            features)
                      (push ,title titles))))
      (flet ((render-anchor-table ()
               (with-html
                 (:ul :class "anchors"
                   (mapcar (lambda (title)
                             (htm
                               (:li
                                 (:a :href (format nil "#~A" (attributize-name title))
                                     (esc title)))))
                           (nreverse titles)))))
             (render-content ()
               (mapcar (f_ (with-html (str _))) (nreverse features))))

        (make-page "Features"

          (add-feature "Think and code in high-level abstractions"
            (:p "A web page in Weblocks consists of building blocks called "
                (:em "widgets") ". Every widget knows how to render itself
                and keeps it state between requests")
            (:p "This simplifies information book-keeping and code re-use."))

          (add-feature "Newbie-friendly"
            (:p "We try hard to make Weblocks easy to use and install.")
            (:p "Our community helps beginners to get past any obstacles
                they might face."))

          (add-feature "Create multiple views of an object"
            (:p "Weblocks view language lets you specify a view of an object
                in a declarative manner. You can build customized forms and tables
                with only a few lines of code."))

          (add-feature "Fully extensible"
            (:p "Web applications are customized pieces of software.
                Weblocks helps you adapting it by offering an object-oriented
                multiple dispatch interface:")
            (:ul
              (:li "Every generic function is a hook which you can customize
                  using " (:code ":BEFORE") ", " (:code ":AFTER") " and " (:code ":AROUND")
                  " methods.")
              (:li "Every widget written by others may be specialized for your needs
              as well.")))

          (add-feature "Powerful modular dispatcher"
            (:p "User-defined Dispatchers based on string and regex matchers are present
                in every major web framework.")
            (:p "The usual way to go about it is having a centralized dispatcher definition
                (usually declarative or functional). Weblocks takes it one step further
                and offers completely modular and customizable dispatchers that can consume
                any amount of URI parts and invoke other dispatchers.")
            (:p "Additionally the host name and the URI prefix can dispatch
                to different applications."))

          (add-feature "Macros: complete realization of DRY"
            (:p (:em "Don’t Repeat Yourself") " is one of the core principles of
                Agile Development.")
            (:p "Languages without macros or with insufficiently advanced
                macros cannot avoid large amounts of code redundancy. By relying
                on Common Lisp Weblocks offers the user the full power
                of code transformation."))

          (add-feature "Harness the power of continuations"
            (:p "Widget continuations let your users use your site in
                a highly flexible manner.")
            (:p "You can have multiple parts of a page going into
                different directions simultaneously. Weblocks does
                all the book-keeping for you.")
            (:p "The best thing is: you don’t even need to know
                anything about continuations. Just use the high-level
                API provided by Weblocks to direct your control flow."))

          (add-feature "Thin Javascript layer"
            (:p "Thanks to Weblocks' thin Javascript layer your content degrades gracefully
                on clients that don't have Javascript enabled.")
            (:p "Features like fully sortable tables use AJAX when available but
                also offer the same user experience without AJAX by making a normal
                request.")
            (:p "All this happens automatically so you don't have to worry
                about it."))
    
          (render-anchor-table)
          (render-content))))))

(defun make-installation-page ()
  (make-page "Getting started"
    (:p "There are several ways to install Common Lisp and Weblocks.")
    (:h3 "Using Quicklisp")
    (:p (:a :href "http://www.quicklisp.org/" "Quicklisp")
        " is the easiest way to get started with Weblocks.")
    (:p "Perform the following steps:")
    (:ol
      (:li (:strong "Install SBCL or CCL."))
      (:li (:strong "Run SBCL or CCL and load Quicklisp")
           ": this is described on the Quicklisp page.")
      (:li (:strong "Install Weblocks")
           ": this is described on the Quicklisp page as well.")
      (:li (:strong "Load the demo") ": "
           (:pre "* (asdf:oos 'asdf:load-op 'weblocks-demo)"))
      (:li (:strong "Start the demo") ": "
           (:pre "* (weblocks-demo:start-weblocks-demo :port 3455)")
           " (replace 3455 with some port that is currently not in use on your system)")
      (:li (:strong "Check out the demo") " by pointing your browser at "
           (:pre "http://localhost:3455/weblocks-demo"))
      (:li "Use the demo as a starting point for " (:strong "your own application")
           " or generate a new base application named NAME in an existing directory DIR"
           " by issuing"
           (:pre "* (wop:make-app 'NAME \"DIR\")")))

    (:h3 "Installer script")
    (:p (:a :href "http://leftrightfold.com/" "Aaron Feng") " wrote a shell script"
          " that lets you start a new Weblocks project from scratch; all you need is"
          " an installed SBCL.")
    (:p "Get it here: " (:a :href "http://github.com/aaronfeng/weblocks-install/"
                            "http://github.com/aaronfeng/weblocks-install/"))

    (:h3 "Manual setup")
    (:p "We have repository at Github - "
     (:a :href "https://github.com/skypher/weblocks" "https://github.com/skypher/weblocks"))
    (:p "Saikat Chakrabarti has written a "
        (:a :href "http://slg.posterous.com/installing-weblocks" "step-by-step tutorial on
            setting up SBCL and Weblocks from scratch on Darwin."))))

(defun make-faq-page ()
  (make-page "FAQ"))

(defun make-documentation-page ()
  (make-page "Documentation"
    (:h3 "Tutorials and blog posts")
    (:p "Some introductory material can be found on the old "
        (:a :href "http://trac.common-lisp.net/cl-weblocks/wiki/Tutorials" "Weblocks Trac wiki")
        ". Some of these are slightly outdated and need some bits changed to make them
        work with the latest Weblocks code, though.")

    (:h3 "User guide")
    (:p "An early manual draft is " (:a :href "http://viridian-project.de/~sky/user-guide.stx.html" "available") ".")
    (:p "Additionally please use the "
        (:a :href "http://trac.common-lisp.net/cl-weblocks/wiki/UserManual" "quick guide")
        " in conjunction with community support and the source code documentation.")
    (:p "The " (:a :href "https://github.com/skypher/weblocks/tree/master/test" "tests")
        " and the "
        (:a :href "https://github.com/skypher/weblocks/tree/master/examples" "examples")
        " are also helpful.")

    (:h3 "API documentation")
    (:p "The latest auto-generated API documentation for the stable branch
        can be found at " (:a :href *doc-uri*
                                    (esc *doc-uri*)))
             
    (:h3 "Development process")
    (:dl
      (:dt "Submitting patches, working with the repositories")
      (:dd (:a :href "http://trac.common-lisp.net/cl-weblocks/wiki/WeblocksDevelopment"
               "http://trac.common-lisp.net/cl-weblocks/wiki/WeblocksDevelopment"))
      (:dt "Working with the test framework")
      (:dd (:a :href "http://groups.google.com/group/weblocks/msg/b25cbcd1398a91cc"
               "http://groups.google.com/group/weblocks/msg/b25cbcd1398a91cc")))))

(defun make-community-page ()
  (make-page "Community"
    (:h3 "Weblocks")
    (:p "The " (:a :href "http://groups.google.com/group/weblocks/" "Weblocks Group")
        " is the central place to get help and discuss development of Weblocks.")
    (:p "You can get free support and talk about bugs and features there.")
;    (:p "For professional support please contact "
;        (:a :href "mailto:polzer@stardawn.org" "Leslie P. Polzer") ".")

    (:h3 "Common Lisp")
    (:h4 "LispForum")
    (:p (:a :href "http://www.lispforum.com/" "LispForum") " is a suitable place
        for beginners to get help with Common Lisp problems and questions.")
    
    (:h4 "comp.lang.lisp")
    (:p "This newsgroup is the classic place to discuss all things pertaining to
        Common Lisp. You can access it with a newsreader or "
        (:a :href "http://groups.google.com/group/comp.lang.lisp" "via Google Groups") ".")
    (:p "It's not for the faint of heart, however.")))

(defmethod page-title ((app weblocks-site))
  (declare (special *current-page-description*))
  (format nil "Weblocks: ~A" (or *current-page-description* "")))

(defun init-user-session (root-widget)
  (setf (widget-prefix-fn root-widget) 'render-header)
  (setf (widget-children root-widget)
        (list (make-navigation "Main"
			       :navigation-class 'site-main-navigation
                               "Welcome" (make-welcome-page)
                               "Features" (make-features-page)
                               "Installation" (make-installation-page)
                               ;"FAQ" (make-faq-page)
                               "Documentation" (make-documentation-page)
                               "Community" (make-community-page)))))

(defun render-header (&rest args)
  (declare (ignore args))
  (with-html
    (:div :class "container-fluid"
	  (:div :class "row-fluid"
		:style "margin-top: 10px; margin-bottom: 20px"
		(:img :src "/pub/images/page/header.png")))))

(defwidget site-main-navigation (navigation)
  ())

(defmethod render-navigation-menu ((nav site-main-navigation) &rest args)
  (declare (ignore args))
  (call-next-method)
  (with-html
    (:div :style "background: #435781; margin-top: 20px; -webkit-border-radius: 6px; -moz-border-radius: 6px; border-radius: 6px;"
	  (:img :src "/pub/images/menu/lisp-logo.png"))))

(defmethod weblocks::render-navigation-styled :around ((nav navigation) (style-kind (eql ':sidebar)) (style-pos (eql ':left)) &rest args)
  (with-html
    (:div :class "container-fluid"
     (:div :class "row-fluid"
      (:div :class "nav-sidebar-menu span3"
       (apply #'render-navigation-menu nav :menu-args
              '(:list-class "nav nav-list") args))
      (:div :class "nav-sidebar-body span8"
       (apply #'weblocks::render-navigation-body nav args))))))
