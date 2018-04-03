;;;  -*- Mode: Lisp; -*-

(defpackage #:aima
  (:use #:common-lisp)
  (:import-from #:trivia #:match #:guard)
  (:import-from #:asdf
                #:aima-system-examples)
  (:export #:*aima-version*
           #:*aima-root*
           #:*aima-binary-type*
           #:*aima-system-names*
           #:def-aima-system
           #:add-aima-system
           #:aima-load-if-unloaded
           #:get-aima-system
           #:aima-system-parts
           #:aima-file
           #:operate-on-aima-system
           #:file-with-type
           #:mklist))

(in-package #:aima)

;;;; Vendor-Specific Customizations

#+Lucid (setq *warn-if-no-in-package* nil)

;; 6oct05 charley cox
;; print-structure symbol conflicts with existing symbol in Allegro Common
;; Graphics
#+Allegro (shadow 'print-structure)


;;;; A minimal facility for defining systems of files

(defparameter *aima-root* (truename ".") ; <<<<<<<< Edit this <<<<<<
  "The root directory where the code is stored.")

#-asdf
(defparameter *aima-binary-type*
  (first (list   ; <<<<<<<<<<<<<<<<<<<< Edit this <<<<<<<<<
          #+Lispworks system::*binary-file-type*
          #+Lucid (first lucid::*load-binary-pathname-types*)
          #+Allegro excl:*fasl-default-type*
          #+(or AKCL KCL ECL) "o"
          #+CMU "sparcf"
          #+CLISP "fas"
          #+(or SBCL CCL) "fasl"
          #+ABCL "abcl"))
  "If calling aima-load loads your source files and not your compiled
  binary files, insert the file type for your binaries before the <<<<
  and load systems with (aima-load-binary NAME).")

(defconstant +aima-version+ '|0.99 AIMA Code, Appomattox Version, 09-Apr-2002|)

#-asdf
(defparameter *aima-system-names* nil
  "A list of names of the systems that have been defined.")

#-asdf
(defstruct aima-system
  name
  (requires nil)
  (doc "")
  (parts nil)
  (examples nil)
  (loaded? nil))

;;;; The Top-Level Functions:

#-asdf
(defmacro def-aima-system (name requires doc &body parts)
  "Define a system as a list of parts.  A part can be a string, which denotes
  a file name; or a symbol, which denotes a (sub)system name; or a list of the
  form (subdirectory / part...), which means the parts are in a subdirectory.
  The REQUIRES argument is a list of systems that must be loaded before this
  one.  Note that a documentation string is mandatory."
  `(add-aima-system :name ',name
                    :requires ',requires :doc ',doc :parts ',parts))

#-asdf
(defun aima-load (&optional (name 'all))
  "Load file(s), trying the system-dependent method first."
  (operate-on-aima-system name 'load-something))

#-asdf
(defun aima-load-binary (&optional (name 'all))
  "Load file(s), prefering binaries to source."
  (operate-on-aima-system name 'load-binary))

#-asdf
(defun aima-compile (&optional (name 'everything))
  "Compile (and load) the file or files that make up an AIMA system."
  (operate-on-aima-system name 'compile-load))

#-asdf
(defun aima-load-if-unloaded (name)
  (let ((system (get-aima-system name)))
    (unless (and system (aima-system-loaded? system))
      (aima-load system))
    system))

#+asdf
(defun aima-load-if-unloaded (name)
  (let ((system-name (format nil "aima-~(~A~)" (symbol-name name))))
    (asdf:load-system system-name)
    (asdf:find-system system-name)))

;;;; Support Functions

#-asdf
(defun add-aima-system (&key name requires doc parts examples)
  (pushnew name *aima-system-names*)
  (setf (get 'aima-system name)
        (make-aima-system :name name :examples examples
                          :requires requires :doc doc :parts parts)))

#+asdf
(defun add-aima-system (&key name requires doc parts examples)
  "In real, the system is already defined within ASDF realm.
   All we need here are examples list."
  (declare (ignore requires doc parts))
  (let ((system (asdf:find-system name)))
    (unless system
      (setf (aima-system-examples system) examples))
    system))

#+asdf
(defun aima-system-parts (system)
  (let* ((components (asdf:component-children system))
         (not-modules #'(lambda (m) (not (eq (class-name (class-of m))
                                      'asdf/component:module))))
         (modules (remove-if not-modules components)))
    (when system
      (map 'list
           #'(lambda (module) (make-symbol (slot-value module 'asdf::name)))
           modules))))

#-asdf
(defun get-aima-system (name)
  "Return the system with this name.  (If argument is a system, return it.)"
  (cond ((aima-system-p name) name)
        ((symbolp name) (get 'aima-system name))
        (t nil)))

#+asdf
(defun get-aima-system (name-or-system)
  "Return the system with this name.  (If argument is a system, return it.)"
  (match name-or-system
    ((guard s (or (symbolp s) (stringp s)))
     (asdf:find-system (format nil "aima-~(~A~)" name-or-system)))
    ((asdf/system:system)
     name-or-system)
    (_ nil)))


#-asdf
(defun operate-on-aima-system (part operation &key (path nil) (load t)
                                                   (directory-operation #'identity))
  "Perform the operation on the part (or system) and its subparts (if any).
  Reasonable operations are load, load-binary, compile-load, and echo.
  If LOAD is true, then load any required systems that are unloaded."
  (let (system)
    (cond
      ((stringp part) (funcall operation (aima-file part :path path)))
      ((and (consp part) (eq (second part) '/))
       (let* ((subdirectory (mklist (first part)))
              (new-path (append path subdirectory)))
         (funcall directory-operation new-path)
         (dolist (subpart (nthcdr 2 part))
           (operate-on-aima-system subpart operation :load load
                                                     :path new-path
                                                     :directory-operation directory-operation))))
      ((consp part)
       (dolist (subpart part)
         (operate-on-aima-system subpart operation :load load :path path
                                                   :directory-operation directory-operation)))
      ((setf system (get-aima-system part))
       ;; Load the required systems, then operate on the parts
       (when load (mapc #'aima-load-if-unloaded (aima-system-requires system)))
       (operate-on-aima-system (aima-system-parts system) operation
                               :load load :path path
                               :directory-operation directory-operation)
       (setf (aima-system-loaded? system) t))
      (t (warn "Unrecognized part: ~S in path ~A" part path)))))

#-asdf
(defun aima-file (name &key (type nil) (path nil))
  "Given a file name and maybe a file type and a relative path from the
  AIMA directory, return the right complete pathname."
  (make-pathname :name name :type type :defaults *aima-root*
                 :directory (append (pathname-directory *aima-root*)
                                    (mklist path))))
#-asdf
#-MCL ;; Macintosh Common Lisp already defines this function
(defun compile-load (file)
  "Compile file and then load it."
  ;; This could be made more sophisticated, to compile only when out of date.
  (compile-file (file-with-type file "lisp"))
  (load-binary file))

#-asdf
(defun load-binary (file)
  "Load file, trying the binary first, but loading the source if necessary."
  (load-something file '(binary nil "lisp")))

#-asdf
(defun load-something (file &optional (types '(nil binary "lisp")))
  "Try each of the types in turn until we get a file that loads.
  Complain if we can't find anything.  By default, try the system-dependent
  method first, then the binary, and finally the source (lisp) file."
  (dolist (type types (warn "Can't find file: ~A" file))
    (when (load (file-with-type file type) :if-does-not-exist nil)
      (return t))))

#-asdf
(defun file-with-type (file type)
  "Return a pathname with the given type."
  (if (null type)
      file
      (merge-pathnames
       (make-pathname :type (if (eq type 'binary) *aima-binary-type* type))
       file)))

#-asdf
(defun mklist (x)
  "If x is a list, return it; otherwise return a singleton list, (x)."
  (if (listp x) x (list x)))

;;; ----------------------------------------------------------------------
;;;; Definitions of Systems
;;; ----------------------------------------------------------------------

#-asdf
(progn
  (def-aima-system utilities ()
      "Basic functions that are loaded every time, and used by many other systems."
    ("utilities" / "utilities" "binary-tree" "queue" "cltl2" "test-utilities"))

  (def-aima-system agents (utilities)
      "Code from Part I: Agents and Environments"
    ("agents" / "test-agents"
              ("environments" / "basic-env" "grid-env" "vacuum" "wumpus")
              ("agents" / "agent" "vacuum" "wumpus")
              ("algorithms" / "grid")))

  (def-aima-system search (agents)
      "Code from Part II: Problem Solving and Search"
    ("search" / "test-search"
              ("algorithms" / "problems" "simple" "repeated"
                            "csp" "ida" "iterative" "sma" "minimax")
              ("environments" / "games" "prob-solve")
              ("domains" / "cannibals" "ttt" "cognac" "nqueens" "path-planning"
                         "puzzle8" "route-finding" "tsp" "vacuum")
              ("agents" / "ps-agents" "ttt-agent")))

  (def-aima-system logic (agents)
      "Code from Part III: Logic, Inference, and Knowledge Representation"
    ("logic" / "test-logic"
             ("algorithms" / "tell-ask" "unify" "normal" "prop" "horn" "fol" "infix")
             ("environments" / "shopping")))

  (def-aima-system planning ()
      "Code from Part IV: Planning and Acting"
    ("planning" / ))

  (def-aima-system uncertainty (agents)
      "Code from Part V: Uncertain Knowledge and Reasoning"
    ("uncertainty" / "test-uncertainty"
                   ("agents" / "mdp-agent")
                   ("domains" / "mdp" "4x3-mdp")
                   ("environments" / "mdp")
                   ("algorithms" / "dp" "stats")))

  (def-aima-system learning (uncertainty)
      "Code from Part VI: Learning"
    ("learning" / "test-learning"
                ("algorithms" / "inductive-learning" "learning-curves" "dtl" "dll"
                              "nn" "perceptron" "multilayer" "q-iteration")
                ("domains" / "restaurant-multivalued" "restaurant-real"
                           "restaurant-boolean" "majority-boolean" "ex-19-4-boolean"
                           "and-boolean" "xor-boolean" "4x3-passive-mdp")
                ("agents" / "passive-lms-learner" "passive-adp-learner"
                          "passive-td-learner" "active-adp-learner" "active-qi-learner"
                          "exploring-adp-learner" "exploring-tdq-learner")))

  (def-aima-system language (logic)
      "Code from Part VII, Chapters 22-23: Natural Language and Communication"
    ("language" / "test-language"
                ("algorithms" / "chart-parse")
                ("domains" / "grammars" )))

  (def-aima-system all ()
      "All systems except the utilities system, which is always already loaded"
    agents search logic planning uncertainty learning language)

  (def-aima-system everything ()
      "All the code, including the utilities"
    utilities all)

  (setf *aima-system-names* (nreverse *aima-system-names*)))

;;;; Always load the utilities

(when (string= "nIL" (symbol-name (read-from-string "nIL")))
  (cerror
   "Continue loading AIMA code."
   "Lisp reader is case-sensitive.  Some AIMA code may not work correctly."))

;;; push :CLOS into features list
#+(or sbcl ccl clisp abcl ecl allegro lispworks) (push :clos *features*)

#+nil (aima-load 'utilities)
