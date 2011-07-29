;;; $Id$
;;; Copyright (C) 2009 Facebook Inc.
;;; Author: Greg J. Badros <badros@facebook.com
;;;
;;; Simple interactive commands for [fpt][gr] e.g., tbgr, tbgs, fbgr,
;;; etc.  Modeled after grep major mode, which itself uses compilation
;;; minor mode.
;;;
;;; Tested using GNU Emacs 22.3.2 on Linux.
;;;
;;; For customization, see the variables in grep.el (or
;;; describe-variable on grep-*)
;;;
;;; For keybindings, see compile.el.
;;;
;;; TODO(badros): consider specializations of those grep customization
;;; variables for just this mode.
;;;
;;; TODO(badros): factor the functions out better by defining a macro
;;; that can define each of the six functions (eliminate cut & paste).


(require 'grep)
(require 'compile)

(defgroup facebook-grep-utils nil
  "Various options that affect the tbgX, fbgX and pbgX commands."
  :group 'tools
  :group 'processes)

(defcustom tbgX-compilation-search-path (list (expand-file-name "~"))
  "Lists of search paths to try prepending to the output of a tbgX command like \\[tbgs]."
  :type '(repeat directory)
  :group 'facebook-grep-utils)
(defcustom fbgX-compilation-search-path (list (expand-file-name "~"))
  "Lists of search paths to try prepending to the output of a fbgX command like \\[fbgs]."
  :type '(repeat directory)
  :group 'facebook-grep-utils)
(defcustom pbgX-compilation-search-path (list (expand-file-name "~"))
  "Lists of search paths to try prepending to the output of a pbgX command like \\[pbgs]."
  :type '(repeat directory)
  :group 'facebook-grep-utils)
(defcustom xbgX-common-args "--smartdir"
  "arguement string to pass to tbgX, fbgX or pbgX commands"
  :type 'string
  :group 'facebook-grep-utils)


;; For testing:
;; (setq compilation-search-path tbgX-compilation-search-path)
;; (setq compilation-search-path nil)

(defun tbgX-do-grep (args)
  "Run grep with args, doing quoting"
  (grep (concat grep-command " " (shell-quote-argument args))))

;; inspired by igrep.el's igrep-read-regex
(defun tbgX-read-string (prompt)
  "Read and return a string from the minibuffer.
PROMPT is used as the prompt."
  (let* ((symbol (symbol-at-point))
         (default-value (if symbol (symbol-name symbol) nil)))
    (list
     (read-string (concat prompt ": ") default-value 'tbgX-history))))


(defun tbgr (regex)
  "Run tbgr, like grep, but using tbgX-compilation-search-path.
tbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches tfb-trunk in SVN for a regular expression."
  (interactive (tbgX-read-string "stbgr (regexp)"))
  (message "Using %s" regex)
  (let ((grep-use-null-device nil)
        (grep-command (concat "tbgr " xbgX-common-args)))
    (tbgX-do-grep regex)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) tbgX-compilation-search-path))))

(defun tbgs (string)
  "Run tbgs, like grep, but using tbgX-compilation-search-path.
tbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches tfb-trunk in SVN for a string."
  (interactive (tbgX-read-string "stbgs (string)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat "tbgs " xbgX-common-args)))
    (tbgX-do-grep string)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) tbgX-compilation-search-path))))


(defun fbgr (regex)
  "Run fbgr, like grep, but using fbgX-compilation-search-path.
fbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches fbcode-trunk in SVN for a regular expression."
  (interactive (tbgX-read-string "sfbgr (regexp)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat "fbgr " xbgX-common-args)))
    (tbgX-do-grep regex)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) fbgX-compilation-search-path))))


(defun fbgs (string)
  "Run fbgs, like grep, but using fbgX-compilation-search-path.
fbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches fbcode-trunk in SVN for a string."
  (interactive (tbgX-read-string "sfbgs (string)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat "fbgs " xbgX-common-args)))
    (tbgX-do-grep string)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) fbgX-compilation-search-path))))


(defun pbgr (regex)
  "Run pbgr, like grep, but using pbgX-compilation-search-path.
pbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches projects in SVN for a regular expression."
  (interactive (tbgX-read-string "spbgr (regexp)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat "pbgs " xbgX-common-args)))
    (tbgX-do-grep regex)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) pbgX-compilation-search-path))))

(defun pbgs (string)
  "Run pbgs, like grep, but using pbgX-compilation-search-path.
pbgX-compilation-search-path should contain a list of parents of
sandbox directories, e.g., (list (expand-file-name \"~\")) if you
have a sandbox in ~/www.
Searches projects in SVN for a string."
  (interactive (tbgX-read-string "spbgs (string)"))
  (let ((grep-use-null-device nil)
        (grep-command (concat "pbgs " xbgX-common-args)))
    (tbgX-do-grep string)
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) pbgX-compilation-search-path))))

(defcustom gg-args ""
  "argument string to pass to git grep"
  :type 'string
  :group 'facebook-grep-utils)

(defun git-root ()
  (let ((dir (fb-parent-directory
              (shell-command-to-string "git rev-parse --git-dir"))))
    (if (not dir)
        (error "git root dir not found - is this a git repo?")
      dir)))

(defun gg (regex)
  "run git grep interactively"
  (interactive (tbgX-read-string "regex"))
  (run-git-grep
   (concat "git --no-pager grep -n --full-name " gg-args)
   regex))

(defun run-git-grep (cmd regex)
  (let ((grep-use-null-device nil)
        (grep-command cmd)
        (root-dir (git-root)))
    (with-working-directory root-dir (tbgX-do-grep regex))
    (save-window-excursion
      (switch-to-buffer "*grep*")
      (set (make-local-variable 'compilation-search-path) (list root-dir)))))

(defcustom sgrep-path "sgrep_php"
  "path to sgrep"
  :type 'string
  :group 'facebook-grep-utils)

(defun fb-argmax (ffun lst)
  (if (not lst) nil
    (reduce (lambda (lhs rhs)
              (let ((lhsval (funcall ffun lhs))
                    (rhsval (funcall ffun rhs)))
                (if (> lhsval rhsval) lhs rhs)))
            (cdr lst)
            :initial-value (car lst))))

(defun sg (pattern)
  "run sgrep interactively"
  (interactive (tbgX-read-string "pattern"))
  ;; heuristic to figure out what the inteded filter string is.  Take
  ;; the longest identifier-like thing
  (let ((prefilter (fb-argmax 'length (split-string pattern "[^a-zA-Z_0-9]+"))))
    (run-git-grep
     (concat "git grep --null -l " (shell-quote-argument prefilter)
             " -- \\*.php | xargs --null " sgrep-path " -emacs -e ")
       pattern)))

(provide 'tbgX)
