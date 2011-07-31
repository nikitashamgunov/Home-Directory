;; -*- emacs-lisp -*-

;; please uncomment these two lines to your personal ~/.emacs file:
;;(defvar master-dir (getenv "ADMIN_SCRIPTS"))
;;(load-library (concat master-dir "/master.emacs"))
;; this keeps you up to date with the latest master.emacs changes; if you opt
;; not to, you'll be on your own staying up to date with general changes

;; Find emacs-packages
(add-to-list 'load-path (concat master-dir "/emacs-packages"))

;; Have highlighting all the time
(global-font-lock-mode 1)

;; use spaces, not tabs for indenting
(setq-default indent-tabs-mode nil)

;; git (try `git-blame-mode' and `(global-set-key "\C-xxb" 'git-blame-mode)')
(require 'git)
(setq git-blame-log-message-format "format:%an (%ar): %s (%h) ")
(autoload 'git-blame-mode "git-blame"
  "Minor mode for incremental blame for Git." t)

;; PHP mode for .phpt files
(autoload 'php-mode "php-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.phpt$" . php-mode))
                              auto-mode-alist))
(autoload 'xhp-mode "xhp-mode"
  "Major mode for editing PHP code including XHP support." t)

;; Set PHP mode based on the #! line
(add-to-list 'interpreter-mode-alist '("php" . php-mode))

;; Javascript mode for .js files
(autoload 'javascript-mode "javascript-mode" nil t nil)
(setq auto-mode-alist (append '(("\\.js$" . javascript-mode))
                              auto-mode-alist))
(require 'highlight-80+)
(add-hook 'javascript-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            ))

;; Thrift mode for .thrift files
(autoload 'thrift-mode "thrift" nil t nil)
(setq auto-mode-alist (append '(("\\.thrift$" . thrift-mode))
                              auto-mode-alist))

(if (locate-library "python")
    (require 'python)) ;; python mode available in emacs >= 22

(add-hook 'python-mode-hook
          (lambda ()
            (highlight-80+-mode t)
            ))

;; python mode for SConscript and SConstruct files
(setq auto-mode-alist (cons '("\\SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\SConscript" . python-mode) auto-mode-alist))

;; python mode for TARGETS files
(setq auto-mode-alist (cons '("\\/TARGETS$" . python-mode) auto-mode-alist))

;; d mode
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(add-to-list 'auto-mode-alist '("\\.d[i]?\\'" . d-mode))

;; show trailing whitespace ...
(set-face-background 'trailing-whitespace "#900000")
(setq-default show-trailing-whitespace t)
;; ... and terminate with extreme prejudice
(if (fboundp 'delete-trailing-whitespace)
    (add-hook 'write-file-hooks 'delete-trailing-whitespace)
)

;;=============================================================
;; SUBVERSION COMMIT HOOK
;;=============================================================
;; type "M-u" to insert svn commit template
;; (global-set-key "\M-u"
;;                 '(lambda ()
;;                    "Inserting Subversion template"
;;                    (interactive)
;;                    (insert-file-contents  (concat master-dir "/templates/svn-commit-template.txt"))
;;                    ))

;; Automatic insertion of template when entering subversion commit mode
(add-hook 'svncommit-mode-hook
          (function (lambda ()
                      "Inserting Subversion template"
                      (interactive)
                      (insert-file-contents  (concat master-dir "/templates/svn-commit-template.txt"))
                      )))


(define-derived-mode svncommit-mode
  text-mode
  "Subversion commit"
  "So you think you can commit code huh!")

(setq auto-mode-alist (append '(("svn-commit" . svncommit-mode))
                              auto-mode-alist))


;;=========================================================
;;PHP Doc Function template
;;========================================================
(defun php-doc ()
    "Inserts a phpdoc function template"
    (interactive)
    (insert-file-contents (concat master-dir "/templates/php-doc-template.txt")))


;;=========================================================
;;C++ Function documentation template
;;========================================================
(defun c++-doc ()
    "Inserts a C++ doc function template"
    (interactive)
    (insert-file-contents (concat master-dir "/templates/c++-doc-template.txt")))

;;=========================================================
;;Add a "--read-only" command line option
;;========================================================
(setq command-switch-alist
      (cons '("--read-only" . find-file-read-only-command-line-arg) ; "-R"
            command-switch-alist))

(defun find-file-read-only-command-line-arg (switch)
  "Visit next command line argument (after SWITCH) as a read-only file."
  ;; (prog1 (car x) (setq x (cdr x))) == (pop x):
  (find-file-read-only (prog1 (car command-line-args-left)
                         (setq command-line-args-left
                               (cdr command-line-args-left)))))

;;=========================================================
;;PHP Indentation Style
;;========================================================
(defconst fb-php-style
  '((c-basic-offset . 4)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        )))
  "Facebook's PHP Programming style"
)
(c-add-style "fb-php-style" fb-php-style)

(require 'highlight-80+)
(add-hook 'php-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ))

(add-hook 'xhp-mode-hook
          (lambda ()
            (c-set-style "fb-php-style")
            (highlight-80+-mode t)
            ))

;;=========================================================
;;C, C++, Objective-C Indentation Style
;;========================================================
(require 'fb-objc)

(defconst fb-c-style
  '((c-basic-offset . 4)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        (innamespace . 0)
                        (member-init-intro . ++)
                        (inher-intro . ++)
                        (objc-method-args-cont . fb-c-lineup-ObjC-method-args)
                        (objc-method-call-cont
                         (fb-c-lineup-ObjC-method-call-colons
                          fb-c-lineup-ObjC-method-call +))
                        )))
  "Facebook's C, C++, and Objective-C programming style"
)
(c-add-style "fb-c-style" fb-c-style)

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "fb-c-style")
            (highlight-80+-mode t)
            ))

;;=========================================================
;;Java Indentation Style
;;========================================================
(defconst fb-java-style
  '((c-basic-offset . 4)
    (c-offsets-alist . (
                        (arglist-intro . +)
                        (case-label . +)
                        (arglist-close . c-lineup-close-paren)
                        )))
  "Facebook's Java programming style"
)
(c-add-style "fb-java-style" fb-java-style)

(add-hook 'java-mode-hook
          (lambda ()
            (c-set-style "fb-java-style")
            (highlight-80+-mode t)
            ))

;; Automatically select the appropriate mode based on matching the
;; text at the beginning of the file.
(if (boundp 'magic-mode-alist)
    (setq magic-mode-alist
          (append (list
                   '("\\(.\\|\n\\)*\n@implementation" . objc-mode)
                   '("\\(.\\|\n\\)*\n@interface" . objc-mode)
                   '("\\(.\\|\n\\)*\n@protocol" . objc-mode)
                   '("\\(.\\|\n\\)*\nnamespace.*{" . c++-mode)
                   '("<\\?php\\s " . php-mode))
                  magic-mode-alist))
)

;; Navigate to the file providing the Haste component name found under
;; the cursor.  Bound by default to C-x C-h
;;
;; @author ahupp

(defun fb-is-www-root (dirname)
  (file-exists-p (expand-file-name "last_min_rev.txt" dirname)))

(defun fb-parent-directory (filename)
  (file-name-directory (directory-file-name filename)))

(defun fb-find-www-root (filename)
  (cond
   ((equal filename "/") nil)
   ((fb-is-www-root filename) filename)
   (t (fb-find-www-root (fb-parent-directory filename)))))

(defun haste-file-for-component (filename component)
  (interactive)
  (let* ((www-root (fb-find-www-root filename))
         (cmd-to-run (concat www-root "/scripts/file_for_component.php " component)))
    (shell-command-to-string cmd-to-run)))

(defun haste-find-component (component)
  "Open the file that provides the haste component name found under the mouse cursor"
  (interactive
   ;; 'filename is kind of a weird choice but symbol doesn't work in php-mode
   (let* ((def (thing-at-point 'filename))
          (input (read-string "Open component: " def)))
     (list input)))
  (let* ((file-path (haste-file-for-component buffer-file-name component)))
    (if (> (length file-path) 0)
        (find-file file-path)
      (message "unknown component: %s" component))))

(add-hook 'php-mode-hook
          (lambda ()
            (define-key php-mode-map "\C-x\C-h" 'haste-find-component)))

;; unique buffer names using path
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; from mst-arch.el.
(defmacro with-working-directory (dir &rest body)
  (let ((old (gensym)))
    `(let ((,old default-directory))
       (cd ,dir)
       (unwind-protect
           (progn ,@body)
         (cd ,old)))))

(put 'with-working-directory 'lisp-indent-function 1)

;; tbgX commands to search the codebase
(autoload 'tbgr "tbgX" nil t nil)
(autoload 'tbgs "tbgX" nil t nil)
(autoload 'fbgr "tbgX" nil t nil)
(autoload 'fbgs "tbgX" nil t nil)
(autoload 'pbgr "tbgX" nil t nil)
(autoload 'pbgs "tbgX" nil t nil)
(autoload 'gg "tbgX" nil t nil)
