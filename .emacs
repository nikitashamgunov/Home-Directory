(defvar master-dir "~")
(load-library (concat master-dir "/master.emacs"))


(setq local-elisp-dir (expand-file-name "~/elisp/"))
(add-to-list 'load-path local-elisp-dir)

(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

;;(global-auto-revert-mode nil)
;; (add-to-list 'load-path (expand-file-name "~/local/auto-complete"))

;; (require 'auto-complete)
;; (require 'auto-complete-config)


;; (setq-default ac-sources '(ac-source-words-in-same-mode-buffers))
;; (add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))
;; (add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))
;; (global-auto-complete-mode t)
;; (set-face-background 'ac-candidate-face "lightgray")
;; (set-face-underline 'ac-candidate-face "darkgray")
;; (set-face-background 'ac-selection-face "steelblue")
;; (define-key ac-completing-map "\M-n" 'ac-next)
;; (define-key ac-completing-map "\M-p" 'ac-previous)
;; (setq ac-auto-start 2)
;; (setq ac-dwim t)
;; (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)



;; (load "hupp.el")
;; (load "hupp-local.el" t) ;; don't report error
(require 'tbgX)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(pair-mode-chars (quote (40 91 123 60 2219 96 34)))
 '(gg-args " -C5 ")
;; '(vc-handled-backends nil)
 )

(require 'tbgX)
;;(require 'flib-tools)
;;(require 'yasnippet-bundle)
;;(yas/initialize)
;;y(yas/load-directory "~/dotfiles/snippets")

;; There's something similar (but fancier) in vc-git.el: vc-git-grep
;; -I means don't search through binary files
(defcustom git-grep-switches "-I -n --ignore-case"
  "Switches to pass to `git  grep'."
  :type 'string)

(defun git-grep (command-args)
  (interactive
   (list (read-shell-command "Run git-grep (like this): "
                             (format "git --no-pager grep %s -e "
                                     git-grep-switches)
                             'git-grep-history)))
  (let ((grep-use-null-device nil))
    (grep command-args)))

(setq c-default-style "bsd")
(setq large-file-warning-threshold nil)
(setq-default tab-width 4)
(visit-tags-table "~/memsql/TAGS")
(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)
