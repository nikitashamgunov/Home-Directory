;;; $Id: elisp-insert.el,v 1.3 1997/04/03 20:39:02 gjb Exp $
;;; Copyright (C) 2009 Facebook Inc.
;;; Author: Greg J. Badros <badros@facebook.com
;;;
;;; browse-url-append-to-file.el - Support appending to a file every time a URL is visited.
;;;
;;; Every time the an emacs command wants to visit a URL, just
;;; append the URL to a filename.  Assumes another process is
;;; tailing that file (possibly via ssh from another machine)
;;; and visiting the URLs written to that file.
;;;
;;; See ./tail-f-urls script for example such script.
;;;
;;; To use, simply:
;;; (autoload 'browse-url-append-to-file "browse-url-append-to-file")
;;; (setq browse-url-browser-function 'browse-url-append-to-file)
;;;
;;; BE SURE THAT $HOME/private/.browse-url-append-to-file-urls.txt IS READABLE ONLY BY YOU
;;; (or use another browse-url-append-to-file-destination-file location that is)
;;;
;;; You may need to:
;;;    mkdir ~/private
;;;    chmod go-rx private


(defconst browse-url-append-to-file-version (substring "$Revision: 1.3 $" 11 -2)
  "$Id: elisp-insert.el,v 1.3 1997/04/03 20:39:02 gjb Exp $

Report bugs to: Greg Badros badros@facebook.com")

(require 'browse-url)

;;; IMPOR
(defvar browse-url-append-to-file-destination-file
  (concat (getenv "HOME") "/private/.browse-url-append-to-file-urls.txt"))

;; (setq url "http://www.facebook.com/")
;; (setq maybe-new-window "'::new-window\n'")
;; (setq maybe-new-window "")
(defun browse-url-append-to-file (url &optional new-window)
  "Append the URL to a file so that another process can visit that URL.
Set variable browse-url-append-to-file-destination-file to specify
what file to write to, and see tail-f-urls for the shell script to
tail the file."
  (interactive (browse-url-interactive-arg "browse-url-append-to-file: "))
  (message "Writing to %s" browse-url-append-to-file-destination-file)
  (let ((maybe-new-window (if new-window
                              "'::new-window\n'" "")))
    (apply #'start-process (concat "browse-url-append-to-file " url)
           "*scratch*" "sh"
           (list "-c" (concat "echo " maybe-new-window
                              (shell-quote-argument url)
                              " >>" browse-url-append-to-file-destination-file)))))

(provide 'browse-url-append-to-file)
