;;; rrb.el - Ruby Refactoring Brower emacs interface

;; Copyright (C) 2003 OHBAYASHI Ippei

;; Author: OHBAYASHI Ippei <ohai@kmc.gr.jp>
;; Maintainer: OHBAYASHI Ippei <ohai@kmc.gr.jp>

;;; Code:

;;; Customizable variables
(defvar rrb-ruby-file-name-regexp "^.*\\.rb$"
  "*Regular expression matched ruby script file name")

(defvar rrb-tmp-file-base "rrblog"
  "*Base file name to use error log file")

;;; Internal variables
(defvar rrb-main-buffer nil
  "Ruby main script buffer")

(defconst rrb-io-splitter "\C-a")
(defconst rrb-io-terminator "-- END --")

(defvar rrb-input-buffer (get-buffer-create " *rrb-input*"))
(defvar rrb-output-buffer (get-buffer-create " *rrb-output*"))
(defvar rrb-error-buffer (get-buffer-create " *rrb-error*"))

;;; Utility functions
(defun rrb-find-all (prec list)
  (cond ((eq list nil) nil)
	((funcall prec (car list)) (cons (car list)
					 (rrb-find-all prec (cdr list))))
	(t (rrb-find-all prec (cdr list)))))
  
;;; Main functions
(defun rrb-set-main-script-buffer (buffer-name)
  "Set ruby main script buffer"
  (interactive "*bSelect main script: ")
  (setq rrb-main-buffer (get-buffer buffer-name)))

(defun rrb-all-ruby-script-buffer ()
  "Return all ruby script buffer,`ruby script buffer' means `its file name
matches with rrb-ruby-file-name-regexp'"
  (rrb-find-all (lambda (buffer)
		  (and (buffer-file-name buffer) 
		       (string-match rrb-ruby-file-name-regexp
				     (buffer-file-name buffer))))
		(buffer-list)))

(defun rrb-insert-input-string (src-buffer)
  (insert (buffer-file-name src-buffer))
  (insert rrb-io-splitter)
  (insert-buffer-substring src-buffer)
  (insert rrb-io-splitter))

(defun rrb-setup-input-buffer ()
  "Generate input string on \" *rrb-input*\""
  (save-excursion
    (set-buffer rrb-input-buffer)
    (rrb-insert-input-string rrb-main-buffer)
    (mapcar (lambda (buffer)
	      (if (eq buffer rrb-main-buffer)
		  nil
		(rrb-insert-input-string buffer)))
	    (rrb-all-ruby-script-buffer))
    (insert rrb-io-terminator)
    (insert rrb-io-splitter)))

(defun rrb-clean-buffer ()
  "Clean temporary buffers"
  (set-buffer rrb-input-buffer)
  (erase-buffer)
  (set-buffer rrb-output-buffer)
  (erase-buffer)
  (set-buffer rrb-error-buffer)
  (erase-buffer))


(defun rrb-output-to-buffer ()
  "Rewrite all ruby script buffer from \" *rrb-output\""
  (save-excursion
    (set-buffer rrb-output-buffer)
    (let ((list-top (split-string (buffer-string) rrb-io-splitter)))
      (while (not (string= (car list-top) rrb-io-terminator))
	(set-buffer (get-file-buffer (car list-top)))
	(erase-buffer)
	(insert (cadr list-top))
	(setq list-top (cddr list-top))))))

(defun rrb-rename-local-variable (method old-var new-var)
  "Refactor code: rename local variable"
  (interactive "sRefactored method: \nsOld variable: \nsNew variable: ")
  (save-excursion
    (let ((error-code))
      (rrb-clean-buffer)
      (rrb-setup-input-buffer)
      (set-buffer rrb-input-buffer)
      (setq error-code (call-process-region (point-min) (point-max)
					    "rrb"
					    nil
					    (list rrb-output-buffer
						  "/tmp/rrb_log")
					    nil
					    "--rename-local-variable"
					    method old-var new-var
					    "--stdin-stdout"))
      (if (/= error-code 0)
	  (message "rrb: fail to refactor")
	(rrb-output-to-buffer)))))


			   
      
  
  