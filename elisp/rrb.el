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
  "Return list containing all elements of PREC is true"
  (cond ((eq list nil) nil)
	((funcall prec (car list)) (cons (car list)
					 (rrb-find-all prec (cdr list))))
	(t (rrb-find-all prec (cdr list)))))

(defun rrb-buffer-map-line (prec)
  "Return list of (PREC line) for each line in current buffer"
  (goto-char (point-max))
  (let ((result nil))
    (while (not (bobp))
      (setq result (cons (funcall prec (thing-at-point 'line)) result))
      (forward-line -1))
    result))
      

(defun rrb-chop (str)
  "Return chopped string"
  (substring str 0 -1))

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
    (erase-buffer)
    (rrb-insert-input-string rrb-main-buffer)
    (mapcar (lambda (buffer)
	      (unless (eq buffer rrb-main-buffer)
		(rrb-insert-input-string buffer)))
	    (rrb-all-ruby-script-buffer))
    (insert rrb-io-terminator)
    (insert rrb-io-splitter)))

(defun rrb-clean-output-buffer ()
  "Clean temporary buffers"
  (set-buffer rrb-output-buffer)
  (erase-buffer)
  (set-buffer rrb-error-buffer)
  (erase-buffer))

(defun rrb-output-to-buffer ()
  "Rewrite all ruby script buffer from \" *rrb-output\""
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (let ((list-top (split-string (buffer-string) rrb-io-splitter)))
      (while (not (string= (car list-top) rrb-io-terminator))
	(set-buffer (get-file-buffer (car list-top)))
	(erase-buffer)
	(insert (cadr list-top))
	(setq list-top (cddr list-top))))))

(defun rrb-output-to-error-buffer (filename)
  "load FILENAME to \" *rrb-error\""
  (save-current-buffer
    (set-buffer rrb-error-buffer)
    (insert-file-contents filename)))

(defun rrb-error-message ()
  "Get Error Message in \" *rrb-error\""
  (save-current-buffer
    (set-buffer rrb-error-buffer)
    (goto-char (point-min))
    (substring (thing-at-point 'line) 0 -1)))
    
(defun rrb-do-refactoring (&rest args)
  "Do refactoring"
  (if (/= (apply 'rrb-run-process "rrb" args) 0)
      (error "fail to refactor: %s" (rrb-error-message)))
    (rrb-output-to-buffer))

(defun rrb-make-temp-name (base)
  (make-temp-name (expand-file-name base temporary-file-directory)))

(defun rrb-run-process (command &rest args)
  "Run COMMAND and return error code"
  (let ((error-code)
	(tmpfile (rrb-make-temp-name rrb-tmp-file-base)))
    (rrb-clean-output-buffer)
    (set-buffer rrb-input-buffer)
    (setq error-code (apply 'call-process-region
			    (point-min) (point-max)
			    command
			    nil
			    (list rrb-output-buffer tmpfile)
			    nil
			    `(,@args "--stdin-stdout")))
    (rrb-output-to-error-buffer tmpfile)
    (delete-file tmpfile)
    error-code))

(defun rrb-complist-method ()
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (rrb-buffer-map-line (lambda (line) (cons (car (split-string line ";")) nil)))))

(defun rrb-complist-local-var (method)
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (goto-char (point-min))
    (when (search-forward method nil t)
      (forward-char)
      (mapcar 'list
	      (split-string (buffer-substring (point) (point-at-eol)) ",")))))

(defun rrb-comp-read-rename-local-variable ()
  "Completion read for Rename local variable"
  (when (/= (rrb-run-process "rrb_compinfo" "--methods-local-vars") 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((method (completing-read "Refactored method: " (rrb-complist-method))))
    (list method
 	  (completing-read "Old variable: " (rrb-complist-local-var method))
 	  (read-from-minibuffer "New variable: "))))

(defun rrb-rename-local-variable (method old-var new-var)
  "Refactor code: rename local variable"
  (interactive (progn
		 (rrb-setup-input-buffer)
		 (rrb-comp-read-rename-local-variable)))
  (save-excursion
    (rrb-do-refactoring "--rename-local-variable" method old-var new-var)))

(defun rrb-rename-method-all (old-method new-method)
  "Refactor code: rename method all old method as new"
  (interactive "sOld method: \nsNew method: ")
  (save-excursion
    (rrb-setup-input-buffer)
    (rrb-do-refactoring "--rename-method-all" old-method new-method)))