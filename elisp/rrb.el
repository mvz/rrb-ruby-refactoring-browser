;;; rrb.el -- Ruby Refactoring Brower emacs interface

;; Copyright (C) 2003 OHBAYASHI Ippei, YOSHIDA Yuichi, HARA Yutaka

;; Author: OHBAYASHI Ippei <ohai@kmc.gr.jp>
;;	YOSHIDA Yuichi <oxy@kmc.gr.jp>
;;	HARA Yutaka <yhara@kmc.gr.jp>
;; Keywords: ruby refactoring browser

;;; Code:

;;;; Customizable variables
(defvar rrb-ruby-file-name-regexp "^.*\\.rb$"
  "*Regular expression matched ruby script file name")
(defvar rrb-first-line-of-ruby-file-regexp "^#!.*ruby.*$"
  "*Regular expression matched first line of ruby script")

(defvar rrb-tmp-file-base "rrblog"
  "*Base file name to use error log file")

(defvar rrb-undo-file-base "rrbundo"
  "*Base file name for undo file")

(defvar rrb-undo-directory 
  (file-name-as-directory
   (make-temp-name
    (expand-file-name rrb-undo-file-base
		      temporary-file-directory)))
  "Directory that stores undo files")

(defvar rrb-marshal-file-base "rrbmarshal"
  "*Base file name for marshal file")

(defvar rrb-marshal-file-name "")


;;;; Internal variables
(defconst rrb-io-splitter "\C-a")
(defconst rrb-io-terminator "-- END --")
(defconst rrb-modifier "modified")
(defconst rrb-not-modifier "not-modified")

(defvar rrb-input-buffer (get-buffer-create " *rrb-input*"))
(defvar rrb-output-buffer (get-buffer-create " *rrb-output*"))
(defvar rrb-error-buffer (get-buffer-create " *rrb-error*"))
(defvar rrb-default-value-buffer (get-buffer-create " *rrb-default-value*"))
(defvar rrb-undo-buffer (get-buffer-create " *rrb-undo*"))
(defvar rrb-modified-p-buffer (get-buffer-create " *rrb-modified-p-file"))

(defvar rrb-undo-count 0)
(defvar rrb-now-refactoring nil)

(add-hook 'kill-emacs-hook 'rrb-delete-undo-files)


;;;; Utility functions
(defun rrb-find-all (prec list)
  "Return list containing all elements of PREC is true"
  (cond ((eq list nil) nil)
	((funcall prec (car list)) (cons (car list)
					 (rrb-find-all prec (cdr list))))
	(t (rrb-find-all prec (cdr list)))))

(defun rrb-buffer-map-line (prec)
  "Return list of (PREC line) for each line in current buffer"
  (mapcar prec
	  (split-string (buffer-string) "\n")))

(defun rrb-chop (str)
  "Return chopped string"
  (substring str 0 -1))

(defun rrb-current-line ()
  "Return the vertical position of point..."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun rrb-each (function list)
  "Call function for each value of list"
  (while list
    (funcall function (car list))
    (setq list (cdr list))))

;;;; Compatibility (for old emacs)

;;;point-at-bol => line-beginning-position
;;;point-at-eol => line-end-position

(if (not (fboundp 'point-at-bol))
    (defun point-at-bol ()
       (line-beginning-position)))
(if (not (fboundp 'point-at-eol))
    (defun point-at-eol ()
       (line-end-position)))

;;;; Main functions

(defun rrb-ruby-script-p (buffer)
  (and (buffer-file-name buffer)
       (or (string-match rrb-ruby-file-name-regexp (buffer-file-name buffer))
	   (save-current-buffer
	     (save-excursion
	       (set-buffer buffer)
	       (goto-char (point-min))
	       (looking-at rrb-first-line-of-ruby-file-regexp))))))

(defun rrb-all-ruby-script-buffer ()
  "Return all ruby script buffer,`ruby script buffer' means `its file name
matches with rrb-ruby-file-name-regexp' or `its first line is /^#!.*ruby.*$/'"
  (rrb-find-all 'rrb-ruby-script-p
		(buffer-list)))

(defun rrb-insert-input-string (src-buffer)
  "Insert contents of src-buffer to current buffer"
  (insert (buffer-file-name src-buffer))
  (insert rrb-io-splitter)
  (insert-buffer-substring src-buffer)
  (insert rrb-io-splitter))

(defun rrb-insert-modified-p (src-buffer)
  "Insert if src-buffer is modified or not to current buffer"
  (insert (buffer-file-name src-buffer))
  (insert rrb-io-splitter)
  (insert (if (buffer-modified-p src-buffer)
	      rrb-modifier
	    rrb-not-modifier))
  (insert rrb-io-splitter))

(defun rrb-setup-buffer (proc buffer-list buffer)
  "Generate input string on \" *rrb-input*\""
  (save-current-buffer
    (set-buffer buffer)
    (erase-buffer)
    (mapcar proc
	    buffer-list)
    (insert rrb-io-terminator)
    (insert rrb-io-splitter)))

(defun rrb-clean-buffer (buffer)
  "Clean buffer"
  (save-current-buffer
    (set-buffer buffer)
    (erase-buffer)))

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
  (let ((buffer-point-alist (rrb-buffer-point-alist)))
    (if (/= (apply 'rrb-run-process "rrb" args) 0)
	(error "fail to refactor: %s" (rrb-error-message)))
    (rrb-make-undo-files (rrb-get-buffer-list rrb-output-buffer))
    (setq rrb-undo-count (+ rrb-undo-count 1))
    (rrb-output-to-buffer-and-reset-point buffer-point-alist)))

(defun rrb-make-temp-name (base)
  (make-temp-name (expand-file-name base temporary-file-directory)))

(defun rrb-make-marshal-file ()
  (save-current-buffer
    (let ((error-code)
	  (tmpfile (rrb-make-temp-name rrb-tmp-file-base)))
      (setq rrb-marshal-file-name (rrb-make-temp-name rrb-marshal-file-base))
      (rrb-clean-buffer rrb-output-buffer)
      (rrb-clean-buffer rrb-error-buffer)
      (set-buffer rrb-input-buffer)
      (setq error-code (apply 'call-process-region
			      (point-min) (point-max)
			      "rrb_marshal"
			      nil
			      (list rrb-output-buffer tmpfile)
			      nil
			     (list "--stdin-fileout" rrb-marshal-file-name)))
      (rrb-output-to-error-buffer tmpfile)
      (delete-file tmpfile)
      error-code)))

(defun rrb-delete-marshal-file ()
  (delete-file rrb-marshal-file-name))

(defun rrb-run-process (command &rest args)
  "Run COMMAND and return error code"
  (save-current-buffer
    (let ((error-code)
	  (tmpfile (rrb-make-temp-name rrb-tmp-file-base)))
      (rrb-clean-buffer rrb-output-buffer)
      (rrb-clean-buffer rrb-error-buffer)
      (setq error-code (apply 'call-process
			      command
			      nil
			      (list rrb-output-buffer tmpfile)
			      nil
			      `(,@args "--marshalin-stdout" ,rrb-marshal-file-name)))
      (rrb-output-to-error-buffer tmpfile)
      (delete-file tmpfile)
      error-code)))

(defun rrb-buffer-point-alist ()
  (save-current-buffer
    (mapcar (lambda (buf)
	      (set-buffer buf)
	      (cons buf (point)))
	    (rrb-all-ruby-script-buffer))))

(defun rrb-add-change-hook-to-all-ruby-script ()
  "Add hook(before-change-function) to all ruby scripts"
  (save-current-buffer
    (rrb-each 
     (lambda (buffer)
       (set-buffer buffer)
       (make-local-hook 'before-change-functions)
       (add-hook 'before-change-functions 
		 'rrb-notify-file-changed nil t))
     (rrb-all-ruby-script-buffer))))
    
(defun rrb-prepare-refactoring ()
  "Call this function before Refactoring"
  (setq rrb-now-refactoring t)
  (rrb-add-change-hook-to-all-ruby-script)
  (rrb-setup-buffer 'rrb-insert-input-string
		    (rrb-all-ruby-script-buffer)
		    rrb-input-buffer)
  (rrb-make-marshal-file))

(defun rrb-terminate-refactoring ()
  "Call this function after Refactoring"
  (setq rrb-now-refactoring nil)
  (rrb-delete-marshal-file))

;;;; operation for script-buffer

(defun rrb-each-for-compound-buffer (function compound-buffer)
  "Call function for each file included in compound buffer"
  (save-current-buffer
    (set-buffer compound-buffer)
    (let ((script-list (split-string (buffer-string) rrb-io-splitter)))
      (while (not (string= (car script-list) rrb-io-terminator))
	(funcall function (car script-list) (cadr script-list))
	(setq script-list (cddr script-list))))))
  

(defun rrb-get-file-name-list (compound-buffer)
  "Get filename list from compound buffer"
  (let (file-name-list '())
    (rrb-each-for-compound-buffer 
     (lambda (file-name file-content)
       (setq file-name-list (cons file-name file-name-list)))
     compound-buffer)
    file-name-list
    ))

(defun rrb-get-buffer-list (compound-buffer)
  "Get buffer list visiting files included in compound-buffer"
  (mapcar 'get-file-buffer
	  (rrb-get-file-name-list compound-buffer)))


(defun rrb-output-to-buffer-and-reset-point (alist)
  "Rewrite all ruby script buffer from \" *rrb-output\" and reset cursor point"
  (save-current-buffer
   (rrb-each-for-compound-buffer 
    (lambda (file-name file-content)
      (set-buffer (get-file-buffer file-name))
      (erase-buffer)
      (insert file-content)
      (goto-char (cdr (assq (current-buffer) alist))))
    rrb-output-buffer)))
  

;;;; Completion

;;;
;;; completion type-1 ( str ; str,str,...,str)
;;
;; rrb_compinfo output example
;;
;;  A#x;heke,uhe,hoge
;;  A::B#y;hoge
;;  a::B#z;i,j,k,l
;;
(defun rrb-complist-first-for-type-1 ()
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (rrb-buffer-map-line (lambda (line) (split-string line ";")))))

(defun rrb-complist-second-for-type-1 (key)
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (goto-char (point-min))
    (when (search-forward (concat key ";") nil t)
      (mapcar 'list
	      (split-string (buffer-substring (point) (point-at-eol)) ",")))))

(defun rrb-comp-read-type-1 (compinfo-arg default-arg prompt1 prompt2 prompt3)
  "completion read for Rename local variable, Rename instance variable, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((method (completing-read prompt1
				 (rrb-complist-first-for-type-1) nil nil default-arg)))
    (list method
 	  (completing-read prompt2 (rrb-complist-second-for-type-1 method))
 	  (read-from-minibuffer prompt3))))

;;;
;;; completion type-2  ( str,str,...,str, )
;;
;; rrb_compinfo output example
;;
;;  A,A::B,A::C,A::C::C1,A::C::C2,
;;
(defun rrb-complist-type-2 ()
  (save-current-buffer
    (set-buffer rrb-output-buffer)
    (goto-char (point-min))
    (mapcar 'list
            (split-string (buffer-substring (point-at-bol) (point-at-eol)) ","))))

(defun rrb-comp-read-type-2 (compinfo-arg default-arg prompt1 prompt2)
  "Completion read for Rename method all, Rename Constant, etc.."
  (when (/= 0 (rrb-run-process "rrb_compinfo" compinfo-arg))
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (list (completing-read prompt1 (rrb-complist-type-2) nil nil default-arg)
	(read-from-minibuffer prompt2)))

;;;
;;; completion for Pull up method, Push down method
;;
(defun rrb-comp-read-move-method (compinfo-arg1 prompt1 compinfo-arg2 default-arg2 prompt2)
  "completion read for Pull up method, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg1) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((old-class-method (completing-read prompt1 (rrb-complist-type-2))))
    (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg2) 0)
      (error "rrb_info: fail to get information %s" (rrb-error-message)))
    (list old-class-method
	  (completing-read prompt2 (rrb-complist-type-2) nil nil default-arg2))))

;;;
;;; completion for Rename variable defined at some namespace
;;
(defun rrb-comp-read-type-4 (compinfo-arg1 default-arg1 prompt1 compinfo-arg2 prompt2 prompt3)
  "completion read for rename instance variable, etc.."
  (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg1) 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (let ((retval-1 (completing-read prompt1 (rrb-complist-type-2) nil nil default-arg1)))
    (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg2 "--target" retval-1) 0)
      (error "rrb_info: fail to get information %s" (rrb-error-message)))
    (list retval-1
	  (completing-read prompt2 (rrb-complist-type-2))
	  (read-from-minibuffer prompt3))))



(defun rrb-comp-read-recursively (compinfo-arg default-arg prompt)
  "completion read for rename method, etc.."
  (let ((compound-result "")
	(result))
    (when (/= (rrb-run-process "rrb_compinfo" compinfo-arg) 0)
      (error "rrb_info: fail to get information %s" (rrb-error-message)))
    (while (progn 
	     (setq result (completing-read 
			   prompt
			   (rrb-complist-type-2)
			   nil nil default-arg))
	     (not (string= result "")))
      (setq default-arg "")
      (setq compound-result (format "%s %s" compound-result result))
      (message (format "%s %s" "inputed data: " compound-result))
      (sit-for 1))
    compound-result))

;;;
;;; default value
;;    
(defun rrb-get-value-on-cursor (args)
  (save-current-buffer
    (rrb-setup-buffer 'rrb-insert-input-string 
		      (list (current-buffer))
		      rrb-default-value-buffer)
    (if (/= (rrb-run-process "rrb_default_value"
			     (buffer-file-name) 
			     (number-to-string (rrb-current-line)) args) 0)
	""
      (set-buffer rrb-output-buffer)
      (buffer-substring (point-min) (point-max)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Undo, Redo
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rrb-undo-base (undo-file-name modified-p-file-name)
  "Base of Undo and Redo of the previous refactoring"
  (defun rrb-read-undo-file ()
    (save-current-buffer
      (set-buffer rrb-output-buffer)
      (rrb-clean-buffer rrb-output-buffer)
      (insert-file-contents undo-file-name)
      (let* ((buffer-list (rrb-get-buffer-list rrb-output-buffer))
	     (modified-list (mapcar 'buffer-modified-p buffer-list)))
	(rrb-make-undo-files buffer-list)
	(rrb-output-to-buffer-and-reset-point (rrb-buffer-point-alist))
	modified-list)))
  (defun rrb-read-modified-p-file (modified-list)
    (save-current-buffer
      (set-buffer rrb-output-buffer)
      (rrb-clean-buffer rrb-output-buffer)
      (insert-file-contents modified-p-file-name)
      (rrb-each-for-compound-buffer
       (lambda (file-name file-contents)
	 (if (string= file-contents rrb-not-modifier)
	     (progn
	       (set-buffer (get-file-buffer file-name))
	       (set-buffer-modified-p (not (car modified-list)))
	       (setq modifed-list (cdr modified-list)))))
       rrb-output-buffer)))
  (if (file-readable-p undo-file-name)
      (let* ((modified-list (rrb-read-undo-file)))
	(rrb-read-modified-p-file modified-list)
	t)
    (message "Nothing to do!")
    nil))


(defun rrb-make-undo-files (buffer-list)
  "Make temporary files for Undo, Redo"
  (defun rrb-make-undo-file ()
    "Make temporary file which records previous states of each file"
    (save-current-buffer
      (set-buffer rrb-undo-buffer)
      (rrb-clean-buffer rrb-undo-buffer)
      (rrb-setup-buffer 'rrb-insert-input-string 
			buffer-list
			rrb-undo-buffer)
      (write-region (point-min) (point-max)
		    (rrb-make-undo-file-name rrb-undo-count) nil 0 nil)
      (set-buffer rrb-modified-p-buffer)
      (rrb-clean-buffer rrb-modified-p-buffer)
      (rrb-setup-buffer 'rrb-insert-modified-p
			buffer-list
			rrb-modified-p-buffer)))
  (defun rrb-make-modified-p-file ()
    "Make temporary file which records if each files are modified or not"
    (save-current-buffer
      (set-buffer rrb-modified-p-buffer)
      (rrb-clean-buffer rrb-modified-p-buffer)
      (rrb-setup-buffer 'rrb-insert-modified-p
			buffer-list
			rrb-modified-p-buffer)
      (write-region (point-min) (point-max)
		    (rrb-make-not-modified-file-name rrb-undo-count)
		    nil 0 nil)))
  (if (rrb-make-undo-directory)
      (progn
	(rrb-make-undo-file)
	(rrb-make-modified-p-file))))
	

(defun rrb-make-undo-file-name (undo-count)
  (expand-file-name (number-to-string undo-count)
		    rrb-undo-directory))

(defun rrb-make-not-modified-file-name (undo-count)
  (expand-file-name (format "%s.%s" 
			    (number-to-string undo-count)
			    "nmod")
		    rrb-undo-directory))

(defun rrb-make-undo-directory ()
  "Make directory for temporary file for Undo, Redo"
  (if (not (file-exists-p rrb-undo-directory))
      (make-directory rrb-undo-directory))
  (file-accessible-directory-p rrb-undo-directory))

(defun rrb-delete-undo-files ()
  "Delete temporary files for Undo, Redo"
  (and (file-exists-p rrb-undo-directory)
       (file-accessible-directory-p rrb-undo-directory)
       (progn
	 (rrb-each 
	  (lambda (sub-file)
	    (if (not (file-directory-p sub-file))
		(delete-file sub-file)))
	  (directory-files rrb-undo-directory t))
	 (delete-directory rrb-undo-directory)))
  (setq rrb-undo-count 0))

(defun rrb-notify-file-changed (start end)
  (if (not rrb-now-refactoring)
      (rrb-delete-undo-files)))
;;;
;;; Undo
;;
(defun rrb-undo ()
  "Undo of the last refactoring"
  (interactive)
  (setq rrb-now-refactoring t)
  (let ((prev-undo-count (- rrb-undo-count 1)))
    (if (rrb-undo-base (rrb-make-undo-file-name prev-undo-count)
		       (rrb-make-not-modified-file-name prev-undo-count))
	(setq rrb-undo-count prev-undo-count)))
  (setq rrb-now-refactoring nil))
					  
;;;
;;; Redo
;;
(defun rrb-redo ()
  "Redo of the last Undo"
  (interactive)
  (setq rrb-now-refactoring t)
  (let ((next-undo-count (+ rrb-undo-count 1)))
    (if (rrb-undo-base (rrb-make-undo-file-name next-undo-count)
		       (rrb-make-not-modified-file-name next-undo-count))
	(setq rrb-undo-count next-undo-count)))
  (setq rrb-now-refactoring nil))
	

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Refactoring
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Refactoring: Rename local variable 
(defun rrb-comp-read-rename-local-variable ()
  "Completion read for Rename local variable"
  (rrb-comp-read-type-4 "--methods" (rrb-get-value-on-cursor "--method") 
			"Refactored method: " "--local-vars" "Old variable: "
			"New variable: "))

(defun rrb-rename-local-variable (method old-var new-var)
  "Refactor code: rename local variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-local-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-local-variable" method old-var new-var))
  (rrb-terminate-refactoring))

;;;; Refactoring: Rename method
;;;
(defun rrb-comp-read-rename-method ()
  "completion read for rename method, etc.."
  (let ((compound-result 
	 (rrb-comp-read-recursively
	  "--classes"
	  (rrb-get-value-on-cursor "--class")
	  "Refactored classes(type just RET to finish): ")))
    (when (/= (rrb-run-process "rrb_compinfo" "--bare-methods"
			       "--target" compound-result) 0)
      (error "rrb_info: fail to get information %s" (rrb-error-message)))
    (list compound-result
	  (completing-read "Old method: " (rrb-complist-type-2))
	  (read-from-minibuffer "New method: "))))


(defun rrb-rename-method (classes old-method new-method)
  "Refactor code: rename local variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-method)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-method" classes old-method new-method))
  (rrb-terminate-refactoring))

;;;; Refactoring: Rename method all
(defun rrb-comp-read-rename-method-all ()
  "Completion read for Rename method all"
  (rrb-comp-read-type-2 "--bare-methods" 
			(rrb-get-value-on-cursor "--bare-name")
			"Old method: " "New method: "))

(defun rrb-rename-method-all (old-method new-method)
  "Refactor code: rename method all old method as new"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-method-all)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-method-all" old-method new-method))
  (rrb-terminate-refactoring))

;;;; Refactoring: Extract method
(defun rrb-begin-line-num (begin)
  (goto-char begin)
  (if (eolp)
      (forward-char)
    (beginning-of-line))
  (1+ (count-lines (point-min) (point))))

(defun rrb-end-line-num (end)
  (goto-char end)
  (if (bolp)
      (backward-char))
  (count-lines (point-min) (point)))

(defun rrb-extract-method (begin end new_method)
  "Refactor code: Extract method"
  (interactive "r\nsNew method: ")
  (save-current-buffer
    (rrb-prepare-refactoring)
    (rrb-do-refactoring "--extract-method"
			(buffer-file-name)
			new_method
			(number-to-string (rrb-begin-line-num begin))
			(number-to-string (rrb-end-line-num end))))
  (rrb-terminate-refactoring))

;;;; Refactoring: Rename instance variable
(defun rrb-comp-read-rename-instance-variable ()
  "completion read for rename instance variable"
  (rrb-comp-read-type-4 "--classes" 
			(rrb-get-value-on-cursor "--class") 
			"Refactord class: " 
			"--instance-vars"
			"Old instance variable: "
			"New instance variable: " ))

(defun rrb-rename-instance-variable (ns old-var new-var)
  "Refactor code: Rename instance variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-instance-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-instance-variable" ns old-var new-var))
  (rrb-terminate-refactoring))

;;;; Refactoring: Rename class variable
(defun rrb-comp-read-rename-class-variable ()
  "completion read for rename instance variable"
  (rrb-comp-read-type-4 "--classes"
			(rrb-get-value-on-cursor "--class") 
			"Refactord class: " 
			"--class-vars"
			"Old class variable: " 
			"New class variable: " ))

(defun rrb-rename-class-variable (ns old-var new-var)
  "Refactor code: Rename instance variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-class-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-class-variable" ns old-var new-var))
  (rrb-terminate-refactoring))


;;;; Refactoring: Rename global variable
(defun rrb-comp-read-rename-global-variable ()
  "compleion read for rename global variable"
  (rrb-comp-read-type-2 "--global-vars"
			""
			"Old global variable: " "New global variable: "))

(defun rrb-rename-global-variable (old-var new-var)
  "Refactor code: Rename global variable"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-global-variable)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-global-variable" old-var new-var))
  (rrb-terminate-refactoring))

;;;; Refactoring: Rename constant
(defun rrb-comp-read-rename-constant ()
  "compleion read for rename constant"
  (rrb-comp-read-type-2 "--constants" 
			(rrb-get-value-on-cursor "--class")
			"Old constant: " "New constant: "))

(defun rrb-rename-constant (old-const new-const)
  "Refactor code: Rename constant"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-constant)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-constant" old-const new-const))
  (rrb-terminate-refactoring))

;;;; Refactoring: Rename class

(defun rrb-comp-read-rename-class ()
  "compleion read for rename class"
  (rrb-comp-read-type-2 "--classes" 
			(rrb-get-value-on-cursor "--class")
			"Old class/module: " "New class/module: "))

(defun rrb-rename-class (old-class new-class)
  "Refactor code: Rename class or module"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-rename-class)))
  (save-current-buffer
    (rrb-do-refactoring "--rename-constant" old-class new-class))
  (rrb-terminate-refactoring))
			;class name is a constant.

	       
;;;; Refactoring: Pull up method

(defun rrb-comp-read-pullup-method ()
  "completion read for pull up method"
  (rrb-comp-read-move-method "--methods" "Old Method: "
			"--classes"
			(rrb-get-value-on-cursor "--class") "New class: "))

(defun rrb-pullup-method (old-method new-class)
  "Refactor code: Pull up method"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-pullup-method)))
  (save-current-buffer
    (rrb-do-refactoring "--pullup-method" old-method new-class
			(buffer-file-name)
			(number-to-string (rrb-current-line))))
  (rrb-terminate-refactoring))
		 
;;;; Refactoring: Push down method

(defun rrb-comp-read-pushdown-method ()
  "completion read for push down method"
  (rrb-comp-read-move-method "--methods" "Old Method: "
			"--classes" 
			(rrb-get-value-on-cursor "--class") "New class: "))

(defun rrb-pushdown-method (old-method new-class)
  "Refactor code: Push down method"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-pushdown-method)))
  (save-current-buffer
    (rrb-do-refactoring "--pushdown-method" old-method new-class 
			(buffer-file-name)
			(number-to-string (rrb-current-line))))
  (rrb-terminate-refactoring))
		 
;;;; Refactoring: Extract superclass
;;;
(defun rrb-comp-read-extract-superclass ()
  "completion read for extract superclass"
  (when (/= (rrb-run-process "rrb_compinfo" "--classes") 0)
    (error "rrb_info: fail to get information %s" (rrb-error-message)))
  (list (completing-read "Location: " (rrb-complist-type-2) nil nil 
			 (rrb-get-value-on-cursor "--class"))
	(read-from-minibuffer "New Class: ")
	(rrb-comp-read-recursively "--classes" ""
				   "Targets(type just RET to finish): ")))



(defun rrb-extract-superclass (namespace new-class targets)
  "Refactor code: Extract superclass"
  (interactive (progn
		 (rrb-prepare-refactoring)
		 (rrb-comp-read-extract-superclass)))
  (save-current-buffer
    (rrb-do-refactoring "--extract-superclass" namespace new-class targets
			(buffer-file-name)
			(number-to-string (rrb-current-line))))
  (rrb-terminate-refactoring))