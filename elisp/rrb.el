;;; rrb.el -- Ruby Refactoring Brower emacs interface

;; Copyright (C) 2004 OHBAYASHI Ippei, YOSHIDA Yuichi, HARA Yutaka
;; Copyright (C) 2004  Kyoto univ. Microcomputer Club

;; Author: OHBAYASHI Ippei <ohai@kmc.gr.jp>
;;	YOSHIDA Yuichi <oxy@kmc.gr.jp>
;;	HARA Yutaka <yhara@kmc.gr.jp>
;; Keywords: ruby refactoring browser

;; This file is part of Ruby Refactoring Browser.
;; Ruby Refactoring Browser is distributed under the same term as Ruby.

;;; Code:

;;;; Customizable variables
(defvar rrb-ruby-file-name-regexp "^.*\\.rb$"
  "*Regular expression matching ruby script file name")
(defvar rrb-first-line-of-ruby-file-regexp "^#!.*ruby.*$"
  "*Regular expression matching first line of ruby script")

(defvar rrb-tmp-file-base "rrblog"
  "*Base file name of error log file")

(defvar rrb-undo-file-base "rrbundo"
  "*Base file name of undo files")

(defvar rrb-undo-directory 
  (file-name-as-directory
   (make-temp-name
    (expand-file-name rrb-undo-file-base
		      temporary-file-directory)))
  "Directory which stores undo files")

(defvar rrb-marshal-file-base "rrbmarshal"
  "*Base file name of marshal file")


;;;; Internal variables
(defconst rrb-io-splitter "\C-a")
(defconst rrb-io-terminator "-- END --")
(defconst rrb-modifier "modified")
(defconst rrb-not-modifier "not-modified")

(defvar rrb-input-buffer (get-buffer-create " *rrb-input*"))
(defvar rrb-output-buffer (get-buffer-create " *rrb-output*"))
(defvar rrb-error-buffer (get-buffer-create " *rrb-error*"))
(defvar rrb-undo-buffer (get-buffer-create " *rrb-undo*"))
(defvar rrb-modified-p-buffer (get-buffer-create " *rrb-modified-p-file"))

(defvar rrb-undo-count 0)
(defvar rrb-now-refactoring-flag nil)
(defvar rrb-marshal-file-name "")

(add-hook 'kill-emacs-hook 'rrb-delete-undo-files)


;;;; Utility functions
(defun rrb-find-all (prec list)
  "Return a list consisted of the elements which satisfy PREC."
  (cond ((eq list nil) nil)
	((funcall prec (car list)) (cons (car list)
					 (rrb-find-all prec (cdr list))))
	(t (rrb-find-all prec (cdr list)))))

(defun rrb-current-line ()
  "Return the vertical position of 'point'."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)))

(defun rrb-make-temp-name (base)
  "Make temporary file/directory name and return it."
  (make-temp-name (expand-file-name base temporary-file-directory)))

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
  "Return if the file BUFFER is visiting is ruby script or not."
  (and (buffer-file-name buffer)
       (or (string-match rrb-ruby-file-name-regexp (buffer-file-name buffer))
           (save-excursion
             (set-buffer buffer)
             (goto-char (point-min))
             (looking-at rrb-first-line-of-ruby-file-regexp)))))

(defun rrb-all-ruby-script-buffer ()
  "Return a list containing all ruby script buffers."
  (rrb-find-all 'rrb-ruby-script-p
		(buffer-list)))

(defun rrb-insert-input-string (src-buffer)
  "Insert the contents of SRC-BUFFER to 'current-buffer'."
  (insert (buffer-file-name src-buffer))
  (insert rrb-io-splitter)
  (insert-buffer-substring src-buffer)
  (insert rrb-io-splitter))

(defun rrb-insert-modified-p (src-buffer)
  "Insert if SRC-BUFFER is modified or not to 'current-buffer'."
  (insert (buffer-file-name src-buffer))
  (insert rrb-io-splitter)
  (insert (if (buffer-modified-p src-buffer)
	      rrb-modifier
	    rrb-not-modifier))
  (insert rrb-io-splitter))

(defun rrb-setup-buffer (proc buffer-list target-buffer)
  "Generate compound string on TARGET-BUFFER."
  (save-current-buffer
    (set-buffer target-buffer)
    (erase-buffer)
    (mapcar proc
	    buffer-list)
    (insert rrb-io-terminator)
    (insert rrb-io-splitter)))

(defun rrb-clean-buffer (target-buffer)
  "Clean TARGET-BUFFER."
  (save-current-buffer
    (set-buffer target-buffer)
    (erase-buffer)))

(defun rrb-output-to-error-buffer (filename)
  "load FILENAME to \" *rrb-error\"."
  (save-current-buffer
    (set-buffer rrb-error-buffer)
    (insert-file-contents filename)))

(defun rrb-error-message ()
  "Extract Error Message from \" *rrb-error\"."
  (save-current-buffer
    (set-buffer rrb-error-buffer)
    (goto-char (point-min))
    (substring (thing-at-point 'line) 0 -1)))
    
(defun rrb-do-refactoring (&rest args)
  "Do refactoring."
  (if (/= (apply 'rrb-run-process "rrb" 
		 (append args (list "--marshalin-stdout" rrb-marshal-file-name))) 0)
      (error "fail to refactor: %s" (rrb-error-message)))
  (rrb-make-undo-files (rrb-get-buffer-list rrb-output-buffer))
  (setq rrb-undo-count (+ rrb-undo-count 1))
  (rrb-output-to-buffer rrb-output-buffer))

;;;
;;; Run rrb_compinfo
;;; The output of rrb_compino is like follow line
;;;  A,A::B,A::C,A::C::C1,A::C::C2,
;;;
(defun rrb-run-comp-info (&rest args)
  "Run rrb_compinfo and return completion list."
  (save-current-buffer
    (if (/= (apply 'rrb-run-process "rrb_compinfo" 
		   (append args (list "--marshalin-stdout" rrb-marshal-file-name))) 0)
	(error "rrb_compinfo: fail to get information %s" (rrb-error-message)))
    (set-buffer rrb-output-buffer)
    (goto-char (point-min))
    (mapcar 'list
	    (split-string (buffer-substring (point-at-bol) (point-at-eol)) ","))))


(defun rrb-run-default-value (&rest args)
  "Run rrb_default_value and return default value."
  (save-current-buffer
    (if (/= (apply 'rrb-run-process "rrb_default_value" 
		   (append args (list "--marshalin-stdout" rrb-marshal-file-name))) 0)
	""
      (set-buffer rrb-output-buffer)
      (buffer-substring (point-min) (point-max)))))




(defun rrb-run-process (command &rest args)
  "Run COMMAND with no input from stdin and return the error code."
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
			      args))
      (rrb-output-to-error-buffer tmpfile)
      (delete-file tmpfile)
      error-code)))

(defun rrb-run-process-region (command &rest args)
  "Run COMMAND and return the error code."
  (save-current-buffer
    (let ((error-code)
	  (tmpfile (rrb-make-temp-name rrb-tmp-file-base)))
      (rrb-clean-buffer rrb-output-buffer)
      (rrb-clean-buffer rrb-error-buffer)
      (set-buffer rrb-input-buffer)
      (setq error-code (apply 'call-process-region
			      (point-min) (point-max)
			      command
			      nil
			      (list rrb-output-buffer tmpfile)
			      nil
                              args))
      (rrb-output-to-error-buffer tmpfile)
      (delete-file tmpfile)
      error-code)))



(defmacro rrb-declare-refactoring (&rest body)
  `(progn
     (setq rrb-now-refactoring-flag t)
     (unwind-protect
         ,@body
       (setq rrb-now-refactoring-flag nil))))

(defun rrb-make-marshal-file ()
  "Run rrb_marchal and make cache file"
  (rrb-setup-buffer 'rrb-insert-input-string
                    (rrb-all-ruby-script-buffer)
                    rrb-input-buffer)
  (setq rrb-marshal-file-name (rrb-make-temp-name rrb-marshal-file-base))
  (if (/= (apply 'rrb-run-process-region "rrb_marshal" 
                 (list "--stdin-fileout" rrb-marshal-file-name)) 0)
      (error "rrb_marshal: fail to read source files %s" (rrb-error-message))))

(defun rrb-delete-marshal-file ()
  "Delete cache file that was created by rrb-make-marshal-file"
  (if (file-readable-p rrb-marshal-file-name)
      (delete-file rrb-marshal-file-name)))

(defun rrb-add-change-hook-to-all-ruby-script ()
  "Add hook('before-change-function') to all ruby scripts. 
This hook clears undo files when ary ruby script buffer is changed"
  (save-current-buffer
    (mapc 
     (lambda (buffer)
       (set-buffer buffer)
       (make-local-hook 'before-change-functions)
       (add-hook 'before-change-functions 
		 'rrb-notify-file-changed nil t))
     (rrb-all-ruby-script-buffer))))

(defun rrb-prepare-refactoring ()
  (rrb-add-change-hook-to-all-ruby-script)
  (rrb-make-marshal-file))

(defun rrb-terminate-refactoring ()
  (rrb-delete-marshal-file))

(defmacro rrb-setup-refactoring (&rest body)
  `(rrb-declare-refactoring
    (rrb-prepare-refactoring)
    (unwind-protect
        ,@body
      (rrb-terminate-refactoring))))
    


;;;; operation for script-buffer

(defun rrb-each-for-compound-buffer (function compound-buffer)
  "Apply FUNCTION to each file included in COMPOUND-BUFFER."
  (save-current-buffer
    (set-buffer compound-buffer)
    (let ((script-list (split-string (buffer-string) rrb-io-splitter)))
      (while (not (string= (car script-list) rrb-io-terminator))
	(funcall function (car script-list) (cadr script-list))
	(setq script-list (cddr script-list))))))
  

(defun rrb-get-file-name-list (compound-buffer)
  "Return a filename list of COMPOUND-BUFFER."
  (let (file-name-list '())
    (rrb-each-for-compound-buffer 
     (lambda (file-name file-content)
       (setq file-name-list (cons file-name file-name-list)))
     compound-buffer)
    file-name-list
    ))

(defun rrb-get-buffer-list (compound-buffer)
  "Return a list of buffer which is visiting files included in COUMPOUND-BUFFER."
  (mapcar 'get-file-buffer
	  (rrb-get-file-name-list compound-buffer)))


(defun rrb-output-to-buffer (compound-buffer)
  "Rewrite all ruby script buffer, using COMPOUND-BUFFER."
  (save-current-buffer
    (rrb-each-for-compound-buffer 
     (lambda (file-name file-content)
       (set-buffer (get-file-buffer file-name))
       (let ((before-point (point)))
	 (erase-buffer)
	 (insert file-content)
	 (goto-char before-point)))
     compound-buffer)))

;;;; Completion

;;;
;;; completion type-2  ( str,str,...,str, )
;;;

(defun rrb-comp-read-type-2 (compinfo-arg default-arg prompt1 prompt2)
  "Completely read for Rename method all, Rename Constant, etc.."
  (list (completing-read prompt1 (rrb-run-comp-info compinfo-arg) nil nil default-arg)
	(read-from-minibuffer prompt2)))

;;;
;;; completion for Pull up method, Push down method
;;;
(defun rrb-comp-read-move-method (compinfo-arg1 prompt1 compinfo-arg2 default-arg2 prompt2)
  "Completely read for Pull up method, etc.."
  (list (completing-read prompt1 (rrb-run-comp-info compinfo-arg1))
	(completing-read prompt2 (rrb-run-comp-info compinfo-arg2) nil nil default-arg2)))

;;;
;;; completion for Rename variable defined at some namespace
;;;
(defun rrb-comp-read-type-4 (compinfo-arg1 default-arg1 prompt1 compinfo-arg2 prompt2 prompt3)
  "Completely read for rename instance variable, etc.."
  (let ((result (completing-read prompt1 
			 (rrb-run-comp-info compinfo-arg1) nil nil default-arg1)))
    (list 
     result
     (completing-read prompt2
		      (rrb-run-comp-info compinfo-arg2 "--target" result))
     (read-from-minibuffer prompt3))))



(defun rrb-comp-read-recursively (compinfo-arg default-arg prompt)
  "Completing read recursively."
  (let ((compound-result "")
	(result))
    (while (progn 
	     (setq result (completing-read 
			   prompt
			   (rrb-run-comp-info compinfo-arg)
			   nil nil default-arg))
	     (not (string= result "")))
      (setq default-arg "")
      (setq compound-result (format "%s %s" compound-result result))
      (message (format "%s %s" "Inputed data: " compound-result))
      (sit-for 1))
    compound-result))

;;;; default value
(defun rrb-get-value-on-cursor (args)
  (rrb-run-default-value (buffer-file-name) (number-to-string (rrb-current-line)) args))


;;;;;  Undo, Redo
(defun rrb-undo-base (undo-file-name modified-p-file-name)
  "Base function of undo and redo."
  (let ((undo-buffer-list))
    (defun rrb-read-undo-file ()
      (save-current-buffer
	(set-buffer rrb-output-buffer)
	(rrb-clean-buffer rrb-output-buffer)
	(insert-file-contents undo-file-name)
	(setq undo-buffer-list (rrb-get-buffer-list rrb-output-buffer))
	(rrb-make-undo-files undo-buffer-list)
	(rrb-output-to-buffer rrb-output-buffer)))
    (defun rrb-read-modified-p-file ()
      (save-current-buffer
	(set-buffer rrb-output-buffer)
	(rrb-clean-buffer rrb-output-buffer)
	(insert-file-contents modified-p-file-name)
	(let ((modified-list (mapcar 'buffer-modified-p undo-buffer-list)))
	  (rrb-each-for-compound-buffer
	   (lambda (file-name file-contents)
	     (if (string= file-contents rrb-not-modifier)
		 (progn
		   (set-buffer (get-file-buffer file-name))
		   (set-buffer-modified-p (not (car modified-list)))
		   (setq modifed-list (cdr modified-list)))))
	   rrb-output-buffer))))
    (if (file-readable-p undo-file-name)
	(progn
	  (rrb-read-undo-file)
	  (rrb-read-modified-p-file)
	  t)
      (message "Nothing to do!")
      nil)))


(defun rrb-make-undo-files (buffer-list)
  "Make temporary files for undo and redo."
  (defun rrb-make-undo-file ()
    "Make temporary file which records previous states of each file."
    (save-current-buffer
      (set-buffer rrb-undo-buffer)
      (rrb-clean-buffer rrb-undo-buffer)
      (rrb-setup-buffer 'rrb-insert-input-string 
			buffer-list
			rrb-undo-buffer)
      (write-region (point-min) (point-max)
		    (rrb-make-undo-file-name rrb-undo-count) nil 0 nil)))
  (defun rrb-make-modified-p-file ()
    "Make temporary file which records if each files are modified or not."
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
  "Make directory for temporary file for undo and redo."
  (if (not (file-exists-p rrb-undo-directory))
      (make-directory rrb-undo-directory))
  (file-accessible-directory-p rrb-undo-directory))

(defun rrb-delete-undo-files ()
  "Delete temporary files made for undo and redo."
  (and (file-exists-p rrb-undo-directory)
       (file-accessible-directory-p rrb-undo-directory)
       (progn
	 (mapc 
	  (lambda (sub-file)
	    (if (not (file-directory-p sub-file))
		(delete-file sub-file)))
	  (directory-files rrb-undo-directory t))
	 (delete-directory rrb-undo-directory)))
  (setq rrb-undo-count 0))


(defun rrb-notify-file-changed (start end)
  (if (not rrb-now-refactoring-flag)
      (rrb-delete-undo-files)))
;;;
;;; Undo
;;;
(defun rrb-undo ()
  "Undo the previous refactoring."
  (interactive)
  (rrb-declare-refactoring
   (let ((prev-undo-count (- rrb-undo-count 1)))
     (if (rrb-undo-base (rrb-make-undo-file-name prev-undo-count)
			(rrb-make-not-modified-file-name prev-undo-count))
	 (setq rrb-undo-count prev-undo-count)))))
					  
;;;
;;; Redo
;;;
(defun rrb-redo ()
  "Redo the previous undo."
  (interactive)
  (rrb-declare-refactoring
   (let ((next-undo-count (+ rrb-undo-count 1)))
     (if (rrb-undo-base (rrb-make-undo-file-name next-undo-count)
			(rrb-make-not-modified-file-name next-undo-count))
	 (setq rrb-undo-count next-undo-count)))))

;;;; Refactoring

;;;
;;; Refactoring: Rename local variable 
;;;
(defun rrb-comp-read-rename-local-variable ()
  "Read completely for Rename Local Variable."
  (rrb-comp-read-type-4 "--methods" (rrb-get-value-on-cursor "--method") 
			"Refactored method: " "--local-vars" "Old variable: "
			"New variable: "))

(defun rrb-rename-local-variable-impl (method old-var new-var)
  (rrb-do-refactoring "--rename-local-variable" method old-var new-var))

(defun rrb-rename-local-variable ()
  "Rename Local Variable."
  (interactive)
  (rrb-setup-refactoring 
   (apply 'rrb-rename-local-variable-impl (rrb-comp-read-rename-local-variable))))

;;; Refactoring: Rename method
(defun rrb-comp-read-rename-method ()
  "Read completely for Rename Method"
  (let ((compound-result (rrb-comp-read-recursively
	  "--classes"
	  (rrb-get-value-on-cursor "--class")
	  "Refactored classes(type just RET to finish): ")))
    (list
     compound-result
     (completing-read 
      "Old method: "
      (rrb-run-comp-info "--bare-methods" "--target" compound-result))
     (read-from-minibuffer "New method: "))))


(defun rrb-rename-method-impl (classes old-method new-method)
  (rrb-do-refactoring "--rename-method" classes old-method new-method))  

(defun rrb-rename-method ()
  "Rename Method."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-rename-method-impl (rrb-comp-read-rename-method))))

;;;
;;; Refactoring: Rename method all
;;;
(defun rrb-comp-read-rename-method-all ()
  "Read completely for Rename Method All"
  (rrb-comp-read-type-2 "--bare-methods" 
			(rrb-get-value-on-cursor "--bare-name")
			"Old method: " "New method: "))

(defun rrb-rename-method-all-impl (old-method new-method)
  (rrb-do-refactoring "--rename-method-all" old-method new-method))

(defun rrb-rename-method-all ()
  "Rename Method All."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-rename-method-all-impl (rrb-comp-read-rename-method-all))))

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
  "Extract Method."
  (interactive "r\nsNew method: ")
  (rrb-setup-refactoring 
   (rrb-do-refactoring "--extract-method"
		       (buffer-file-name)
		       new_method
		       (number-to-string (rrb-begin-line-num begin))
		       (number-to-string (rrb-end-line-num end)))))

;;;
;;; Refactoring: Rename instance variable
;;;
(defun rrb-comp-read-rename-instance-variable ()
  "Read completely for Rename Instance Variable."
  (rrb-comp-read-type-4 "--classes" 
			(rrb-get-value-on-cursor "--class") 
			"Refactord class: " 
			"--instance-vars"
			"Old instance variable: "
			"New instance variable: " ))

(defun rrb-rename-instance-variable-impl (ns old-var new-var)
  (rrb-do-refactoring "--rename-instance-variable" ns old-var new-var))

(defun rrb-rename-instance-variable ()
  "Rename Instance Variable."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-rename-instance-variable-impl (rrb-comp-read-rename-instance-variable))))

;;;; Refactoring: Rename class variable
(defun rrb-comp-read-rename-class-variable ()
  "Read completely for Rename Class Variable"
  (rrb-comp-read-type-4 "--classes"
			(rrb-get-value-on-cursor "--class") 
			"Refactord class: " 
			"--class-vars"
			"Old class variable: " 
			"New class variable: " ))

(defun rrb-rename-class-variable-impl (ns old-var new-var)
  (rrb-do-refactoring "--rename-class-variable" ns old-var new-var))

(defun rrb-rename-class-variable ()
  "Rename Class Variable."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-rename-class-variable-impl (rrb-comp-read-rename-class-variable))))

;;;
;;; Refactoring: Rename global variable
;;;
(defun rrb-comp-read-rename-global-variable ()
  "Read completely for Rename Global Variable."
  (rrb-comp-read-type-2 "--global-vars"
			""
			"Old global variable: " "New global variable: "))

(defun rrb-rename-global-variable-impl (old-var new-var)
  (rrb-do-refactoring "--rename-global-variable" old-var new-var))  

(defun rrb-rename-global-variable ()
  "Rename Global Variable."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-rename-global-variable-impl (rrb-comp-read-rename-global-variable))))

;;;; Refactoring: Rename constant
(defun rrb-comp-read-rename-constant ()
  "Completely read for Rename Constant."
  (rrb-comp-read-type-2 "--constants" 
			(rrb-get-value-on-cursor "--class")
			"Old constant: " "New constant: "))

(defun rrb-rename-constant-impl (old-const new-const)
    (rrb-do-refactoring "--rename-constant" old-const new-const))  

(defun rrb-rename-constant ()
  "Rename Constant."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-rename-constant-impl (rrb-comp-read-rename-constant))))

;;;
;;; Refactoring: Rename class
;;;
(defun rrb-comp-read-rename-class ()
  "Read completely for Rename Class."
  (rrb-comp-read-type-2 "--classes" 
			(rrb-get-value-on-cursor "--class")
			"Old class/module: " "New class/module: "))

(defun rrb-rename-class-impl (old-class new-class)
  (rrb-do-refactoring "--rename-constant" old-class new-class))
					;class name is a constant.
(defun rrb-rename-class ()
  "Rename Class or Module."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-rename-class-impl (rrb-comp-read-rename-class))))
	
;;;       
;;; Refactoring: Pull up method
;;;
(defun rrb-comp-read-pullup-method ()
  "Read completely for Pull Up Method."
  (rrb-comp-read-move-method "--methods" "Old Method: "
			"--classes"
			(rrb-get-value-on-cursor "--class") "New class: "))

(defun rrb-pullup-method-impl (old-method new-class)
  (rrb-do-refactoring "--pullup-method" old-method new-class
		      (buffer-file-name)
		      (number-to-string (rrb-current-line))))


(defun rrb-pullup-method ()
  "Pull Up Method."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-pullup-method-impl (rrb-comp-read-pullup-method))))

;;;		 
;;; Refactoring: Push down method
;;;
(defun rrb-comp-read-pushdown-method ()
  "Read completely for Push Down Method."
  (rrb-comp-read-move-method "--methods" "Old Method: "
			"--classes" 
			(rrb-get-value-on-cursor "--class") "New class: "))

(defun rrb-pushdown-method-impl (old-method new-class)
  (rrb-do-refactoring "--pushdown-method" old-method new-class 
		      (buffer-file-name)
		      (number-to-string (rrb-current-line))))

(defun rrb-pushdown-method ()
  "Push Down Method."
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-pushdown-method-impl (rrb-comp-read-pushdown-method))))
	
;;;	 
;;; Refactoring: Extract superclass
;;;
(defun rrb-comp-read-extract-superclass ()
  "Read completely for Extract Superclass."
  (list (completing-read "Location: "
			 (rrb-run-comp-info "--classes")
			 nil nil 
			 (rrb-get-value-on-cursor "--class"))
	(read-from-minibuffer "New Class: ")
	(rrb-comp-read-recursively "--classes" ""
				   "Targets(type just RET to finish): ")))

(defun rrb-extract-superclass-impl (namespace new-class targets)
  (rrb-do-refactoring "--extract-superclass" namespace new-class targets
		      (buffer-file-name)
		      (number-to-string (rrb-current-line))))
  

(defun rrb-extract-superclass ()
  "Extract Superclass"
  (interactive)
  (rrb-setup-refactoring
   (apply 'rrb-extract-superclass-impl (rrb-comp-read-extract-superclass))))

;;; rrb.el ends here