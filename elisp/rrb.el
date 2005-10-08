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

;;;; Keybind for test new undo
;(global-unset-key "\C-cu")
;(global-set-key "\C-cu" 'rrb-undo)

;;;; Customizable variables
(defvar rrb-ruby-file-name-regexp "^.*\\.rb$"
  "*Regular expression matching ruby script file name")
(defvar rrb-first-line-of-ruby-file-regexp "^#!.*ruby.*$"
  "*Regular expression matching first line of ruby script")

(defvar rrb-tmp-file-base "rrblog"
  "*Base file name of error log file")

(defvar rrb-marshal-file-base "rrbmarshal"
  "*Base file name of marshal file")

;;;; Internal variables
(defconst rrb-io-splitter "\C-a")
(defconst rrb-io-terminator "-- END --")

(defvar rrb-input-buffer (get-buffer-create " *rrb-input*"))
(defvar rrb-output-buffer (get-buffer-create " *rrb-output*"))
(defvar rrb-error-buffer (get-buffer-create " *rrb-error*"))

(defvar rrb-marshal-file-name "")

(defvar rrb-undo-list nil)
(defvar rrb-pending-undo-list nil)

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

(defun rrb-times (count proc)
  (let ((i count))
    (while (> i 0)
      (funcall proc)
      (setq i (1- i)))))

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
    (substring (thing-at-point 'line) 0 nil)))
    
(defun rrb-do-refactoring (&rest args)
  "Do refactoring."
  (if (/= (apply 'rrb-run-process "rrb" 
		 (append args (list "--marshalin-stdout" rrb-marshal-file-name))) 0)
      (error "fail to refactor: %s" (rrb-error-message)))
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

(defun rrb-undo-set-undo-mark ()
  (save-current-buffer
    (setq rrb-undo-list
          (cons (mapcar (lambda (buffer)
                          (set-buffer buffer)
                          (rrb-undo-boundary)
                          (list buffer buffer-undo-list))
                        (rrb-all-ruby-script-buffer))
                rrb-undo-list))))
           
(defun rrb-prepare-refactoring ()
  (rrb-undo-set-undo-mark)
  (rrb-make-marshal-file))

(defun rrb-terminate-refactoring ()
  (rrb-delete-marshal-file))

(defmacro rrb-setup-refactoring (&rest body)
  `(progn
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

;;;; undo
(defun rrb-undo-count (undo-list undo-mark)
  (let ((count 0)
        (l undo-list))
    (while (not (or (eq l undo-mark) (eq l nil)))
      (if (eq (car l) nil)
          (setq count (1+ count)))
      (setq l (cdr l)))
    (if (eq l undo-mark)
        count
      0)))

(if (<= emacs-major-version 20)
    (defun rrb-do-undo (count)
      (unless (= count 0)
        (undo)
        (let ((last-command 'undo))
          (rrb-times (1- count) 'undo)))))

(if (>= emacs-major-version 21)
    (defun rrb-do-undo (count)
      (unless (= count 0)
        (undo count))))

(defun rrb-undo-boundary ()
  (cond ((null buffer-undo-list) (setq buffer-undo-list (list nil)))
        ((null (car buffer-undo-list)) nil) ; do nothing
        (t (setq buffer-undo-list (cons nil buffer-undo-list)))))


(defun rrb-undo-buffer (undo-markar)
  (set-buffer (car undo-markar))
  (undo-boundary)
  (let ((redo-markar buffer-undo-list))
    (rrb-do-undo (rrb-undo-count buffer-undo-list (cadr undo-markar)))
    (rrb-undo-boundary)
    (list (current-buffer) redo-markar)))
    
(defun rrb-undo ()
  (interactive)
  (save-current-buffer
    (unless (eq last-command 'rrb-undo)
      (setq rrb-pending-undo-list rrb-undo-list))
    (if (eq rrb-pending-undo-list nil)
        (error "Can't undo"))
    (setq rrb-undo-list (cons (mapcar 'rrb-undo-buffer
                                        (car rrb-pending-undo-list))
                                rrb-undo-list))
    (setq this-command 'rrb-undo)
    (setq rrb-pending-undo-list (cdr rrb-pending-undo-list))))
            
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

(provide 'rrb)
;;; rrb.el ends here
