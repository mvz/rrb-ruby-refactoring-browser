;;; rrb.el - Ruby Refactoring Brower emacs interface

;; Copyright (C) 2003 OHBAYASHI Ippei

;; Author: OHBAYASHI Ippei <ohai@kmc.gr.jp>
;; Maintainer: OHBAYASHI Ippei <ohai@kmc.gr.jp>

(defconst rrb-io-splitter "\C-l")
(defconst rrb-io-terminator "-- END --")

(defvar rrb-input-buffer (get-buffer-create " *rrb-input*"))
(defvar rrb-output-buffer (get-buffer-create " *rrb-output*"))
(defvar rrb-error-buffer (get-buffer-create " *rrb-error*"))

(defun rrb-clean-buffer ()
  (set-buffer rrb-input-buffer)
  (erase-buffer)
  (set-buffer rrb-output-buffer)
  (erase-buffer)
  (set-buffer rrb-error-buffer)
  (erase-buffer))


(defun rrb-get-new-script ()
  (save-excursion
    (set-buffer rrb-output-buffer)
    (cadr (split-string (buffer-string) rrb-io-splitter))))

(defun rrb-rename-local-variable (method old-var new-var)
  (interactive "sRefactored method: \nsOld variable: \nsNew variable: ")
  (save-excursion
    (let ((src-buffer (current-buffer))
	  (error-code))
      
      (rrb-clean-buffer)

      ;; setup input string
      (set-buffer rrb-input-buffer)
      (insert (buffer-file-name src-buffer))
      (insert rrb-io-splitter)
      (insert-buffer-substring src-buffer)
      (insert rrb-io-splitter)
      (insert rrb-io-terminator)
      (insert rrb-io-splitter)
      ;; execute rrb
      (setq error-code (call-process-region (point-min) (point-max)
					    "rrb"
					    nil
					    (list rrb-output-buffer nil)
					    nil
					    "rename-local-variable"
					    method old-var new-var
					    "--stdin-stdout"))
      (if (= error-code 0)
	  ;; output result
	  (progn
	    (set-buffer src-buffer)
	    (erase-buffer)
	    (insert (rrb-get-new-script)))
	;; error
	(message "rrb: fail to refactor")))))
	
	
	

			   
      
  
  