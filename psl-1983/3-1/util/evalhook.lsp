;;;
;;; EVALHOOK.LSP - Support for special evaluation
;;; 
;;; Author:      Eric Benson
;;;	         Symbolic Computation Group
;;;              Computer Science Dept.
;;;              University of Utah
;;; Date:        30 March 1982
;;; Copyright (c) 1982 University of Utah
;;;

(defvar evalhook () "Variable to be funcalled if not () when Eval is called")

(fset 'old-eval (fsymeval 'eval))	; Redefine Eval

(defun eval (form)
  (if evalhook
      (let ((outer-evalhook evalhook))	; Bind evalhook to (), then funcall it
	   (let ((evalhook ())) (funcall outer-evalhook form)))
      (old-eval form)))

;;;; EVALHOOKFN - outer evaluation uses old-eval, inner evaluations use hook
(defun evalhookfn (form hook)
  (let ((evalhook hook))
    (old-eval form)))
