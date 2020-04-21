%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Association.SL - Mutable Association Lists
%
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        21 July 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common))

(defun association-create ()
  % Create an empty association list (that is mutable!).
  (list (cons '*DUMMY* '*DUMMY*)))

(defun association-bind (alist indicator value)
  % Change or extend the ALIST to map INDICATOR to VALUE.
  (let ((pair (atsoc indicator alist)))
    (if pair
	(rplacd pair value)
	% ELSE
	(aconc alist (cons indicator value))
	(setq pair (car alist))
	(if (and (eq (car pair) '*DUMMY*)
		 (eq (cdr pair) '*DUMMY*))
	    (progn (rplacw pair (cadr alist)) (rplacd alist (cddr alist)))
	    )
	)))

(defun association-lookup (alist indicator)
  % Return the value attached to the given indicator (using EQ for
  % comparing indicators).  If there is no attached value, return NIL.

  (let ((pair (atsoc indicator alist)))
    (if pair (cdr pair) NIL)))

(defmacro map-over-association ((alist indicator-var value-var) . body)
  % Execute the body once for each indicator in the alist, binding
  % the specified indicator-var to the indicator and the specified
  % value-var to the attached value.  Return the result of the body
  % (implicit PROGN).

  `(for (in ***PAIR*** ,alist)
	(with ***RESULT***)
	(do (let ((,indicator-var (car ***PAIR***))
		  (,value-var (cdr ***PAIR***))
		  )
	      (cond ((not (eq ,indicator-var '*DUMMY*))
		     (setf ***RESULT*** (progn ,@body))
		       ))))
	(returns ***RESULT***)
	))
