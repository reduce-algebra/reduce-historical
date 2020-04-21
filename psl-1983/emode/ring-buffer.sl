%
% RING-BUFFER.SL - Ring Buffers
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 July 1982
%
% This file implements general ring buffers.
% This file requires COMMON, NSTRUCT.

% Modifications by William Galway:
%   "defun" -> "de" so TAGS can find things.
%   "setq" -> "setf"

(defstruct (ring-buffer)
  ring-buffer-vector	% Elements 1..N are used.
  ring-buffer-top-ptr	% Elements 1..Top are valid.
  ring-buffer-pointer	% Element Vector[POINTER] is current.
  )

(de ring-buffer-create (number-of-elements)
  (let ((rb (make-ring-buffer)))
    (setf (ring-buffer-vector rb) (mkvect number-of-elements))
    (setf (ring-buffer-top-ptr rb) 0)
    (setf (ring-buffer-pointer rb) 0)
    rb
    ))

(de ring-buffer-push (rb new-element)
  (let ((new-pointer (+ (ring-buffer-pointer rb) 1))
	(v (ring-buffer-vector rb))
	)
    (if (> new-pointer (upbv v))
      (setf new-pointer 1))
    (if (> new-pointer (ring-buffer-top-ptr rb))
      (setf (ring-buffer-top-ptr rb) new-pointer))
    (setf (ring-buffer-pointer rb) new-pointer)
    (setf (getv (ring-buffer-vector rb) new-pointer) new-element)
    new-element
    ))

(de ring-buffer-top (rb)
  % Returns NIL if the buffer is empty.
  (let* ((ptr (ring-buffer-pointer rb))
	 (v (ring-buffer-vector rb))
	 )
    (cond ((= ptr 0) NIL)
	  (t (getv v ptr)))))

(de ring-buffer-pop (rb)
  % Returns NIL if the buffer is empty.
  (let* ((ptr (ring-buffer-pointer rb))
	 (new-ptr (- ptr 1))
	 (v (ring-buffer-vector rb))
	 )
    (cond ((= ptr 0) NIL)
	  (t (if (= new-ptr 0) (setf new-ptr (ring-buffer-top-ptr rb)))
	     (setf (ring-buffer-pointer rb) new-ptr)
	     (getv v ptr)))))
