%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% RING-BUFFER.SL - General Ring Buffers
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 July 1982
% Revised:     16 November 1982
%
% 16-Nov-82 Alan Snyder
%   Recoded using OBJECTS package.  Added FETCH and ROTATE operations.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors))

(de ring-buffer-create (maximum-size)
  (make-instance 'ring-buffer 'maximum-size maximum-size))

(defflavor ring-buffer ((maximum-size 16)	% Maximum number of elements.
			vec			% Stores the elements.
			(size 0)		% Elements 0..size-1 are valid.
			(ptr -1)		% Element vec[ptr] is current.
			)
  ()
  (gettable-instance-variables maximum-size size)
  (initable-instance-variables maximum-size)
  )

(defmethod (ring-buffer init) (init-plist)
  (setf vec (mkvect (- maximum-size 1))))

(defmethod (ring-buffer push) (new-element)
  (let ((new-ptr (+ ptr 1)))
    (when (> new-ptr (vector-upper-bound vec))
      (setf new-ptr 0))
    (when (>= new-ptr size)
      (setf size (+ new-ptr 1)))
    (setf ptr new-ptr)
    (vector-store vec new-ptr new-element)
    new-element
    ))

(defmethod (ring-buffer top) ()
  % Returns NIL if the buffer is empty.
  (=> self fetch 0))

(defmethod (ring-buffer pop) ()
  % Returns NIL if the buffer is empty.
  (when (> size 0)
    (let ((old-element (vector-fetch vec ptr)))
      (setf ptr (- ptr 1))
      (when (< ptr 0) (setf ptr (- size 1)))
      old-element
      )))

(defmethod (ring-buffer fetch) (index)
  % Index 0 is the top element.
  % Index -1 is the next previous element, etc.
  % Index 1 is the most previous element, etc.
  % Returns NIL if the buffer is empty.

  (when (> size 0)
    (vector-fetch vec (ring-buffer-mod (+ ptr index) size))
    ))

(defmethod (ring-buffer rotate) (count)
  % Rotate -1 makes the next "older" element current (like POP), etc.
  % Rotate 1 makes the next "newer" element current, etc.

  (when (> size 0)
    (setf ptr (ring-buffer-mod (+ ptr count) size))
    ))

(de ring-buffer-mod (a b)
  (let ((remainder (// a b)))
    (if (>= remainder 0) remainder (+ b remainder))
    ))

% The following functions are defined for backwards compatibility:

(de ring-buffer-push (rb new-element)
  (=> rb push new-element))

(de ring-buffer-top (rb)
  (=> rb top))

(de ring-buffer-pop (rb)
  (=> rb pop))
