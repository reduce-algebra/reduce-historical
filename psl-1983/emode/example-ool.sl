%
% EXAMPLE-OOL.SL - Examples of the usage of OOL.SL, an "object oriented
%                  language".
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        24 July 1982
% Copyright (c) 1982 University of Utah
%

(setf generic-number
  (create_class
    (value NIL)  % Local state is a "value", initially NIL.
    % Message table
    (
     ((gets x) (setf value x))   % Assign argument to local state
     ((value) value)     % Return the local value

     % Raise to a power
     ((to-power n)
       (let ((p 1))
         (for (from i 1 n 1)
           % Repeatedly send a "times" message to our "value".
           (do (setf p (send_msg value `(times ,p))))
         p))))))

(setf complex-number
  (create_class
    (real-part 0 imag-part 0)

    % Message dictionary
    ((times y) ....???