%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% REC.SL - Recursive Editing Functioons
%
% Author:	Jeffrey Soreff
%		Hewlett-Packard/CRC
% Date:		24 Jan 1983
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load extended-char fast-int objects))

% External variables used here:

(fluid '(recurse-mode nmode-current-buffer))

% Global variables defined here:

(fluid '(recurse-query recurse-query-answered))

% Recurse-Query will be T if the user leaves a recursive editing level
% with a "Y". It will be nil if the user leaves with an "N". In either
% of those cases recurse-query-answered will be set to T. If the user
% leaves the recursive editing level by some other means then
% recurse-query-answered will be NIL.

(de recursive-edit-y-or-n (buffer outer-message inner-message)
  % This function allows a user to make a yes or no decision about
  % some buffer, either before looking at it with the editor or while
  % editing within it. Before starting to edit the user is prompted
  % with the outer message. This function takes care of interpreting a
  % Y or N prior to editing and of providing a prompt (the outer
  % message) before editing. The call to recursive-edit takes care of
  % the prompt during editing and of interpreting a Y or N during
  % editing. This function returns a boolean value.
  (prog1
   (while t
     (write-message outer-message)
     (let ((ch (x-char-upcase (input-extended-character))))
       (when (= ch (x-char Y)) (exit T))
       (when (= ch (x-char N)) (exit NIL))
       (when (= ch (x-char C-R))
	 (recursive-edit buffer recurse-mode inner-message))
       (when recurse-query-answered (exit recurse-query))))
   (write-message "")))    

(de recursive-edit (new-buffer mode inner-message)
  % This function triggers the recursive editing loop, switching
  % buffers, setting the new buffer temporarily into a user selected
  % mode, and returning the buffer and mode to their old values after
  % the editing. This function returns a value only through global
  % variables, particularly recurse-query and recurse-query-answered.
  (let ((old-buffer nmode-current-buffer)
	(old-mode (=> new-buffer mode)))
    (=> new-buffer set-mode mode)
    (buffer-select new-buffer)
    (let ((old-message (write-message inner-message)))
      (setf recurse-query-answered NIL)
      (nmode-reader NIL)
      (write-message old-message))
    (=> new-buffer set-mode old-mode)
    (buffer-select old-buffer))) % Note: resets nmode-current-buffer
  
(de affirmative-exit ()
  % Returns T from a recursive editing mode, usually bound to Y.
  (setf recurse-query T)
  (setf recurse-query-answered T)
  (exit-nmode-reader))

(de negative-exit ()
  % Returns NIL from a recursive editing mode, usually bound to N.
  (setf recurse-query NIL)
  (setf recurse-query-answered T)
  (exit-nmode-reader))
