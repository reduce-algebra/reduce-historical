%
% CLCOMP.SL - Incompatible Common Lisp compatibility
% 
% Author:      Eric Benson
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        12 April 1982
% Copyright (c) 1982 University of Utah
%

% These are Common Lisp compatiblity definitions that cause Standard Lisp
% to break.  Changes character definitions and redefines functions.

(imports '(useful common fast-vector))

(defmacro prog2 (first second . others)
  `(progn ,first (prog1 ,second ,@others)))

(remprop 'prog2 'compfn)

(defun char (s i) (igets s i))

(put 'char 'cmacro '(lambda (s i) (igets s i)))

% NTH is a problem, hasn't been dealt with yet
% Also MAP functions...

(comment "make backslash the escape character")

(setf IDEscapeChar* #\!\)
(setf (elt lispscantable* #\!\) 14)

(comment "Make percent a letter")

(setf (elt lispscantable* #\!%) 10)

(comment "Make semicolon start comments")

(setf (elt lispscantable* #\;) 12)

(comment "make bang a letter")

(setf (elt lispscantable* #\!!) 10)

(comment "Make colon the package character")

(setf PackageCharacter* #\:)
(setf (elt lispscantable* #\:) 16)

(comment "Add vertical bars for reading IDs")

(setf (elt lispscantable* #\|) 21)

(comment "#M and #Q mean if_maclisp and if_lispm")

(defun throw-away-next-form (channel qt)
  (ChannelReadTokenWithHooks channel)
  (ChannelReadTokenWithHooks channel))

(put '!#M 'LispReadMacro 'throw-away-next-form)
(put '!#Q 'LispReadMacro 'throw-away-next-form)

(push '(M . !#M) (get '!# (getv LispScanTable* 128)))
(push '(Q . !#Q) (get '!# (getv LispScanTable* 128)))

(comment "So we can add #+psl to maclisp code")

(push 'psl system_list*)
