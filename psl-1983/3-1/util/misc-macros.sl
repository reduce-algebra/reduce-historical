% MISC-MACROS.SL - assorted useful macros
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

(defmacro funcall u `(apply ,(car u) (list ,@(cdr u))))

(copyd 'call 'funcall)

(defmacro eqfirst (u v) `(eqcar ,u ,v))

(defmacro bldid (s . args) `(intern (bldmsg ,s ,@args)))

(defmacro nary-concat u (expand u 'concat))

(defmacro-no-displace defstub (name . rst)
% quick, kludgy hack -- should be much better
  (let ((args (if (pairp rst) (pop rst))))
    `(de ,name ,args
       (stub-print ',name ',args (list ,@args))
       ,@rst
       (let ((*ContinuableError t)) (break)))))

(de stub-print (name arg-names actual-args)
  (errorprintf "Stub %w called with arguments:" name)
  (for (in u arg-names) (in v actual-args)
    (do (errorprintf "   %w: %p%n" u v)))
  (terpri))

(defmacro circular-list L
  `(let ((***CIRCULAR-LIST-ARG*** (list ,@L)))
     (nconc ***CIRCULAR-LIST-ARG*** ***CIRCULAR-LIST-ARG***)))

(defmacro nothing U nil) % Nary no-op returning nil; args not evaluated.

(defmacro make-list (N . rst)
  `(make-list-1 ,N ,(if (pairp rst) (car rst) nil)))

(de make-list-1 (N init)
  (for (from i 1 N) (collect init)))
