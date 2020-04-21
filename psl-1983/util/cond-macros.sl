% COND-MACROS.SL - convenient macros for conditional expressions
%
% Author:      Don Morrison
%	       Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        Wednesday, 12 May 1982
% Copyright (c) 1981 University of Utah

(defmacro if (predicate then . else)
  (cond ((null else) `(cond (,predicate ,then)))
	(t `(cond (,predicate ,then) (t . ,else)))))

(defmacro xor (u v) 
% done this way to both "semi-open-code" but not repeat the code for either
% arg; also evaluates args in the correct (left to right) order.
  `((lambda (***XOR-ARG***) (if ,v (not ***XOR-ARG***) ***XOR-ARG***)) ,u))

(defmacro when (p . c) `(cond (,p . ,c)))

(defmacro unless (p . c) `(cond ((not ,p) . ,c)))
