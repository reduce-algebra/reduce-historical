%%%% GET-HEAP-BOUNDS - looks up the addresses of the Syslisp variables
%%%   HeapLast and HeapLowerBound and makes it so that the Lisp function
%%%   HeapLast() returns the value of the variable HeapLast and the
%%%   Lisp function HeapLowerBound() returns the value of the variable
%%%   HeapLowerBound.  Dec-20 only.

(compiletime (load if-system syslisp))

% This depends on exactly the code generated for the CONS function
% on the Dec-20.  Very, very brittle code!

(fluid '(!%heaplast-address))

(if_system PDP10
(de get-heap-bounds ()
  (setq !%heaplast-address (inf (wgetv (getfcodepointer 'cons) 2)))))

(de heaplast ()
  (getmem !%heaplast-address))

% This depends on the order of declarations in PI:ALLOCATORS.RED and the
% way storage is assigned for Syslisp variables.

(de heaplowerbound ()
  (wgetv !%heaplast-address 2))

(get-heap-bounds)
