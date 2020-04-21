%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hash table package, rather general purpose.
%%% Author: Cris Perdue 8/25/82
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Edit by Cris Perdue,  9 Apr 1983 1159-PST
% Now uses fast, open-coded operations.
% Edit by Cris Perdue, 25 Feb 1983 1408-PST
% Cleaned up code and documentation for demo.
% Added NBuckets as an INITable variable.

(compiletime (load if data-machine numeric-operators
		   fast-vector))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Hash table flavor.
%%%
%%% This is an external chaining hash table.  Thus the table can never
%%% overflow and collision path length grows slowly, though search time
%%% can theoretically grow large.  The implementation includes ability
%%% to delete an association plus several other bells and whistles.
%%%
%%% Hash table instantiation can be as simple as:
%%% (make-instance 'hash).
%%% 
%%% Options to make-instance are:
%%% NBuckets:	Number of hash buckets to create initially.  Defaults
%%% 		to 100.
%%% HashFn:	Given a key, must return a fairly large pseudo-random
%%% 		integer.  Defaults to StrHash, for string keys.
%%% NullValue:	A value for Lookup to return if no association is found.
%%% 		Defaults to NIL.
%%% MaxFillRatio: A floating point number which is the maximum ratio of
%%% 		the number of associations to the number of buckets.
%%% 		If this ratio is reached, the table will be enlarged
%%% 		to make the ratio about .5.  Defaults to 2.0.
%%% KeyCopyFn:	Used by PutAssn.  In some cases when a new association
%%% 		is created one may want to copy the key so that it
%%% 		will be guaranteed not to be modified.  Defaults to
%%% 		a function that returns its argument without any copying.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gettable state:
%%%
%%% Usage:	Number of associations currently in the table.
%%% NullValue:  Value for Lookup to return if no association found.
%%%
%%% The following relate specifically to associations made via
%%% hash table:
%%% MaxFillRatio
%%% NBuckets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Operations:
%%%
%%% Present?(key)
%%%
%%% Returns T or NIL depending on whether there is an association with
%%% the given key.
%%% 
%%% Lookup(key)
%%%
%%% Returns the value associated with the key, or the NullValue for the
%%% table if no association exists.
%%% 
%%% PutAssn(key value)
%%%
%%% Makes an association between the key and value, replacing any old
%%% association.  The key may be copied if a new association is created,
%%% otherwise the copy of the key already stored continues to be used.
%%% Returns the value.
%%% 
%%% DeleteAssn(key)
%%%
%%% Deletes any association that may exist for the key.  Returns a value
%%% in the manner of Lookup.
%%% 
%%% ReSize(size)
%%%
%%% Rehashes the table into "size" buckets.  This operation is specific
%%% to associations made with hash tables.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Preliminaries: definitions, etc.

%(setq bitsperword 32)		% Hack to use from LISP.
				% Available as constant in SYSLISP.
				% In this package need only be no
				%  greater than actual bits per word.

(defmacro funcall (fn . args)
  `(apply ,fn (list ,@args)))

%%% Hash flavor definition.

(defflavor Hash
  (Table (NBuckets 100) (Usage 0) OverFlowLevel (MaxFillRatio 2.0)
	 (HashFn 'StrHash) (NullValue NIL) (CompareFn 'String=)
	 (KeyCopyFn 'no-op))
  ()
  (gettable-instance-variables NBuckets Usage NullValue MaxFillRatio)
  (initable-instance-variables
   NBuckets MaxFillRatio HashFn NullValue KeyCopyFn)
  )

(defmethod (Hash init) (init-plist)

  %% Perhaps the table size should be prime . . .
  (setf Table
    (MkVect (- NBuckets 1)))
  (while (leq MaxFillRatio .5)
    (ContinuableError
     0 "Set MaxFillRatio greater than .5 before continuing" t))
  (setf OverFlowLevel (Fix (* NBuckets MaxFillRatio))))

(defmethod (Hash Present?) (key)
  (let ((i (Hash$HashBucket Table (funcall HashFn Key))))
    (if (Ass CompareFn Key (igetv Table i))
	then t else nil)))

(defmethod (Hash Lookup) (key)
  (let ((i (Hash$HashBucket Table (funcall HashFn Key))))
    (let ((Entry (Ass CompareFn Key (igetv Table i))))
      (if Entry then (cdr Entry) else NullValue))))

(defmethod (Hash PutAssn) (key value)
  (let ((i (Hash$HashBucket Table (funcall HashFn Key))))
    (let ((Entry (Ass CompareFn Key (igetv Table i))))
      (if Entry then (RplacD Entry value)
	  else
	  (setf (igetv Table i)
		(cons (cons (funcall KeyCopyfn key) value)
		      (igetv Table i)))
	  (setf Usage (add1 Usage))
	  (if (not (< Usage OverFlowLevel)) then
              (=> Self resize (* 2 Usage))))))
  value)

(defmethod (Hash DeleteAssn) (key)
  (let ((i (Hash$HashBucket Table (funcall HashFn Key))))
    (let ((Entry (Ass CompareFn Key (igetv Table i))) (Value))
      (if Entry then
          (setq Value (cdr Entry))
	  (setf (igetv Table i) (DelQIP Entry (igetv Table i)))
	  (setf Usage (- Usage 1))
	  Value
	  else
	  NullValue))))

(defmethod (Hash MapAssn) (fn)
  (for (from i 0 (Size Table))
       (do (for (in a (igetv Table i))
		(do (funcall fn (car a)))))))

% Operations that are not basic

(defmethod (Hash ReSize) (new-size)
  (if (< new-size 1)
    (StdError (BldMsg "Hash table size of %p too small" new-size)))
  (let ((newtable
	 (mkvect (- new-size 1)))
	(oldtable table))
    (setf NBuckets new-size)
    (setf Table newtable)
    (setf OverFlowLevel (Fix (* NBuckets MaxFillRatio)))
    (setf Usage 0)
    (for (from i 0 (Size oldtable))
	 (do (for (in a (igetv oldtable i))
		  (do (=> Self PutAssn (car a) (cdr a))))))
    Self))

%%% Internal functions

(on fast-integers)

(defun Hash$HashBucket (table hashed-key) % Returns index of bucket
  (remainder hashed-key (isizev table)))

(defun no-op (x) x)

%%% Useful related function

(defun StrHash (S)	 % Compute hash function of string
  (let ((len (StrLen S))
	(AvailableBits (- (wconst InfBitLength) 8))
	(HashVal 0))
    (if (> Len AvailableBits) then
	(setq Len AvailableBits))
    (setq s (StrBase (StrInf s)))
    (for (from I 0 Len)
	 (do (setq HashVal
		   (LXOR HashVal
			 (LShift (Byte S I)
				 (- AvailableBits I))))))
    HashVal))

(off fast-integers)
