%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Objects.SL - A simple facility for object-oriented programming.
%
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        22 July 1982
% Revised:     16 February 1983
%
% 16-Feb-83 Alan Snyder
%  Add ev-send function.  Rename declare and undeclare to declare-flavor
%  and undeclare-flavor, to avoid conflict with common lisp declare.
% 30-Dec-82 Alan Snyder
%  General clean-up; rename internal functions and variables; document
%  method lookup functions; add method lookup trace facility.
% 1-Nov-82 Alan Snyder
%  Added Object-Type function.
% 27-Sept-82 Alan Snyder
%  Removed Variable-Table (which was available only at compile-time); made
%  Variable-Names available at both compile-time and load-time; now use
%  Variable-Names to "compile" method bodies.  Result: now can compile new
%  method bodies after loading a "compiled" flavor definition.
% 27-Sept-82 Alan Snyder
%  Evaluating (or loading) a DEFFLAVOR no longer clears the method table, if it
%  had been defined previously.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(Bothtimes (imports '(common fast-vector)))
(imports '(association strings))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NOTE: THIS FILE DEFINES MACROS.  IT MUST BE LOADED BEFORE ANY OF THESE
% FUNCTIONS ARE USED.  The recommended way to do this is to put the statement
% (BothTimes (load objects)) at the beginning of your source file.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% Summary of Public Functions:
%   
% (defflavor flavor-name (var1 var2 ...) (flav1 flav2 ...) option1 option2 ...)
% (defmethod (flavor-name message-name) (arg1 arg2 ...) form1 form2 ...)
%
% (make-instance 'flavor-name 'var1 value1 ...)
%
% (=> foo message-name arg1 arg2 ...)
%
% (send foo 'message-name arg1 arg2 ...)
% (lexpr-send foo 'message-name arg1 arg2 ... rest-arg-list)
% (lexpr-send-1 foo 'message-name arg-list)
% (ev-send foo 'message-name arg-list) {EXPR form}
%
% (send-if-handles foo 'message-name arg1 arg2 ...)
% (lexpr-send-if-handles foo 'message-name arg1 arg2 ... rest-arg-list)
% (lexpr-send-1-if-handles foo 'message-name arg-list)
%
% (instantiate-flavor 'flavor-name init-list)
%
% (object-type x)  --- returns the type of an object, or NIL if not an object
%
% (object-get-handler x message-name) -- lookup method function (see below)
% (object-get-handler-quietly x message-name)
%
% (trace-method-lookups) - start recording stats about method lookup
% (untrace-method-lookups) - stop recording stats about method lookup
% (print-method-lookup-info) - untrace and print accumulated stats
%
% (declare-flavor flavor var1 var2 ...)   NOTE: see warnings below!
% (undeclare-flavor var1 var2 ...)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Private Constants, Fluids, and Macros (mere mortals should ignore these)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '($defflavor-expansion-context
	 $object-number-of-reserved-slots
	 $object-flavor-slot
	 $object-debug-slot
	 $defflavor-option-table
	 $method-lookup-stats
	 ))

(setf $defflavor-expansion-context NIL)
(BothTimes (progn
	    (setf $object-number-of-reserved-slots 2)
	    (setf $object-flavor-slot 0)
	    (setf $object-debug-slot 1)
	    ))
(setf $defflavor-option-table
  (list
   (cons 'gettable-instance-variables '$defflavor-do-gettable-option)
   (cons 'settable-instance-variables '$defflavor-do-settable-option)
   (cons 'initable-instance-variables '$defflavor-do-initable-option)
   ))

% Note the free variable FLAVOR-NAME in this macro:
(defmacro $defflavor-error (format . arguments)
  `(ContinuableError 1000 (BldMsg ,(string-concat "DEFFLAVOR %w: " format)
			          flavor-name . ,arguments) NIL))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Public Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% DEFFLAVOR - Define a new flavor of Object
%   
% Examples:
%
% (defflavor complex-number (real-part imaginary-part) ())
%
% (defflavor complex-number (real-part imaginary-part) ()
%    gettable-instance-variables
%    initable-instance-variables
%    )
%
% (defflavor complex-number ((real-part 0.0)
%			   (imaginary-part 0.0)
%			   )
%    ()
%    gettable-instance-variables
%    (settable-instance-variables real-part)
%    )
%
% An object is represented by a vector; instance variables are allocated
% specific slots in the vector.  Do not use names like "IF" or "WHILE" for
% instance varibles: they are translated freely within method bodies (see
% DEFMETHOD).  Initial values for instance variables may be specified as
% arguments to MAKE-INSTANCE, or as initializing expressions in the variable
% list, or may be supplied by an INIT method (see MAKE-INSTANCE).
% Uninitializied instance variables are bound to *UNBOUND*.
%
% The component flavor list currently must be null.  Recognized options are:
%
%  (GETTABLE-INSTANCE-VARIABLES var1 var2 ...)
%  (SETTABLE-INSTANCE-VARIABLES var1 var2 ...) 
%  (INITABLE-INSTANCE-VARIABLES var1 var2 ...)
%  GETTABLE-INSTANCE-VARIABLES  [make all instance variables GETTABLE]
%  SETTABLE-INSTANCE-VARIABLES  [make all instance variables SETTABLE]
%  INITABLE-INSTANCE-VARIABLES  [make all instance variables INITABLE]
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro defflavor (flavor-name variable-list flavor-list . options-list)
  (prog (var-names		% List of valid instance variable names
	 init-code		% body of DEFAULT-INIT method
	 describe-code		% body of DESCRIBE method
	 defmethod-list		% list of created DEFMETHODs
	 var-options		% AList mapping var names to option list
	 initable-vars		% list of INITABLE instance variables
	 )
    (desetq (var-names init-code)
	    ($defflavor-process-varlist flavor-name variable-list)
	    )
    (setf describe-code ($defflavor-build-describe flavor-name var-names))
    (setf var-options
      ($defflavor-process-options-list flavor-name var-names options-list)
      )
    (setf defmethod-list ($defflavor-create-methods flavor-name var-options))
    (setf initable-vars ($defflavor-initable-vars flavor-name var-options))

    (put flavor-name 'variable-names var-names)
    (setf defmethod-list
      (cons `(defmethod (,flavor-name default-init) () . ,init-code)
	    defmethod-list))
    (setf defmethod-list
      (cons `(defmethod (,flavor-name describe) () . ,describe-code)
	    defmethod-list))
    (if flavor-list
      ($defflavor-error "Component Flavors not implemented")
      )

    % The previous actions happen at compile or dskin time.
    % The following actions happen at dskin or load time.

    (return `(progn
	      (if (not (get ',flavor-name 'method-table))
		(put ',flavor-name 'method-table (association-create)))
	      (put ',flavor-name 'instance-vector-size
		   ,(+ #.$object-number-of-reserved-slots (length var-names)))
	      (put ',flavor-name 'variable-names ',var-names)
	      (put ',flavor-name 'initable-variables ',initable-vars)
	      ,@defmethod-list
	      '(flavor ,flavor-name) % for documentation only
	      ))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% DEFMETHOD - Define a method on an existing flavor.
%   
% Examples:
%
% (defmethod (complex-number real-part) ()
%   real-part)
%
% (defmethod (complex-number set-real-part) (new-real-part)
%   (setf real-part new-real-part))
%
% The body of a method can freely refer to the instance variables of the flavor
% and can set them using SETF.  Each method defines a function FLAVOR$METHOD
% whose first argument is SELF, the object that is performing the method.  All
% references to instance variables (except within vectors or quoted lists) are
% translated to an invocation of the form (IGETV SELF n).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro defmethod ((flavor-name method-name) argument-list . body)
  (setf argument-list (cons 'self argument-list))
  (let ((function-name ($defflavor-function-name flavor-name method-name)))
    (put function-name 'source-code `(lambda ,argument-list . ,body))
    (let ((new-code ($create-method-source-code function-name flavor-name)))

      % The previous actions happen at compile or dskin time.
      % The following actions happen at dskin or load time.

      `(progn
        ($flavor-define-method ',flavor-name ',method-name ',function-name)
        (putd ',function-name 'expr ',new-code)
        '(method ,flavor-name ,method-name) % for documentation only
        ))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% => - Convenient form for sending a message
%   
% Examples:
%
% (=> r real-part)
%
% (=> r set-real-part 1.0)
%
% The message name is not quoted.  Arguments to the method are supplied as
% arguments to =>.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro => (object message-name . arguments)
  `(send ,object ',message-name . ,arguments))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% SEND - Send a Message (Evaluated Message Name)
%   
% Examples:
%
% (send r 'real-part)
%
% (send r 'set-real-part 1.0)
%
% Note that the message name is quoted.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro send (target-form method-form . argument-forms)

  % If the method name is known at compile time (i.e., the method-form is of
  % the form (QUOTE <id>)) and the target is either SELF (within the body of a
  % DEFMETHOD) or a variable which has been declared (using DECLARE-FLAVOR),
  % then optimize the form to a direct invocation of the method function.

  (if (and (PairP method-form)
	   (eq (car method-form) 'quote)
	   (not (null (cdr method-form)))
	   (IdP (cadr method-form))
	   )
    (let ((method-name (cadr method-form)))
      (cond ((and (eq target-form 'self) $defflavor-expansion-context)
	     ($self-send-expansion method-name argument-forms))
	    ((and (IdP target-form) (get target-form 'declared-type))
	     ($direct-send-expansion target-form method-name argument-forms))
	    (t ($normal-send-expansion target-form method-form argument-forms))
	    ))
    ($normal-send-expansion target-form method-form argument-forms)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% SEND-IF-HANDLES - Conditionally Send a Message (Evaluated Message Name)
%   
% Examples:
%
% (send-if-handles r 'real-part)
%
% (send-if-handles r 'set-real-part 1.0)
%
% SEND-IF-HANDLES is like SEND, except that if the object defines no method
% to handle the message, no error is reported and NIL is returned.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro send-if-handles (object message-name . arguments)
  `(let* ((***SELF*** ,object)
	  (***HANDLER*** (object-get-handler-quietly ***SELF*** ,message-name))
	  )
     (and ***HANDLER*** (apply ***HANDLER*** (list ***SELF*** ,@arguments)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% LEXPR-SEND - Send a Message (Explicit "Rest" Argument List)
%   
% Examples:
%
% (lexpr-send foo 'bar a b c list)
%
% The last argument to LEXPR-SEND is a list of the remaining arguments.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro lexpr-send (object message-name . arguments)
  (if arguments
    (let ((explicit-args (reverse (cdr (reverse arguments))))
	  (last-arg (LastCar arguments))
	  )
      (if explicit-args
        `(lexpr-send-1 ,object ,message-name
		       (append (list ,@explicit-args) ,last-arg))
	`(lexpr-send-1 ,object ,message-name ,last-arg)
	)
      )
    `(let ((***SELF*** ,object))
       (apply (object-get-handler ***SELF*** ,message-name)
	      (list ***SELF***)))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% LEXPR-SEND-IF-HANDLES 
%   
% This is the same as LEXPR-SEND, except that no error is reported
% if the object fails to handle the message.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro lexpr-send-if-handles (object message-name . arguments)
  (if arguments
    (let ((explicit-args (reverse (cdr (reverse arguments))))
	  (last-arg (LastCar arguments))
	  )
      (if explicit-args
        `(lexpr-send-1-if-handles ,object ,message-name
				  (append (list ,@explicit-args) ,last-arg))
	`(lexpr-send-1-if-handles ,object ,message-name ,last-arg)
	)
      )
    `(let* ((***SELF*** ,object)
	    (***HANDLER***
	     (object-get-handler-quietly ***SELF*** ,message-name))
	    )
       (and ***HANDLER*** (apply ***HANDLER*** (list ***SELF***))))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% LEXPR-SEND-1 - Send a Message (Explicit Argument List)
%   
% Examples:
%
% (lexpr-send-1 r 'real-part nil)
%
% (lexpr-send-1 r 'set-real-part (list 1.0))
%
% Note that the message name is quoted and that the argument list is passed as a
% single argument to LEXPR-SEND-1.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro lexpr-send-1 (object message-name argument-list)
  `(let ((***SELF*** ,object))
     (apply (object-get-handler ***SELF*** ,message-name)
	    (cons ***SELF*** ,argument-list))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% EV-SEND - EXPR form of LEXPR-SEND-1
%   
% EV-SEND is just like LEXPR-SEND-1, except that it is an EXPR instead of
% a MACRO.  Its sole purpose is to be used as a run-time function object,
% for example, as a function argument to a function.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de ev-send (obj msg arg-list)
  (lexpr-send-1 obj msg arg-list)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% LEXPR-SEND-1-IF-HANDLES
%   
% This is the same as LEXPR-SEND-1, except that no error is reported if the
% object fails to handle the message.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro lexpr-send-1-if-handles (object message-name argument-list)
  `(let* ((***SELF*** ,object)
	  (***HANDLER*** (object-get-handler-quietly ***SELF*** ,message-name))
	  )
     (and ***HANDLER*** (apply ***HANDLER*** (cons ***SELF*** ,argument-list)))
     ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% MAKE-INSTANCE - Create a new instance of a flavor.
%   
% Examples:
%
% (make-instance 'complex-number)
% (make-instance 'complex-number 'real-part 0.0 'imaginary-part 1.0)
%
% MAKE-INSTANCE accepts an optional initialization list, consisting of
% alternating pairs of instance variable names and corresponding initial values.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro make-instance (flavor-name . init-plist)
  `(instantiate-flavor ,flavor-name
		       (list . ,init-plist)
		       ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
% INSTANTIATE-FLAVOR
%   
% This is the same as MAKE-INSTANCE, except that the initialization list is
% provided as a single (required) argument.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun instantiate-flavor (flavor-name init-plist)
  (let* ((vector-size (get flavor-name 'instance-vector-size)))
    (if vector-size
      (let* ((object (MkVect (- vector-size 1)))
	     )
	(setf (igetv object #.$object-flavor-slot) flavor-name)
	(setf (igetv object #.$object-debug-slot) NIL)
	(for (from i #.$object-number-of-reserved-slots (- vector-size 1) 1)
	     (do (iputv object i '*UNBOUND*))
	     )
	($object-perform-initialization object init-plist)
	(send-if-handles object 'default-init)
	(send-if-handles object 'init init-plist)
	object
	)
      (ContError 0 "Attempt to instantiate undefined flavor: %w"
		 flavor-name (Instantiate-Flavor flavor-name init-plist))
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Object-Type
%
% The OBJECT-TYPE function returns the type (an ID) of the specified object, or
% NIL, if the argument is not an object.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun object-type (object)
  (if (and (VectorP object) (> (UpbV object) 1))
    (let ((flavor-name (igetv object #.$object-flavor-slot)))
      (if (IdP flavor-name) flavor-name)
      )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Method Lookup
%
% The following functions return method functions given an object and a message
% name.  The returned function can be invoked, passing the object as the first
% argument and the message arguments as the remaining arguments.  For example,
% the expression (=> foo gorp a b c) is equivalent to:
%
%   (apply (object-get-handler foo 'gorp) (list foo a b c))
%
% It can be useful for efficiency reasons to lookup a method function once and
% then apply it many times to the same object.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun object-get-handler (object message-name)
  % Returns the method function that implements the specified message when sent
  % to the specified object.  If no such method exists, generate a continuable
  % error.

  (let ((flavor-name (object-type object)))
    (cond
     (flavor-name
      (let ((function-name ($flavor-fetch-method flavor-name message-name)))
	(or function-name
	    (ContError 1000
		       "Flavor %w has no method %w."
		       flavor-name
		       message-name
		       (object-get-handler object message-name)
		       ))))
     (t (ContError 1000
		   "Object %w cannot receive messages."
		   object
		   (object-get-handler object message-name)
		   )))))

(defun object-get-handler-quietly (object message-name)
  % Returns the method function that implements the specified message when sent
  % to the specified object, if it exists, otherwise returns NIL.

  (let ((flavor-name (object-type object)))
    (if flavor-name
      ($flavor-fetch-method flavor-name message-name))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Method Lookup Tracing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de trace-method-lookups ()
  % Begin accumulating information about method lookups (invocations of
  % object-get-handler).  The statistics are reset.
  (setf $method-lookup-stats (association-create))
  (copyd 'object-get-handler '$traced-object-get-handler)
  )

(de untrace-method-lookups ()
  % Stop accumulating information about method lookups.
  (copyd 'object-get-handler '$untraced-object-get-handler)
  )

(de print-method-lookup-info ()
  % Stop accumulating information about method lookups and print a summary of
  % the accumulated information about method lookups.  This summary shows which
  % methods were looked up and how many times each method was looked up.

  (untrace-method-lookups)
  (load gsort stringx)
  (setf $method-lookup-stats (gsort $method-lookup-stats '$method-info-sortfn))
  (for (in pair $method-lookup-stats)
       (do (printf "%w  %w%n"
		   (string-pad-left (bldmsg "%w" (cdr pair)) 6)
		   (car pair))))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% DECLARE-FLAVOR
%
% *** Read these warnings carefully! ***
%
% The DECLARE-FLAVOR macro allows you to declare that a specific symbol is
% bound to an object of a specific flavor.  This allows the flavors
% implementation to eliminate the run-time method lookup normally associated
% with sending a message to that variable, which can result in an appreciable
% improvement in execution speed.  This feature is motivated solely by
% efficiency considerations and should be used ONLY where the performance
% improvement is critical.
% 
% Details: if you declare the variable X to be bound to an object of flavor
% FOO, then WITHIN THE CONTEXT OF THE DECLARATION (see below), expressions of
% the form (=> X GORP ...)  or (SEND X 'GORP ...)  will be replaced by function
% invocations of the form (FOO$GORP X ...).  Note that there is no check made
% that the flavor FOO actually contains a method GORP.  If it does not, then a
% run-time error "Invocation of undefined function FOO$GORP" will be reported.
% 
% WARNING: The DECLARE-FLAVOR feature is not presently well integrated with
% the compiler.  Currently, the DECLARE-FLAVOR macro may be used only as a
% top-level form, like the PSL FLUID declaration.  It takes effect for all
% code evaluated or compiled henceforth.  Thus, if you should later compile a
% different file in the same compiler, the declaration will still be in
% effect!  THIS IS A DANGEROUS CROCK, SO BE CAREFUL!  To avoid problems, I
% recommend that DECLARE-FLAVOR be used only for uniquely-named variables.
% The effect of a DECLARE-FLAVOR can be undone by an UNDECLARE-FLAVOR, which
% also may be used only as a top-level form.  Therefore, it is good practice
% to bracket your code in the source file with a DECLARE-FLAVOR and a
% corresponding UNDECLARE-FLAVOR.
%
% Here are the syntactic details:
%
% (DECLARE-FLAVOR FLAVOR-NAME VAR1 VAR2 ...)
% (UNDECLARE-FLAVOR VAR1 VAR2 ...)
%
% *** Did you read the above warnings??? ***
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defmacro declare-flavor (flavor-name . variable-names)
  (prog () % This macro returns NIL!
    (if (not (IdP flavor-name))
      (StdError
       (BldMsg "Flavor name in DECLARE-FLAVOR is not an ID: %p" flavor-name))
      % else
      (for (in var-name variable-names)
	   (do (if (not (IdP var-name))
	         (StdError (BldMsg
			    "Variable name in DECLARE-FLAVOR is not an ID: %p"
			    var-name))
		 % else
		 (put var-name 'declared-type flavor-name)
		 )))
      )))

(dm undeclare-flavor (form)
  (prog () % This macro returns NIL!
    (for (in var-name (cdr form))
	 (do (if (not (IdP var-name))
	       (StdError (BldMsg
			  "Variable name in UNDECLARE-FLAVOR is not an ID: %p"
			  var-name))
	       % else
	       (remprop var-name 'declared-type)
	       )))
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Representation Information:
%
% (You don't need to know any of this to use this stuff.)
%
% A flavor-name is an ID.  It has the following properties:
%
% VARIABLE-NAMES	A list of the instance variables of the flavor, in
%			order of their location in the instance vector.  This
%			property exists at compile time, dskin time, and load
%			time.
%
% INITABLE-VARIABLES	A list of the instance variables that have been declared
%			to be INITABLE.  This property exists at dskin time and
%			at load time.
%
% METHOD-TABLE		An association list mapping each method name (ID)
%			defined for the flavor to the corresponding function
%			name (ID) that implements the method.  This property
%			exists at dskin time and at load time.
%
% INSTANCE-VECTOR-SIZE	An integer that specifies the number of elements in the
%			vector that represents an instance of this flavor.  This
%			property exists at dskin time and at load time.  It is
%			used by MAKE-INSTANCE.
%
% The function that implements a method has a name of the form FLAVOR$METHOD.
% Each such function ID has the following properties:
%
% SOURCE-CODE		A list of the form (LAMBDA (SELF ...) ...) which is the
%			untransformed source code for the method.  This property
%			exists at compile time and dskin time.
%
% Implementation Note:
%
% A tricky aspect of this code is making sure that the right things happen at
% the right time.  When a source file is read and evaluated (using DSKIN), then
% everything must happen at once.  However, when a source file is compiled to
% produce a FASL file, then some actions must be performed at compile-time,
% whereas other actions are supposed to occur when the FASL file is loaded.
% Actions to occur at compile time are performed by macros; actions to occur at
% load time are performed by the forms returned by macros.
%
% Another goal of the implementation is to avoid consing whenever possible
% during method invocation.  The current scheme prefers to compile into (APPLY
% HANDLER (LIST args...)), for which the PSL compiler will produce code that
% performs no consing.
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defun $object-perform-initialization (object init-plist)

  % Perform the initialization of instance variables in OBJECT as specified by
  % the INIT-PLIST, which contains alternating instance variable names and
  % initializing values.

  (let* ((flavor-name (igetv object #.$object-flavor-slot))
	 (initable-vars (get flavor-name 'initable-variables))
	 (variable-names (get flavor-name 'variable-names))
	 name value
	 )
    (while init-plist
      (setf name (car init-plist))
      (setf init-plist (cdr init-plist))
      (if init-plist
	(progn (setf value (car init-plist))
	       (setf init-plist (cdr init-plist)))
	(setf value nil)
	)
      (if (memq name initable-vars)
	(iputv object
	       ($object-lookup-variable-in-list variable-names name)
	       value)
	(ContinuableError 1000
			  (BldMsg "%p not an initable instance variable of flavor %w"
				  name
				  flavor-name)
			  NIL)
	))))

(defun $object-lookup-variable-in-list (variable-names name)
  (for (in v-name variable-names)
       (for i #.$object-number-of-reserved-slots (+ i 1))
       (do (if (eq v-name name) (exit i)))
       (returns nil)
       ))

(defun $substitute-for-symbols (U var-names)
  % Substitute in U for all unquoted instances of the symbols defined in
  % Var-Names.  Also, change SETQ to SETF in forms, since only SETF can handle
  % the substituted forms.

  (cond
   ((IdP U)
    (let ((address ($object-lookup-variable-in-list var-names U)))
      (if address (list 'igetv 'self address) U)
      ))
   ((PairP U)
    (cond
     ((eq (car U) 'quote) U)
     ((eq (car U) 'setq)
      (cons 'setf ($substitute-for-symbols (cdr U) var-names)))
     (t (cons ($substitute-for-symbols (car U) var-names)
	      ($substitute-for-symbols (cdr U) var-names)))
     )
    )
   (t U)
   ))

(defun $flavor-define-method (flavor-name method-name function-name)
  (let ((method-table (get flavor-name 'method-table)))
    (association-bind method-table method-name function-name)))
(copyd 'flavor-define-method '$flavor-define-method) % for compatibility!

(defun $flavor-fetch-method (flavor-name method-name)
  % Returns NIL if the method is undefined.
  (let* ((method-table (get flavor-name 'method-table))
	 (assoc-pair (atsoc method-name method-table))
	 )
    (if assoc-pair (cdr assoc-pair) nil)))

(defun $create-method-source-code (function-name flavor-name)
  (let ((var-names (get flavor-name 'variable-names))
	(source-code (get function-name 'source-code))
        ($defflavor-expansion-context flavor-name) % FLUID variable!
	)
    ($substitute-for-symbols (MacroExpand source-code) var-names)
    ))

(defun $defflavor-process-varlist (flavor-name variable-list)

  % Process the instance variable list of a DEFFLAVOR.  Create a list of valid
  % instance variable names and a list of forms to perform default
  % initialization of instance variables.

  (prog (var-names default-init-code init-form v)
    (for (in v-entry variable-list) (do
				     (cond ((and (PairP v-entry) (IdP (car v-entry)))
					    (setf v (car v-entry))
					    (setf init-form (cdr v-entry))
					    (if init-form (setf init-form (car init-form)))
					    (setf init-form `(if (eq ,v '*UNBOUND*) (setf ,v ,init-form)))
					    (setf default-init-code (aconc default-init-code init-form))
					    )
					   ((IdP v-entry) (setf v v-entry))
					   (t ($defflavor-error "Bad item in variable list: %p" v-entry)
					      (setf v NIL)
					      )
					   )
				     (if v (setf var-names (aconc var-names v)))
				     ))
    (return (list var-names default-init-code))))

(defun $defflavor-build-describe (flavor-name var-names)
  % Return a list of forms that print a description of an instance.

  (let ((describe-code
	 `((printf ,(string-concat "An object of flavor "
				   (id2string flavor-name)
				   ", has instance variable values:%n")))))
    (for (in v var-names)
	 (do
	  (setf describe-code
	    (aconc describe-code `(printf "  %w: %p%n" ',v ,v)))
	  ))
    (aconc describe-code NIL)
    ))

(defun $defflavor-process-options-list (flavor-name var-names options-list)
  % Return an AList mapping var-names to a list of options
  (let ((var-options (association-create)))
    (for (in option options-list)
	 (do ($defflavor-process-option flavor-name var-names
					var-options option)
	     ))
    var-options
    ))

(defun $defflavor-process-option (flavor-name var-names var-options option)
  % Process the option by modifying the AList VAR-OPTIONS.
  (let (option-keyword option-arguments)
    (cond ((PairP option)
	   (setf option-keyword (car option))
	   (setf option-arguments (cdr option))
	   )
	  ((IdP option)
	   (setf option-keyword option)
	   )
	  (t ($defflavor-error "Bad item in options list: %p" option)
	     (setf option-keyword '*NONE*)
	     )
	  )
    (when (neq option-keyword '*NONE*)
      (let ((pair (atsoc option-keyword $defflavor-option-table)))
        (if (null pair)
	  ($defflavor-error "Bad option in options list: %w" option)
	  (apply (cdr pair)
		 (list flavor-name var-names var-options option-arguments))
	  )))))

(defun $defflavor-do-gettable-option (flavor-name var-names var-options args)
  ($defflavor-insert-keyword flavor-name var-names var-options args 'GETTABLE)
  )

(defun $defflavor-do-settable-option (flavor-name var-names var-options args)
  ($defflavor-insert-keyword flavor-name var-names var-options args 'SETTABLE)
  )

(defun $defflavor-do-initable-option (flavor-name var-names var-options args)
  ($defflavor-insert-keyword flavor-name var-names var-options args 'INITABLE)
  )

(defun $defflavor-insert-keyword (flavor-name var-names var-options args key)
  (if (null args) (setf args var-names)) % default: applies to all variables
  (for (in var args) % for each specified instance variable
       (do
	(if (not (memq var var-names))
	  ($defflavor-error "%p (in keyword option) not a variable." var)
	  % else
	  (let ((pair (atsoc var var-options)))
	    (when (null pair)
	      (setf pair (cons var nil))
	      (aconc var-options pair)
	      )
	    (setf (cdr pair) (adjoinq key (cdr pair)))
	    )))))

(defun $defflavor-define-access-function (flavor-name var-name)
  `(defmethod (,flavor-name ,var-name) () ,var-name))

(defun $defflavor-define-update-function (flavor-name var-name)
  (let ((method-name (intern (string-concat "SET-" (id2string var-name)))))
    `(defmethod (,flavor-name ,method-name) (new-value)
       (setf ,var-name new-value))))

(defun $defflavor-create-methods (flavor-name var-options)
  % Return a list of DEFMETHODs for GETTABLE and SETTABLE instance variables.

  (let ((defmethod-list))
    (for (in pair var-options)
	 (do
	  (let ((var-name (car pair))
		(keywords (cdr pair))
		)
	    (if (or (memq 'GETTABLE keywords) (memq 'SETTABLE keywords))
	      (setf defmethod-list
		(cons ($defflavor-define-access-function flavor-name var-name)
		      defmethod-list
		      )))
	    (if (memq 'SETTABLE keywords)
	      (setf defmethod-list
		(cons ($defflavor-define-update-function flavor-name var-name)
		      defmethod-list
		      )))
	    )))
    defmethod-list
    ))

(defun $defflavor-initable-vars (flavor-name var-options)
  % Return a list containing the names of instance variables that have been
  % declared to be INITable.
  (for (in pair var-options)
       (when (and (PairP pair)
		  (or (memq 'INITABLE (cdr pair))
		      (memq 'SETTABLE (cdr pair))
		      )))
       (collect (car pair))
       )
  )

(de $defflavor-function-name (flavor-name method-name)
  (intern (string-concat (id2string flavor-name) "$" (id2string method-name))))

(de $normal-send-expansion (target-form method-form argument-forms)
  `(let ((***SELF*** ,target-form))
     (apply (object-get-handler ***SELF*** ,method-form)
            (list ***SELF*** ,@argument-forms))))

(de $self-send-expansion (method-name argument-forms)
  (cons ($defflavor-function-name $defflavor-expansion-context method-name)
        (cons 'self argument-forms)))

(de $direct-send-expansion (target-id method-name argument-forms)
  (let ((target-type (get target-id 'declared-type)))
    (cons ($defflavor-function-name target-type method-name)
          (cons target-id argument-forms))))

(copyd '$untraced-object-get-handler 'object-get-handler)

(de $traced-object-get-handler (obj method-name)
  (let* ((result ($untraced-object-get-handler obj method-name))
	 (count (association-lookup $method-lookup-stats result))
	 )
    (association-bind $method-lookup-stats result (if count (+ count 1) 1))
    result
    ))

(de $method-info-sortfn (m1 m2)
  (numbersortfn (cdr m2) (cdr m1))
  )
