% 
% DEFSTRUCT.RED - Interim structure definition facility.  
% 
% Author: 	Russ Fish 
% 		Computer Science Dept.  
% 		University of Utah 
% Date: 	18 December 1981 
% Copyright (c) 1981 University of Utah
%

% See files Defstruct.{Hlp,Doc} for description of usage.

%%%% To compile this code, it must first be loaded interpretively. %%%%

%%%% Bootstrap is necessary because defstructs are used internally %%%%
%%%% to record the descriptions of structures, including the       %%%%
%%%% descriptions of the defstruct descriptors themselves.         %%%%

% First, an aside to the compiler.
CompileTime	% Compiler needs to know about LHS forms which will be used.
    put( 'SlotDescInitForm, 'Assign!-Op, 'PUTSlotDescInitForm );

BothTimes	% Declare lists of fluids used for binding options.
<<
    fluid '( DefstructOptions SlotOptions );

    fluid (
	DefstructOptions := 
	    '( !:Constructor !:Alterant !:Predicate !:Creator
	       !:Prefix !:Include !:IncludeInit ) );

    fluid (
	SlotOptions := '( !:Type !:UserGet !:UserPut ) );

	flag('(defstruct), 'Eval);

>>;

% //////////////  Externally known fns  //////////////////////////

% Struct type predicate.
lisp procedure DefstructP( Name );
    get( Name, 'Defstruct );

% Access to "struct type name" field of structure.
lisp procedure DefstructType( Struct );
    if VectorP Struct then		% Minimal checking.
	getv( Struct, 0 )
    else
	NIL;

% Type inclusion predicate.
lisp procedure SubTypeP( I1, I2 );	% T if I1 is a subtype of I2.
begin scalar Incl;
    return
	    I1 eq I2			% Type is subtype of itself.  (LEQ.)
	or
		(Incl := DsDescInclude GetDefstruct I2)	% Done if no subtype.
	    and
		(   I1 eq Incl			% Proper subtype.
		or  SubTypeP( I1, Incl )   )	% Or a subsubtype, or...
end;

% //////////////  Defstruct  /////////////////////////////////////

fexpr procedure Defstruct( Spec );
begin scalar StructName, Init, NameValue, Desc, DsSize, SlotSpec, SlotAlist;

    if atom Spec then			% Spec must be a list.
	TypeError( Spec, 'Defstruct, "a spec list" );

    StructName := if atom first Spec then
	    first Spec			% Grab the struct id.
        else
	    first first Spec;

    if not idp StructName then		% Struct id better be one.
	UsageTypeError( StructName, 'Defstruct, "an id", "a StructName" );

    % Defaults for options.
    !:Constructor := !:Alterant := !:Predicate := T;
    !:Creator := !:Include := !:IncludeInit := NIL;
    !:Prefix := "";

    % Process option list if present.
    if pairp first Spec then
	ProcessOptions( rest first Spec, DefstructOptions );

    if !:Prefix = T then		% Default prefix is StructName.
	!:Prefix := id2string StructName;

    if idp !:Prefix then		% Convert id to printname string.
	!:Prefix := id2string !:Prefix
    else
	if not stringp !:Prefix then	% Error if not id or string.
	    UsageTypeError( !:Prefix, 'Defstruct,
			 "an id or a string", "a SlotName prefix" );

    % Construct macro names in default pattern if necessary.
    if !:Constructor eq T then !:Constructor := IdConcat( 'MAKE, StructName );
    if !:Alterant eq T then !:Alterant := IdConcat( 'ALTER, StructName );
    if !:Predicate eq T then !:Predicate := IdConcat( StructName, 'P );
    if !:Creator eq T then !:Creator := IdConcat( 'CREATE, StructName );

    % Define the constructor, alterant, predicate, and creator, if desired.
    MkStructMac( !:Constructor, 'Make, StructName );
    MkStructMac( !:Alterant, 'Alter, StructName );
    MkStructPred( !:Predicate, StructName );
    MkStructMac( !:Creator, 'Create, StructName );

    DsSize := 0;	% Accumulate size, starting with the DefstructType.
    SlotAlist := NIL;
    if !:Include then	% If including another struct, start after it.
	if Desc := GetDefstruct( !:Include ) then
	<<
	    DsSize := DsDescDsSize( Desc );

	    % Get slots of included type, modified by !:IncludeInit.
	    SlotAlist := for each Init in DsDescSlotAlist( Desc ) collect
	    <<
		if !:IncludeInit and
		    (NameValue := atsoc( car Init, !:IncludeInit )) then
		<<
		    Init := TotalCopy Init;
		    SlotDescInitForm cdr Init := second NameValue
		>>;
		Init
	    >>
	>>
	else
	    TypeError( !:Include, "Defstruct !:Include", "a type id" );

    % Define the Selector macros, and build the alist of slot ids.
    SlotAlist := append( SlotAlist,
	for each SlotSpec in rest Spec collect
	    ProcessSlot( SlotSpec, !:Prefix, DsSize := DsSize+1 )  );

    if Defstructp Structname then
	ErrorPrintF("*** Defstruct %r has been redefined", StructName);

    Put(  StructName, 'Defstruct,	% Stash the Structure Descriptor.

	CreateDefstructDescriptor(
		DsSize, !:Prefix, SlotAlist, !:Constructor, !:Alterant,
		!:Predicate, !:Creator, !:Include, !:IncludeInit )
    );

    return StructName
end;

% Turn slot secifications into (SlotName . SlotDescriptor) pairs.
lisp procedure ProcessSlot( SlotSpec, Prefix, SlotNum );
begin scalar SlotName, SlotFn, It, OptList, InitForm;

    % Got a few possibilities to unravel.
    InitForm := OptList := NIL;		% Only slot-name required.
    if atom SlotSpec then
	SlotName := SlotSpec	% Bare slot-name, no default-init or options.
    else 
    <<
	SlotName := first SlotSpec;

	if It := rest SlotSpec then    % Default-init and/or options provided.
	<<
	    % See if option immediately after name.
	    while pairp It do It := first It;		% Down to first atom.
	    if idp It and memq( It, SlotOptions ) then	% Option keyword?
		OptList := rest SlotSpec		% Yes, no init-form.
	    else
	    <<
		InitForm := second SlotSpec;	% Init-form after keyword.
		OptList := rest rest SlotSpec	% Options or NIL.
	    >>
	>>
    >>;

    if not idp SlotName then		% Slot id better be one.
	UsageTypeError( SlotName, 'Defstruct, "an id", "a SlotName" );

    SlotFn := if Prefix eq "" then	% Slot fns may have a prefix.
	    SlotName
	else
	    IdConcat( Prefix, Slotname );

    % Defaults for options.
    !:Type := !:UserGet := !:UserPut := NIL;
    
    if OptList then	% Process option list
	ProcessOptions( OptList, SlotOptions );

    % Make Selector and Depositor unless overridden.
    if not !:UserGet then MkSelector( SlotFn, SlotNum );
    if not !:UserPut then MkDepositor( SlotFn, SlotNum );

    % Return the ( SlotName . SlotDescriptor ) pair.
    return SlotName .

	CreateSlotDescriptor(
		SlotNum, InitForm, SlotFn, !:Type, !:UserGet, !:UserPut )
end;

% //////////////  Internal fns  //////////////////////////////////

% Process defstruct and slot options, binding values of valid options. 
lisp procedure ProcessOptions( OptList, OptVarList );
begin scalar OptSpec, Option, OptArg;

    for each OptSpec in OptList do
    <<
	if atom OptSpec then		% Bare option id.
	<<
	    Option := OptSpec;
	    OptArg := T
	>>
	else
	<<
	    Option := first OptSpec;
	    OptArg := rest OptSpec;	% List of args to option.
	    if not rest OptArg then	% Single arg, unlist it.
		OptArg := first OptArg
	>>;

	if memq( Option, OptVarList ) then
	    set( Option, OptArg )
	else 
	    UsageTypeError( Option, 'ProcessOptions,
		    ("one of" . OptVarList . "is needed"), "an option id" )
    >>
end;

lisp procedure GetDefstruct( StructId );	% Yank struct defn from id.
begin scalar Desc;
    if Desc := get( StructId, 'Defstruct )
	then return Desc		% Return Struct defn.
    else
    	TypeError( StructId, 'GetDefstruct, "a defstruct id" )
end;

lisp procedure IdConcat( I1, I2 );	% Make two-part names.
<<
    if idp I1 then I1 := id2String I1;
    if idp I2 then I2 := id2String I2;
    intern concat( I1, I2 )
>>;

% //////////////  Fn building fns  ///////////////////////////////

% Fn to build specific Structure Fns as macros which use generic macros.
% The generic macro is called with the StructName and the original
% list of arguments.
%     MacName( arg1, arg2, ... )
%      => GenericMac( StructName, arg1, arg2, ... )
lisp procedure MkStructMac( MacName, GenericMac, StructName );
    if MacName then			% No macro if NIL name.
	putd( MacName, 'macro,
	    list( 'lambda,
		  '(MacroArgs),
		  list( 'append,
			list( 'quote,
			      list( GenericMac, StructName )
			),
			'(rest MacroArgs)
		  )
	    )
	);


% Fn to build specific Structure Predicates.
lisp  procedure MkStructPred( FnName, StructName );
    putd( FnName, 'expr,
	list( 'lambda, '(PredArg),
	    list( 'and,
		  '(vectorp PredArg),
		  list( 'eq,
			list('quote,StructName),
			'(DefstructType PredArg) )
	    )
	)
    );

% RHS selector (get fn) constructor.
lisp procedure MkSelector( Name, Slotnum );
    putd( Name, 'expr,
	list( 'lambda, '(Struct), List( 'getV, 'Struct, SlotNum ) )  );

% LHS depositor (put fn) constructor.
lisp procedure MkDepositor( Name, Slotnum );
begin scalar PutName;
    PutName := intern concat( "PUT", id2string Name );

    putd( PutName, 'expr,
	list( 'lambda, '(Struct Val),
	      List( 'putV, 'Struct, SlotNum, 'Val ) )  );

    put( Name, 'Assign!-Op, PutName );

    return PutName
end;

% //////////////  Fns used by macros.  ///////////////////////////

% Generic macro for constructors, called with structure name and list
% of slot-name:value-form pairs to merge with default-inits.
% Returns vector constructor.
macro procedure Make( ArgList );
begin scalar StructName, OverrideAlist, Slot, NameValue;
    StructName := second ArgList;
    OverrideAlist := rest rest ArgList;

    return append(			% Return vector constructor.
	list( 'vector,
	      list('quote,StructName) ),  % Mark struct type as first element.

	% Build list of init forms for vector constructor.
	for each Slot in DsDescSlotAlist GetDefstruct StructName collect
	    if NameValue := atsoc( car Slot, OverrideAlist ) then
		second NameValue
	    else
		SlotDescInitForm cdr Slot
    )

end;

% Generic Alterant macro, called with structure name, struct instance and
% slot name:value alist.  A list of depositor calls is returned, with a
% PROGN wrapped around it and the struct instance at the end for a return
% value.
macro procedure Alter( ArgList );
begin scalar StructName, StructInstance, SlotValueDlist, SlotAlist,
	     NameValue, Slot;
    StructName := second ArgList;
    StructInstance := third  ArgList;
    SlotValueDlist := rest rest rest  ArgList;
    SlotAlist := DsDescSlotAList GetDefstruct StructName;

    return append( append(
	'(PROGN),	% wraparound PROGN.

	% List of depositor calls.
	for each NameValue in SlotValueDlist collect
	    if Slot := atsoc( first NameValue, SlotAlist) then
		list(
		    % Use depositors, which may be user fns, rather than PutV.
		    IdConCat( 'PUT, SlotDescSlotFn cdr Slot ),
		    StructInstance,
		    second NameValue )
	    else
		TypeError( car NameValue, 'Alter,
		    concat( "a slot of ", id2string StructName )  )

	), list( StructInstance )   )	% Value of PROGN is altered instance.
end;

% Generic Create macro, called with struct name and list of positional args
% which are slot value forms.  Returns struct vector constructor.
macro procedure Create( ArgList );
begin scalar StructName, SlotValues, DsSize;
    StructName := second ArgList;
    SlotValues := rest rest ArgList;
    DsSize := DsDescDsSize GetDefstruct StructName;

    if DsSize = Length SlotValues then
	return append(
	    list( 'VECTOR,
		  list( 'quote, StructName ) ),	% Mark with struct id.
	    SlotValues )
    else
	UsageTypeError( SlotValues, 'Create,
		BldMsg( "a list of length %p", DsSize ),
		concat( "an initializer for ", id2string StructName)  )
end;

% //////////////  Boot Defstruct structs.  ///////////////////////

% Chicken-and-egg problem, need some knowledge of Defstruct descriptor
% structures before they are defined, in order to define them.

CompileTime <<
MkSelector( 'DsDescDsSize, 1 );
MkStructMac( 'CreateDefstructDescriptor, 'Create, 'DefstructDescriptor );
MkStructMac( 'CreateSlotDescriptor, 'Create, 'SlotDescriptor );

put( 'DefstructDescriptor, 'Defstruct,	% Abbreviated struct defns for boot.
    '[ DefstructDescriptor 9 ]  );	% Just DsSize, for Create Fns.
put( 'SlotDescriptor, 'Defstruct,
    '[ SlotDescriptor  6 ]  );
>>;

% Now really declare the Defstruct Descriptor structs.
Defstruct(
    DefstructDescriptor( !:Prefix(DsDesc), !:Creator ),
	   DsSize(	!:Type int ),	% (Upper Bound of vector.)
	   Prefix(	!:Type string ),
	   SlotAlist(	!:Type alist ),	% (Cdrs are SlotDescriptors.)
	   ConsName(	!:Type fnId ),
	   AltrName(	!:Type fnId ),
	   PredName(	!:Type fnId ),
	   CreateName(	!:Type fnId ),
	   Include(	!:Type typeid ),
	   InclInit(	!:Type alist )
);

Defstruct(
    SlotDescriptor( !:Prefix(SlotDesc), !:Creator ),
	   SlotNum(	!:Type int ),
	   InitForm(	!:Type form ),
	   SlotFn(	!:Type fnId ),		% Selector/Depositor id.
	   SlotType(	!:Type type ),		% Hm...
	   UserGet(	!:Type boolean ),
	   UserPut(	!:Type boolean )
);

END;
