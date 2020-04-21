% Fast Defstruct Improvements;
% M.L. Griss
% Load after Defstruct to redefine basic Selectors

FLUID '(DefGetFn!* DefPutFn!* !*DefFnAsExpr);

LoadTime <<
 DefGetFn!*:='IGetv;
 DefPutFn!*:='IPutv;
 !*DefFnAsExpr:=NIL;>>;

% RHS selector (get fn) constructor.
lisp procedure MkSelector( Name, Slotnum );
   If !*DefFnAsExpr then 
         putd( Name, 'expr,
	 list( 'lambda, '(Struct), List( DefGetFn!*, 'Struct, SlotNum ) )  )
    else Putd(name,'macro,
         list('lambda,'(struct), 
            List('LIST,MkQuote DefGetFn!*,'(Cadr Struct),MkQuote SlotNum)));

% LHS depositor (put fn) constructor.
lisp procedure MkDepositor( Name, Slotnum );
begin scalar PutName;
    PutName := intern concat( "PUT", id2string Name );
   If !*DefFnAsExpr then 
    putd( PutName, 'expr,
	list( 'lambda, '(Struct Val),
	      List( DefPutFn!*, 'Struct, SlotNum, 'Val ) )  )
    else Putd(PutName,'macro,
         list('lambda,'(struct), 
            List('List,MkQuote DefPutFn!*,
                   '(Cadr Struct),
                      MkQuote SlotNum,
                        '(Caddr Struct)
))
                );

    put( Name, 'Assign!-Op, PutName );

    return PutName
end;

END;
