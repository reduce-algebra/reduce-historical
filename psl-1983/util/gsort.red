%===================================================================
% Simple sorting functions for PSL strings and Ids
% use with FindPrefix and FindSuffix

% MLG,  8:16pm  Monday, 14 December 1981
%===================================================================

% Revision History
%
% Edit by Cris Perdue, 26 Jan 1983 1343-PST
% Fixed the order of arguments in one call to make GMergeSort stable.
% MLG, 2 Jan 1983
%	Changed IDSORT form Macro to procedure, so that
%	it could be redefined for experiments with alternate GSORT
%	Affected RCREF and FIND


lisp procedure StringCompare(S1,S2);    
%  Returns 1,0,-1 for S1<S2,S1=S2,S1>S2
% String Comparison
 Begin scalar L1,L2,I,L;
        L1:=Size(S1); L2:=Size(S2);
        L:=MIN2(L1,L2);
        I:=0;
  loop: If I>L then return(If L1 <L2 then 1
                           else if L1 > L2 then -1
                           else 0);
	if S1[I] < S2[I] then return 1;
      	if S1[I] > S2[I] then return (-1);
	I:=I+1;
	goto loop;
 End;

lisp procedure IdCompare(D1,D2);	
%  Compare IDs via print names
					%/ What of case
  StringCompare(Id2String D1,Id2String D2);

lisp procedure SlowIdSort DList;            
%  Worst Possible Sort;
  If Null DList then NIL
   else InsertId(car Dlist, SlowIdSort Cdr Dlist);

lisp procedure InsertId(D,DL);
 If Null DL then D . Nil
  else if IdCompare(D,Car DL)>=0 then D . DL
  else Car Dl . InsertId(D,Cdr Dl);

% ======= Tree based ALPHA-SORT package, derived from CREF

%  routines modified from FUNSTR for alphabetic sorting
%
%  Tree Sort of list of  ELEM
%
% Tree is  NIL or STRUCT(VAL:value,SONS:Node-pair)
%		Node-pair=STRUCT(LNode:tree,RNode:tree);

lisp smacro procedure NewNode(Elem); %/ use A vector?
	LIST(Elem,NIL);

lisp smacro procedure VAL Node; 	
%  Access the VAL in node
	CAR Node;

lisp smacro procedure LNode Node;
	CADR Node;

lisp smacro procedure RNode Node;
	CDDR Node;

lisp smacro procedure NewLeftNode(Node,Elem);
	RPLACA(CDR Node,NewNode Elem);

lisp smacro procedure NewRightNode(Node,Elem);
	RPLACD(CDR Node,NewNode Elem);

lisp procedure IdSort LST;  
%  Sort a LIST of ID's. Do not remove Dups
% Build Tree then collapse;
 Tree2LST(IdTreeSort(LST),NIL);

lisp procedure IdTreeSort LST;
% Uses insert of Element to Tree;
   Begin scalar Tree;
	If NULL LST then Return NIL;
	Tree:=NewNode CAR LST; % First Element
	While PAIRP(LST:=CDR LST) DO IdPutTree(CAR LST,Tree);
	Return Tree;
   END;

lisp smacro procedure IdPlaceToLeft (Elem1,Elem2);
% ReturnS T If Elem to go to left of Node
	IdCompare(Elem1,Elem2)>=0;

lisp procedure IdPutTree(Elem,Node);	
%  Insert Elements into Tree
  Begin
  DWN:	If Not IdPlaceToLeft(Elem,VAL Node)  then GOTO RGT;
	If LNode Node then <<Node:=LNode Node;GO TO DWN>>;
		NewLeftNode(Node,Elem);
		Return;
  RGT:	If RNode Node then <<Node:=RNode Node;GO TO DWN>>;
		NewRightNode(Node,Elem);
		Return;
  END;

lisp procedure Tree2LST(Tree,LST);	
%  Collapse Tree to LIST
  Begin
	While Tree DO 
	   <<LST:=VAL(Tree) .Tree2LST(RNode Tree,LST);
	    Tree:=LNode Tree>>;
 	Return LST;
   END;

% More General Sorting, given Fn=PlaceToRight(a,b);

lisp procedure GenSort(LST,Fn);  
%  Sort a LIST of  elems
% Build Tree then collapse;
 Tree2LST(GenTreeSort(LST,Fn),NIL);

lisp procedure GenTreeSort(LST,Fn);
% Uses insert of Element to Tree;
   Begin scalar Tree;
	If NULL LST then Return NIL;
	Tree:=NewNode CAR LST; % First Element
	While PAIRP(LST:=CDR LST) DO GenPutTree(CAR LST,Tree,Fn);
	Return Tree;
   END;

lisp procedure GenPutTree(Elem,Node,SortFn);	
%  Insert Elements into Tree
  Begin
  DWN:	If Not Apply(SortFn,list(Elem,VAL Node))  then GOTO RGT;
	If LNode Node then <<Node:=LNode Node;GO TO DWN>>;
		NewLeftNode(Node,Elem);
		Return;
  RGT:	If RNode Node then <<Node:=RNode Node;GO TO DWN>>;
		NewRightNode(Node,Elem);
		Return;
  END;


% More General Sorting, given SortFn=PlaceToLeft(a,b);

lisp procedure GSort(LST,SortFn);  
%  Sort a LIST of  elems
% Build Tree then collapse;
Begin 
 CopyD('GsortFn!*,SortFn);
 LST:= Tree2LST(GTreeSort LST,NIL);
 RemD('GsortFn!*);
 Return LST;
 End;


lisp procedure GTreeSort LST;
% Uses insert of Element to Tree;
   Begin scalar Tree;
	If NULL LST then Return NIL;
	Tree:=NewNode CAR LST; % First Element
	While PAIRP(LST:=CDR LST) DO GPutTree(CAR LST,Tree);
	Return Tree;
   END;

lisp procedure GPutTree(Elem,Node);	
%  Insert Elements into Tree
  Begin
  DWN:	If Not GSortFn!*(Elem,VAL Node)  then GOTO RGT;
	If LNode Node then <<Node:=LNode Node;GO TO DWN>>;
		NewLeftNode(Node,Elem);
		Return;
  RGT:	If RNode Node then <<Node:=RNode Node;GO TO DWN>>;
		NewRightNode(Node,Elem);
		Return;
  END;

% Standard Comparison Functions:

lisp procedure IdSortFn(Elem1,Elem2);
% ReturnS T If Elem1 to go to right of Elem 2;
	IdCompare(Elem1,Elem2)>=0;

lisp procedure NumberSortFn(Elem1,Elem2);
       Elem1 <= Elem2;

lisp procedure NumberSort Lst;
   Gsort(Lst,'NumberSortFn);

lisp procedure StringSortFn(Elem1,Elem2);
       StringCompare(Elem1,Elem2)>=0;

lisp procedure StringSort Lst;
   Gsort(Lst,'StringSortFn);

lisp procedure NoSortFn(Elem1,Elem2);
       NIL;

lisp procedure AtomSortFn(E1,E2);
 % Ids, Numbers, then strings;
 If IdP E1 then
     If IdP E2 then IdSortFn(E1,E2)
      else NIL
  else if Numberp E1
      then if IdP E2 then T
            else if NumberP E2 then NumberSortFn (E1,E2)
            else NIL
  else if StringP(E1)
        then if IDP(E2) then T
        else if Numberp E2 then T
        else StringSortFn(E1,E2)
  else NIL;

lisp procedure AtomSort Lst;
  Gsort(Lst,'AtomSortFn);

lisp procedure StringLengthFn(S1,S2);    
%  For string length
% String Length Comparison
    Size(S1)<=Size(S2);

procedure IdLengthFn(e1,e2);
  StringLengthFn(Id2string e1,Id2string e2);

On syslisp;

syslsp procedure SC1(S1,S2);    
%  Returns T if S1<=S2
% String Comparison
 Begin scalar L1,L2,I,L;
        S1:=Strinf s1; S2:=Strinf S2;
        L1:=StrLen(S1); L2:=StrLen(S2);
        If L1>L2 then L:=L2 else L:=L1;
        I:=0;
  loop: If I>L then return(If L1 <=L2 then T else NIL);
	if StrByt(S1,I) < StrByt(S2,I) then return T;
	if StrByt(S1,I) > StrByt(S2,I) then return NIL;
	I:=I+1;
	goto loop;
 End;

syslsp procedure IdC1(e1,e2);
  Sc1(ID2String e1, ID2String e2);

syslsp procedure SC2(S1,S2);    
% Returns T if S1<=S2
% String Comparison done via packed word compare, may glitch
 Begin scalar L1,L2,I,L;
        S1:=Strinf s1; S2:=Strinf S2;
        L1:=Strpack StrLen(S1); L2:=strpack StrLen(S2);
        S1:=S1+1; S2:=S2+1;
        If L1>L2 then L:=L2 else L:=L1;
        I:=0;              %/ May be off by one?
  loop: If I>L then return(If L1 <=L2 then T else NIL);
	if S1[I] < S2[I] then return T;
	if S1[I] > S2[I] then return NIL;
	I:=I+1;
	goto loop;
 End;

syslsp procedure IdC2(e1,e2);
  Sc2(ID2String e1,ID2String e2);

Off syslisp;

Lisp procedure GsortP(Lst,SortFn);
Begin 
    If Not PairP Lst then return T;
 L: If Not PairP Cdr Lst then Return T;
    If Not Apply(SortFn,list(Car Lst, Cadr Lst)) then return NIL;
    Lst :=Cdr Lst;
    goto L;
END;

Lisp procedure GMergeLists(L1,L2,SortFn);
 If  Not PairP L1 then L2 
  else if  Not PairP L2 then L1
  else if Apply(SortFn,list(Car L1, Car L2))
    then Car(L1) . GMergeLists(cdr L1, L2,SortFn)
  else car(L2) . GmergeLists(L1, cdr L2,SortFn);

Lisp procedure MidPoint(Lst1,Lst2,M);      % Set MidPointer List at M
  Begin 
        While Not (Lst1 eq Lst2) and M>0 do
          <<Lst1 := cdr Lst1;
            M:=M-1>>;
       return  Lst1;
  End;

Lisp procedure GMergeSort(Lst,SortFn);
 GMergeSort1(Lst,NIL,Length Lst,SortFn);

Lisp procedure GMergeSort1(Lst1,Lst2,M,SortFn);
 If M<=0 then NIL
  else if M =1 then if null cdr Lst1 then Lst1 else List Car lst1
  else if M=2 then
      (if Apply(SortFn,list(Car Lst1,Cadr Lst1)) then List(Car Lst1, Cadr Lst1)
        else List(Cadr Lst1,Car lst1))
  else begin scalar Mid,M1;
       M1:=M/2;
       Mid :=MidPoint(Lst1,Lst2,M1);
       Lst1 :=GMergeSort1(Lst1,Mid, M1,SortFn);
       Lst2 :=GmergeSort1(Mid,Lst2, M-M1,SortFn);
       Return GmergeLists(Lst1,Lst2,SortFn);
  end;

end;
