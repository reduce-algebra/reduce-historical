%  <PSL.UTIL>VECTOR-FIX.RED.5, 18-Mar-82 13:50:06, Edit by BENSON
%  Removed patches that were installed in V3 interp
%  <PSL.UTIL>VECTOR-FIX.RED.4, 20-Jan-82 12:15:26, Edit by GRISS
% Patch to allow 0 element vectors
%  

on Syslisp;

syslsp procedure MkWords N;		%. Allocate vector, init all to #0
    if IntP N then
    <<  if N < (-1) then
	    StdError
  	 '"A WORD vector with fewer than zero elements cannot be allocated"
	else begin scalar W;
	    W := GtWRDS N;
	    for I := 0 step 1 until N do WrdItm(W, I) := 0;
	    return MkWRDS W;		% Tag it
	end >>
    else NonIntegerError(N, 'MkWords);

% A special facility to truncate X-vects in place
% extract peices

syslsp procedure TruncateVector(V,I);
 If Not VectorP V then NonVectorError(V,'TruncateVector)
  else if not IntP I then NonIntegerError(I,'TruncateVector)
  else begin scalar Len,Len2,VI;
	VI:=VecInf V;
	Len:=VecLen VI;
        If Len=I then return V; % Already the size
	If Len<I then 
	  return StdError "Cannot Lengthen a Vector in TruncateVector";
 	If Len<(-1) then
	   return StdError "Cant TruncateVector to less then -1";
        @VI := MkItem(HVECT,I);
	VecItm(VI, I+1) := MkItem(HVECT, Len-I-2);
	return V
  end;

% Missing Words Operations

syslsp procedure WordsP W;
    tag(w) eq Wrds;

syslsp procedure TruncateWords(V,I);
 If Not WordsP V then NonWordsError(V,'TruncateWords)
  else if not IntP I then NonIntegerError(I,'TruncateWords)
  else begin scalar Len,Len2,VI;
	VI:=WRDInf V;
	Len:=WRDLen VI;
        If Len=I then return V; % Already the size
	If Len<I then 
	  return StdError "Cannot Lengthen a Words in TruncateWords";
 	If Len<(-1) then
	   return StdError "Cant TruncateWords to less then -1";
        @VI := MkItem(HWRDS,I);
	WrdItm(VI, I+1) := MkItem(HWRDS, Len-I-2);
	return V
  end;

syslsp procedure GetWords(WRD, I);	%. Retrieve the I'th entry of WRD
begin scalar StripV, StripI;
    return if WordsP WRD then
	if IntP I then			% can't have Wordss bigger than INUM
	<<  StripV := WRDInf WRD;
	    StripI := IntInf I;
	    if StripI >= 0 and StripI <= WRDLen StripV then
		WRDItm(StripV, StripI)
	    else
		StdError BldMsg('"Subscript %r in GetWords is out of range",
					     I) >>
	else
	    IndexError(I, 'GetWords)
    else
	NonWordsError(WRD, 'GetWords);
end;

syslsp procedure PutWords(WRD, I, Val);	%. Store Val at I'th position of WRD
begin scalar StripV, StripI;
    return if WordsP WRD then
	if IntP I then			% can't have Wordss bigger than INUM
	<<  StripV := WRDInf WRD;
	    StripI := IntInf I;
	    if StripI >= 0 and StripI <= WRDLen StripV then
		WRDItm(StripV, StripI) := Val
	    else
		StdError BldMsg('"Subscript %r in PutWords is out of range",
					     I) >>
	else
	    IndexError(I, 'PutWords)
    else
	NonWordsError(WRD, 'PutWords);
end;

syslsp procedure UpbW V;		%. Upper limit of Words V
    if WordsP V then MkINT WRDLen WRDInf V else NIL;

off Syslisp;

END;
