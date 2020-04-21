
% MLG, 6:01am  Thursday, 10 June 1982
% Utilities to read and process DIR files
%

IMPORTS '(EXEC);

% -------- Basic File Reader -------------

Fluid '(File);

procedure ReadOneLine;
% Read a single line, return as string
 begin scalar c,l;
   while ((c:=ReadCh()) NEQ !$EOL!$) do
     If c EQ !$EOF!$ then Throw('Filer,'Done)
      else l:=c . l;
     Return list2string reverse l;
end;

procedure ReadDirFile F;
% Read in a file as vector of strings
 begin scalar oldF,x;
   OldF:=Rds(F:=Open(F,'input));
   File:=NIL;
   Catch('Filer,'(ReadAllFile1));
   Rds OldF;
   Close F;
   Return List2vector Reverse File;
 end;

procedure ReadAllFile1;
% support for Read Dir File
 begin scalar l;
  While (l:=ReadOneLine()) do 
     if Size(l)>=0 then file:= segmentstring(l,char '! ) . file;
  return List2Vector reverse file;
 end;

%---------------------------------------------------
procedure ReadCleanDir F;
% read in a Dir File without dates, and clean up
 Begin scalar x;
   x:=ReadDirFile F; % As a vector of strings
%/ x:=ExpandNames x; % Handle .xxx case
   x:=RemoveAllVersionNumbers x;
%/ x:=RemoveDuplicates x; % Assume ordered
   Return x;
 End;

%---- Now take apart the fields

Procedure GetFileName(S);    % Find part before dot
 begin scalar N,I;
    n:=Size S;
    i:=0;
    While i<=n and S[i] neq Char '!. do i:=i+1;
    return Sub(S,0,i-1);
 end;

procedure GetExtension(S);    % Find second part, after  dot
 begin scalar N,I;
    n:=Size S;
    i:=n;
    While i>=0 and S[i] neq Char '!. do i:=i-1;
    return Sub(S,i+1,n-i-1);
 end;

% Dont need to expand names anymore
CommentOutCode <<

procedure ExpandNames(Fvector); % replace .xxxx with yyy.xxx from previous
 Begin  scalar F;
  for i:=1:Size(Fvector) do
    <<F:=Fvector[I];
      if F[0] EQ char '!. 
        then Fvector[I]:=concat(GetFileName Fvector[I-1],F)>>;
   return Fvector;
 end;
>>;

procedure RemoveVersionNumber F; % replace xxxx.yyyy.nnn with xxxx.yyyy
 Begin  scalar I;
  i:=Size(F);
  While i>=0 and F[i] NEQ char '!. do i:=i-1;
  Return Sub(F,0,i-1);
 end;

procedure RemoveAllVersionNumbers(Fvector); % replace xxxx.yyy.nnn with xxx.yyy
 Begin  
  For i:=0:Size(Fvector)
   do  Fvector[I]:=RemoveVersionNumber Car Fvector[I];
   return Fvector;
 end;

procedure GetDirInFile(Dstring,FileName);
 Docmds List("Dir ",Dstring,",",crlf,
             "out ",Filename,crlf,
             "no heading ",crlf,
             "separate ",crlf,
             "no summary ",crlf,
         crlf,"pop");

procedure GetCleanDir Dstring;
  Begin Scalar x;
    GetDirInFile(Dstring,"Junk.Dir");
    x:=ReadCleanDir "junk.Dir";
    DoCmds List("Del junk.dir,",crlf,
                "exp ",crlf,crlf,"pop");
    return x
  End;

procedure GetDatedDirInFile(Dstring,FileName);
 Docmds List("Dir ",Dstring,",",crlf,
             "out ",Filename,crlf,
             "no heading ",crlf,
             "separate ",crlf,
             "no summary ",crlf,
             "time write ",crlf,
         crlf,"pop");

procedure GetCleanDatedDir Dstring;
  Begin Scalar x;
    GetDatedDirInFile(Dstring,"Junk.Dir");
    x:=ReadCleanDatedDir "junk.Dir";
    DoCmds List("Del junk.dir,",crlf,
                "exp ",crlf,crlf,"pop");
    return x
  End;

procedure ReadCleanDatedDir F;
 begin scalar x;
   x:=ReadDirFile F;
%/ x:=ExpandNames x; % Handle .xxx case
   For i:=0:Size(x)
    do  Rplaca(x[i],RemoveVersionNumber Car x[I]);
   return x
 end;

% Segment a string into fields:

Procedure SegmentString(S,ch); % "parse" string in pieces at CH
 Begin scalar s0,sN,sN1, Parts, sa,sb;
   s0:=0; 
   sn:=Size(S);
   sN1:=sN+1;
 L1:If s0>sn then goto L2;
   sa:=NextNonCh(Ch,S,s0,sN);
   if sa>sN then goto L2;
   sb:=NextCh(Ch,S,sa+1,sN);
   if sb>SN1 then goto L2;
   Parts:=SubSeq(S,sa,sb) . Parts;
   s0:=sb;
   goto L1;
  L2:Return Reverse Parts;
 End;

Procedure NextCh(Ch,S,s1,s2);
 <<While (S1<=S2) and not(S[S1] eq Ch) do s1:=s1+1;
   S1>>;

Procedure NextNonCh(Ch,S,s1,s2);
 <<While (S1<=S2) and (S[S1] eq Ch)  do s1:=s1+1;
   S1>>;
   
End;
