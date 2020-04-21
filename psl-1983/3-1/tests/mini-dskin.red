% MINI-DSKIN.RED

Procedure TypeFile F;
Begin Scalar InChan,OldChan,c;
  InChan:=Open(F,'Input);
  OldChan:=Rds InChan;
  While Not ((c:=Getc()) eq 26) do PutC(c);
  rds OldChan;
  close InChan;
 end;

Procedure DskIn F;
 Begin scalar Infile, OldFile,x;
   Infile:=Open(F,'Input);
   OldFile:=RDS Infile;
   While not ((x:=Read()) eq !$eof!$) do
 << x:=Eval x;
    If !*Pval then Print x>>;
   RDS OldFile;
   Close InFile;
End;

FLUID '(!*Echo !*PVAL);

procedure Lapin F;
 Begin scalar !*echo, !*pval;
    Return Dskin F;
 End;

End;
