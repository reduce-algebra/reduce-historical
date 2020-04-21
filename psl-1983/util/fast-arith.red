% speed up generic arith for V3
% MLG,	9:25pm  Friday, 21 May 1982

ON SYSLISP;

SYSLSP PROCEDURE FASTPLUS2(I1,I2);
 Begin Scalar x;
 IF INTP(I1) AND INTP(I2) 
   AND (X:= WPLUS2(I1,I2)) EQ X
    THEN RETURN X;
   Return Oldplus2(I1,I2);
 End;

SYSLSP PROCEDURE FASTTIMES2(I1,I2);
Begin Scalar x;
 IF INTP(I1) AND INTP(I2) 
    AND (X:= WTIMES2(I1,I2)) EQ X
    Then return X;
  RETURN   OLDTimes2(I1,I2);
END;

SYSLSP PROCEDURE FASTDIFFERENCE(I1,I2);
Begin Scalar x;
 IF INTP(I1) AND INTP(I2) 
    AND (X:=WDIFFERENCE(I1,I2)) EQ X
  Then return x;
  RETURN  OldDifference(I1,I2);
END;

SYSLSP PROCEDURE FASTADD1 I1;
Begin Scalar x;
 IF INTP(I1)  
    AND (x:= IADD1 I1) EQ x
   then Return x;
  RETURN  OldAdd1 I1;
END;

SYSLSP PROCEDURE FASTSUB1 I1;
Begin Scalar x;
 IF INTP(I1) 
    AND (X:= ISUB1 I1) EQ X
   then Return x;
  RETURN  OldSub1 I1;
 end;

SYSLSP PROCEDURE FASTZerop I1;
 IF INTP(I1)  THEN WEQ(I1, 0)
  else OldZerop I1;

SYSLSP PROCEDURE FASTMinusp I1;
 IF INTP(I1)  THEN WLESSP(I1, 0)
  ELSE OldMinusp I1;

SYSLSP PROCEDURE FASTGreaterp(I1,I2);
 IF INTP(I1) AND INTP(I2) THEN WGREATERP(I1,I2)
   ELSE  OldGreaterp I1;

SYSLSP PROCEDURE FASTlessP(I1,I2);
 IF INTP(I1) AND INTP(I2) THEN WLESSP(I1,I2)
  ELSE  OldLessP I1;

off syslisp;

lisp procedure Faster;
Begin
!*usermode:=NIL;

COPYD('OLDPlus2,'Plus2);
COPYD('OLDTimes2,'Times2);
COPYD('OLDDifference,'Difference);
COPYD('OLDZeroP,'Zerop);

COPYD('OLDLessP,'LessP);
COPYD('OLDGreaterP,'GreaterP);
COPYD('OLDAdd1,'Add1);
COPYD('OLDSub1,'Sub1);

COPYD('Plus2,'FastPlus2);
COPYD('Times2,'FastTimes2);
COPYD('Difference,'FastDifference);
COPYD('ZeroP,'FastZerop);

COPYD('LessP,'FastLessP);
COPYD('GreaterP,'FastGreaterP);
COPYD('Add1,'FastAdd1);
COPYD('Sub1,'FastSub1);
end;

END;
