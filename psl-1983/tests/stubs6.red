% STUBS6.RED -Stubs introduced for TEST6 and up

in "PT:mini-printf.red"$
in "PT:mini-top-loop.red"$

On syslisp;

Procedure FUNCALL(FN,I);
 IDApply1(I,FN);

off syslisp;

procedure fluid u;
 print list ('nofluid, u);

procedure global u;
 print list ('noglobal, u);

END;
