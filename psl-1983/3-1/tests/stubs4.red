% STUBS4.RED - Stubs to support more automatic testing from TEST4 and on

procedure SpaceD(M);
<<Prin2 "           ";
    Prin2t M>>;

procedure DasheD(M);
<<Terpri();
   Prin2 "---------- ";
    Prin2T M>>;

procedure DotteD(M);
<<Terpri();
   Prin2 "   ....... ";
    Prin2T M>>;


Procedure ShouldBe(M,v,e); 
% test if V eq e;
 <<Prin2 "   ....... For ";Prin2 M; Prin2 '" ";
   Prin1 v; Prin2 '" should be "; Prin1 e;
   if v eq e then Prin2T '"  [OK ]"
    else Prin2T '"   [BAD] *******">>;

End;
