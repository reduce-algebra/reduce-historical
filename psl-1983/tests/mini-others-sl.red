% MINI-OTHERS-SL.RED
on syslisp;

procedure Length U;
% Length of list U, fast version
    Length1(U, 0);

procedure Length1(U, N);
    if PairP U then Length1(cdr U, N+1) else N;

off syslisp;
end;
