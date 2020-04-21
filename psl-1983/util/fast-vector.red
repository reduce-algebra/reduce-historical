%  <PSL.UTIL>FAST-VECTOR.RED.1, 18-Mar-82 21:26:35, Edit by GRISS
%  Fast Vector operations

imports '(Syslisp);			% Uses syslisp macros

CopyD('IGetV, 'GetV);

CopyD('IPutV, 'PutV);

CopyD('ISizeV, 'Size);

Put('IGetV, 'Assign!-Op, 'IPutV);

CopyD('IGetS, 'Indx);

CopyD('IPutS, 'SetIndx);

CopyD('ISizeS, 'Size);

Put('IGetS, 'Assign!-Op, 'IPutS);

if_system(VAX,
DefList('((IGetV (lambda (V I) (VecItm (VecInf V) I)))
	  (IPutV (lambda (V I X) (PutVecItm (VecInf V) I X)))
	  (IGetS (lambda (S I) (StrByt (StrInf S) I)))
	  (IPutS (lambda (S I X) (PutStrByt (StrInf S) I X)))
	  (ISizeV (lambda (V) (VecLen (VecInf V))))
	  (ISizeS (lambda (V) (StrLen (StrInf V))))), 'CMacro));

if_system(PDP10,		% tags don't need to be stripped on the PDP10
DefList('((IGetV (lambda (V I) (VecItm V I)))
	  (IPutV (lambda (V I X) (PutVecItm V I X)))
	  (IGetS (lambda (S I) (StrByt S I)))
	  (IPutS (lambda (S I X) (PutStrByt S I X)))
	  (ISizeV (lambda (V) (VecLen V)))
	  (ISizeS (lambda (S) (StrLen S)))), 'CMacro));

if_system(MC68000,		% tags don't need to be stripped on the 68000
DefList('((IGetV (lambda (V I) (VecItm V I)))
	  (IPutV (lambda (V I X) (PutVecItm V I X)))
	  (IGetS (lambda (S I) (StrByt S I)))
	  (IPutS (lambda (S I X) (PutStrByt S I X)))
	  (ISizeV (lambda (V) (VecLen V)))
	  (ISizeS (lambda (S) (StrLen S)))), 'CMacro));

END;
