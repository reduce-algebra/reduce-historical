% Sample of use of <Fish.iact>DefStruct.RED
% See <fish.iact>Defstruct.HLP

Defstruct(Complex, R, I);

Defstruct(Complex, R(0), I(0)); % Redefine to see what functions defined
                                % Give 0 Inits
C0:=MakeComplex();
ComplexP C0;

C1:=MakeComplex(('R . 1), ('I . 2));

AlterComplex(C1,'(R . 2), '(I . 3));

Put('R,'Assign!-op,'PutR); % for LHS.

R(C1):=3; I(C1):=4;

C1;

% Show use of Include Option.

Defstruct(MoreComplex(!:Include(Complex)),Z(99));
Defstruct(MoreComplex(!:Include(Complex)),Z(99));

M0 := MakeMoreComplex();
M1:=MakeMoreComplex('R . 1, 'I . 2, ' Z . 3);

R C1;

R M1;
