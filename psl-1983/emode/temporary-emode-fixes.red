%
% TEMPORARY-EMODE-FIXES.RED - Tempory "fixes" to PSL to allow EMODE to run.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%


% This file tends to overlap CUSTOMIZE-RLISP-FOR-EMODE.RED.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Measurement tools
fluid '(cons_count);

Symbolic Procedure counting_cons(x,y);
% Version of cons that counts each call, old_cons_function must be set up
% for this to work.
<<
    cons_count := cons_count + 1;
    old_cons_function(x,y)
>>;

Symbolic Procedure start_cons_count();
% Setup to count conses.  Replaces cons with a version that counts calls to
% itself.
begin scalar !*RedefMSG;
      % !*RedefMSG is a fluid, controls printing of "redefined" messages.
    cons_count := 0;
    !*RedefMSG := NIL;
    CopyD('old_cons_function, 'cons);
    CopyD('cons, 'counting_cons);
end;

Symbolic Procedure stop_cons_count();
% Stop "cons counting", return the count.
begin scalar !*RedefMSG;
        % !*RedefMSG is a fluid, controls printing of "redefined" messages.

    !*RedefMSG := NIL;
    CopyD('cons, 'old_cons_function);
    return cons_count;
end;
