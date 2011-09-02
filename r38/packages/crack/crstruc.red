%*********************************************************************
module structural_eqns$
%*********************************************************************
%  Routines for solving a system of structural equations
%  Author: Thomas Wolf
%  1998
%
% $Id: crstruc.red $
%

%--> Necessary assignments in the application code in algebraic mode:
%
% lisp(struc_eqn:=t)$
% struc_done:={all functions for which the corresponding
%              structural equations have already satisfied their
%              integrability conditions}$

symbolic procedure ini_struc$
begin scalar done$ 

 done:=algebraic struc_done$

 % check initial data
 if (not pairp done) or
    (car done neq 'LIST) then 
 rederr("struc_done not properly initialized!")
                               else done:=cdr done$

 % In order for structural equations to be used for formulating
 % integrability conditions and not for substitutions, we need a
 % `total differential order' ordering where the differential order
 % has highest priority higher than the lex. order of functions
 lex_fc:=nil$ 

 % Although only first order derivatives occur in the structural
 % equations, we will specify to order derivatives by their
 % differential order and not lexicographically.
 lex_df:=nil$

 quick_decoup:=t$ % To do the first reduction found, not looking
                  % for other reductions or integrability conditions

 proc_list_:='(
              subst_level_03
              alg_length_reduction
              decoupling
              subst_level_05
              change_proc_list
             )$

 lisp(adjust_fnc:=t)$

end$

symbolic procedure change_proc_list(arglist)$
begin scalar fcts;
 proc_list_:='(
              to_do
              subst_level_05
              separation
              quick_integration       
              full_integration        
              integration             
              subst_derivative
              subst_level_4
              undetlinode
              gen_separation 
              decoupling
              diff_length_reduction   
              undo_subst_derivative
             )$
 struc_eqn:=nil$
 fcts:=union(cadr arglist,setdiff(ftem_,cadr arglist))$
 if print_ then <<
  terpri()$
  write"The priority list of procedures is changed. The new one is:"$
  priproli(proc_list_)$
  terpri()$ write"The current situation:"$
  print_statistic(car arglist,fcts);
  print_pdes(car arglist);
  print_ineq(ineq_);
  struc_dim:=length fcts;
 >>;
 return arglist
end$

endmodule$
end$

