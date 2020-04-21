%
% P-LAMBIND.SL - Portable cmacro definitions *LAMBIND, *PROGBIND and *FREERSTR
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        6 August 1982
% Copyright (c) 1982 University of Utah
%
% Modification by MLG to preserve REG 1 across FREERSTR
% 19 March,1983
(compiletime (load useful))

(imports '(syslisp))			% requires SYSLISP for AddrUnitsPerItem

(de *lambind (regs fluids)
  (prog (n firstreg)
    (setq n 0)
    (setq regs (rest regs))		% remove REGISTERS at the front
    (setq fluids (rest fluids))		% remove NONLOCALVARS at the front
    (setq fluids			% convert fluids list into vector
          (list2vector (foreach x in fluids collect (second x))))
    (setq firstreg (first regs))
    (setq regs (rest regs))
    (return (if (null regs)			% only one to bind
        `((*move ,firstreg (reg 2))
	  (*move `,',(getv fluids 0) (reg 1))
	  (*call lbind1))
	`((*move ,firstreg (memory (fluid LambindArgs*) (wconst 0)))
	  (*move (fluid LambindArgs*) ,firstreg)
	  ,@(foreach x in regs collect
	    (progn (setq n (add1 n))
	           `(*move ,x
		     (memory ,firstreg
			     (wconst (wtimes2 (wconst AddressingUnitsPerItem)
					      (wconst ,n)))))))
	  (*move `,',fluids (reg 1))
	  (*call lambind))))))

(defcmacro *lambind)

(de *progbind (fluids)
  (if (null (rest (rest fluids)))
      `((*move `,',(second (first (rest fluids))) (reg 1))
	(*call pbind1))
      `((*move `,',(list2vector (foreach x in (rest fluids) collect
				         (second x)))
	       (reg 1))
	(*call progbind))))

(defcmacro *progbind)

(de *freerstr (fluids)
  `((*move (reg 1) (Fluid FreeRstrSave!*))
    (*move `,',(length (rest fluids)) (reg 1))
    (*call UnBindN)
    (*move (Fluid FreeRstrSave!*) (reg 1))))

(defcmacro *freerstr)

(setq *unsafebinder t)			% has to save registers across calls
