
%  Fexpr for defining GLISP functions.
(dm dg (x)
  (prog (result)
   (put (cadr x) 'gloriginalexpr (cons 'lambda (cddr x)))
   (return
     (cond (glcompiledefflg
              (glcc (cadr x))
              (setq result (cons 'df 
                                 (cons (cadr x)
                                       (cdr (get (cadr x) 'glcompiled)))))
              (put (cadr x) 'glcompiled nil)
              result)
           (t (glputhook (cadr x))
              (list 'quote (cadr x)) )) )))

