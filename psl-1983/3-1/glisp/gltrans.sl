%     GLTRANS.SL.1         12 April 1983
%
%     Translate files from GLISP form to PSL.
%     G. Novak     12 April 83

(global '(gltransfile))

% Open a file for output
(de gltransopen (filename)
  (setq gltransfile (open filename 'output)))

% Close the output file
(de gltransclose () (close gltransfile))

% Read a file, translate it, and append to the output file.
(de gltransread (filename)
  (prog (infile expr)
    (setq infile (open filename 'input))
lp  (setq expr (channelread infile))
    (cond ((eq expr !$EOF!$) (return t))
          ((pairp expr)
             (eval expr)
             (channelterpri gltransfile)
             (cond ((eq (car expr) 'dg)
                      (glcc (cadr expr))
                      (channelprin1 gltransfile
                        (cons 'de (cons (cadr expr)
                                        (cdr (get (cadr expr) 'glcompiled))))))
                   (t (channelprin1 gltransfile expr)))
             (channelterpri gltransfile)))
    (go lp)))