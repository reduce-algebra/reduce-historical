% GEVAUX.SL.14     07 April 83
% Auxiliary functions for PSL version of GEV.
% GSN   07 March 83

% Interlisp Substring function.
(de substring (string first last)
    (cond ((not (stringp string)) (setq string (gevstringify string))))
    (cond ((minusp first)
             (setq first (add1 (plus (add1 (size string)) first)))))
    (cond ((minusp last)
             (setq last (add1 (plus (add1 (size string)) last)))))
    (subseq string (sub1 first) last) )


% Make a string out of anything
(de gevstringify (x)
  (cond ((stringp x) x)
        (t (bldmsg "%p" x))))



% Concatenate an arbitrary number of items
(de concatn (l)
  (cond ((null l) "")
        ((null (cdr l)) (gevstringify (car l)))
        (t (concat (gevstringify (car l)) (concatn (cdr l))))))

(de concatln (l)
  (cond ((null l) "")
        ((null (cdr l)) (gevstringify (eval (car l))))
        (t (concat (gevstringify (eval (car l))) (concatln (cdr l))))))

(df concatl (concatlarg) (concatln concatlarg))
(de gevconcat (l) (concatn l))

(de dreverse (l) (reversip l))

(de mkatom (s) (intern s))

(de gevputd (fn form)
  (put fn 'gloriginalexpr (cons 'lambda (cdr form)))
  (put fn 'glcompiled nil)
  (remd fn)
  (putd fn 'macro '(lambda (gldgform) (glhook gldgform))))

% Apply a function to arguments, Glisp-compiling first if needed.
(de gevapply (fn args)
  (cond ((and (atom fn)
              (or (null (get fn 'glcompiled))
                  (not (eq (getddd fn) (get fn 'glcompiled)))))
           (glcc fn)
           (apply fn args))
        (t (apply fn args))))

