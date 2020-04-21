% Define the GLISP types.   GSN   07 march 83
(glispobjects

(gltype
  (atom (proplist
(glstructure (cons (strdes anything)
                   (proplist (prop (listof glpropentry))
                             (adj (listof glpropentry))
                             (isa (listof glpropentry))
                             (msg (listof glpropentry))
                             (supers (listof gltype)))))
(glispatomnumber integer)
(glpropfns (alist (str (listof glpropfnentry))
                  (prop (listof glpropfnentry))
                  (adj (listof glpropfnentry))
                  (isa (listof glpropfnentry))
                  (msg (listof glpropfnentry))))))
prop ((props (prop))
      (adjs (adj))
      (isas (isa))
      (msgs (msg))))

(glpropentry
    (cons (name atom)
          (cons (code anything)
                (proplist (result gltype)
                          (open boolean)))))

(glpropfnentry (list (name atom)
                     (code anything)
                     (result gltype)))
)

(put 'atom 'glstructure
     '(atom prop ((pname id2string result string))))
