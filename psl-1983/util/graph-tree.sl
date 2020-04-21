% Needs USEFUL at compile time

(fluid '(graph-nodes* node-index*))

(de graph-to-tree (u)
  (let ((graph-nodes* nil)(node-index* 0))
    (graph-to-tree-1 u)))

(de graph-to-tree-1 (u)
  (let ((x))
    (cond
      ((not (or (pairp u) (vectorp u))) u)
      ((setf x (atsoc u graph-nodes*))
	(when (null (cdr x))
	  (setf (cdr x) (incr node-index*)))
	(newid (bldmsg "<%w>" (cdr x))))
      (t (let* ((p (ncons u))
		(graph-nodes* (cons p graph-nodes*))
		(v (if (vectorp u)
		     (for (from i 0 (upbv u)) (with (v (mkvect (upbv u))))
		       (do (setf (getv v i) (graph-to-tree-1 (getv u i))))
		       (returns v))
		     (cons
		       (graph-to-tree-1 (car u))
		       (graph-to-tree-1 (cdr u))))))
	   (if (cdr p)
	     (list (newid (bldmsg "<%w>:" (cdr p))) v)
	     v))))))

(de cprint (u)
  (let ((currentscantable* lispscantable*))
    (prettyprint (graph-to-tree u))
    nil))
