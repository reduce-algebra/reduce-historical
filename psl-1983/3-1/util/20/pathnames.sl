%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PathNames.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        14 September 1982
% Revised:     9 February 1983
%
% DEC-20 implementation of some Common Lisp pathname functions.
%
% 9-Feb-83 Alan Snyder
%   Revise conversion to string to omit the dot if there is no type or version.
%   Revise conversion from string to interpret trailing dot as specifying
%   an empty type or version.  Change home-directory to specify PS:
%   Fix bug in make-pathname.  Convert to using fast-strings stuff.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load fast-int fast-vector fast-strings))
(BothTimes (load objects))

(when (funboundp 'string2integer)
  (de string2integer (s)
    (makestringintolispinteger s 10 1)
    ))

% The following function is an NEXPR: be sure this module is loaded at
% compile-time if you use this function in code to be compiled!

(dn make-pathname (keyword-arg-list)
  (let ((pn (make-instance 'pathname)))
    (while (not (null keyword-arg-list))
      (let ((keyword (car keyword-arg-list)))
	(setf keyword-arg-list (cdr keyword-arg-list))
	(cond (keyword-arg-list
	       (let ((value (car keyword-arg-list)))
		 (setf keyword-arg-list (cdr keyword-arg-list))
		 (selectq keyword
		   (host (=> pn set-host value))
		   (device (=> pn set-device value))
		   (directory (=> pn set-directory value))
		   (name (=> pn set-name value))
		   (type (=> pn set-type value))
		   (version (=> pn set-version value))
		   ))))))
    pn
    ))

(de pathname-host (pn)
  (=> (pathname pn) host))

(de pathname-device (pn)
  (=> (pathname pn) device))

(de pathname-directory (pn)
  (=> (pathname pn) directory))

(de pathname-name (pn)
  (=> (pathname pn) name))

(de pathname-type (pn)
  (=> (pathname pn) type))

(de pathname-version (pn)
  (=> (pathname pn) version))

(de PathnameP (x)
  (and (VectorP x) (eq (getv x 0) 'pathname)))

(de StreamP (x)
  (and (VectorP x) (object-get-handler-quietly x 'file-name)))

(de truename (x) (pathname x))

(de pathname (x)
  (cond
   ((PathnameP x) x)
   ((StringP x) (string-to-pathname x))
   ((IdP x) (string-to-pathname (id2string x)))
   ((StreamP x) (string-to-pathname (=> x file-name)))
   (t (TypeError x "PathName" "convertible to a pathname"))
   ))

(de namestring (x)
  (setf x (pathname x))
  (let ((dev (pathname-device x))
	(dir (pathname-directory x))
	(name (pathname-name x))
	(type (pathname-type x))
	(vers (pathname-version x))
	)
    (string-concat
     (if dev (string-concat (pathname-field-to-string dev) ":") "")
     (if dir (string-concat "<" (pathname-field-to-string dir) ">") "")
     (if name (pathname-field-to-string name) "")
     (if (or (not (pathname-empty-field? type))
	     (not (pathname-empty-field? vers)))
       (string-concat "." (pathname-field-to-string type)) "")
     (if (not (pathname-empty-field? vers))
       (string-concat "." (pathname-field-to-string vers)) "")
     )))

(de file-namestring (x)
  (setf x (pathname x))
  (let ((name (pathname-name x))
	(type (pathname-type x))
	(vers (pathname-version x))
	)
    (string-concat
     (if name (pathname-field-to-string name) "")
     (if type (string-concat "." (pathname-field-to-string type)) "")
     (if vers (string-concat "." (pathname-field-to-string vers)) "")
     )))

(de directory-namestring (x)
  (setf x (pathname x))
  (let ((dir (pathname-directory x))
	)
    (if dir (string-concat "<" (pathname-field-to-string dir) ">") "")
    ))

(de user-homedir-pathname ()
  (let ((pn (make-instance 'pathname))
	(user-number (Jsys1 0 0 0 0 (const jsGJINF)))
	(dir-name (MkString 100 (char space)))
	)
    (Jsys1 dir-name user-number 0 0 (const jsDIRST))
    (setf dir-name (recopystringtonull dir-name))
    (=> pn set-device "PS")
    (=> pn set-directory dir-name)
    pn
    ))

(de init-file-pathname (program-name)
  (let ((pn (user-homedir-pathname)))
    (=> pn set-name program-name)
    (=> pn set-type "INIT")
    pn
    ))

(de merge-pathname-defaults (pn defaults-pn default-type default-version)
  (setf pn (pathname pn))
  (setf defaults-pn (pathname defaults-pn))
  (setf pn (CopyVector pn))
  (if (not (=> pn host))
    (=> pn set-host (=> defaults-pn host)))
  (cond ((not (=> pn device))
	 (=> pn set-device (=> defaults-pn device))
	 (if (not (=> pn directory))
	   (=> pn set-directory (=> defaults-pn directory)))
	 ))
  (cond ((not (=> pn name))
	 (=> pn set-name (=> defaults-pn name))
	 (if (not (=> pn type)) (=> pn set-type (=> defaults-pn type)))
	 (if (not (=> pn version)) (=> pn set-version (=> defaults-pn version)))
	 ))
  (if (not (=> pn type))
    (=> pn set-type default-type))
  (if (not (=> pn version))
    (=> pn set-version default-version))
  pn
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defflavor pathname
  ((host "LOCAL")
   (device NIL)
   (directory NIL)
   (name NIL)
   (type NIL)
   (version NIL)
   )
  ()
  gettable-instance-variables
  )

(defmethod (pathname set-host) (new-host)
  (cond ((StringP new-host) (setf host (string-upcase new-host)))
	((and (ListP new-host)
	      (not (null new-host))
	      (StringP (car new-host)))
	 (setf host (string-upcase (car new-host))))
	(t (StdError "Invalid host specified for pathname."))
	))

(defmethod (pathname set-device) (new-device)
  (cond ((StringP new-device) (setf device (string-upcase new-device)))
	((null new-device) (setf device NIL))
	((and (ListP new-device)
	      (StringP (car new-device)))
	 (setf device (string-upcase (car new-device))))
	((and (IdP new-device)
	      (or (eq new-device 'unspecific)
		  (eq new-device 'wild)))
	 (setf device new-device))
	(t (StdError "Invalid device specified for pathname."))
	))

(defmethod (pathname set-directory) (new-directory)
  (cond ((StringP new-directory) (setf directory (string-upcase new-directory)))
	((null new-directory) (setf directory NIL))
	((and (ListP new-directory)
	      (StringP (car new-directory)))
	 (setf directory (string-upcase (car new-directory))))
	((and (IdP new-directory)
	      (or (eq new-directory 'unspecific)
		  (eq new-directory 'wild)))
	 (setf directory new-directory))
	(t (StdError "Invalid directory specified for pathname."))
	))

(defmethod (pathname set-name) (new-name)
  (cond ((StringP new-name) (setf name (string-upcase new-name)))
	((null new-name) (setf name NIL))
	((and (ListP new-name)
	      (StringP (car new-name)))
	 (setf name (string-upcase (car new-name))))
	((and (IdP new-name)
	      (or (eq new-name 'unspecific)
		  (eq new-name 'wild)))
	 (setf name new-name))
	(t (StdError "Invalid name specified for pathname."))
	))

(defmethod (pathname set-type) (new-type)
  (cond ((StringP new-type) (setf type (string-upcase new-type)))
	((null new-type) (setf type NIL))
	((and (IdP new-type)
	      (or (eq new-type 'unspecific)
		  (eq new-type 'wild)))
	 (setf type new-type))
	(t (StdError "Invalid type specified for pathname."))
	))

(defmethod (pathname set-version) (new-version)
  (cond ((and (FixP new-version) (>= new-version 0))
	 (setf version new-version))
	((null new-version) (setf version NIL))
	((and (IdP new-version)
	      (or (eq new-version 'unspecific)
		  (eq new-version 'wild)
		  (eq new-version 'newest)
		  (eq new-version 'oldest)
		  ))
	 (setf version new-version))
	(t (StdError "Invalid version specified for pathname."))
	))

(de string-to-pathname (s)
  (let ((pn (make-instance 'pathname))
	(i 0)
	j
	ch
	(len (string-length s))
	(name-count 0)
	field
	)
    (while (< i len)
      (setf j (pathname-bite s i))
      (selectq
	(string-fetch s (- j 1))
	(#\: (=> pn set-device (pathname-field-from-string
				(substring s i (- j 1)))))
	(#\> (=> pn set-directory (pathname-field-from-string
				   (substring s (+ i 1) (- j 1)))))
	(#\. (setf name-count (+ name-count 1))
	     (setf field (substring s i (- j 1)))
	     (selectq
	       name-count
	       (1 (=> pn set-name (pathname-field-from-string field))
		  (if (>= j len) (=> pn set-type 'UNSPECIFIC))
		  )
	       (2 (=> pn set-type (pathname-field-from-string field))
		  (if (>= j len) (=> pn set-version 'UNSPECIFIC))
		  )
	       (3 (=> pn set-version (pathname-version-from-string field)))
	       ))
	(t (setf name-count (+ name-count 1))
	   (setf field (substring s i j))
	   (selectq
	     name-count
	     (1 (=> pn set-name (pathname-field-from-string field)))
	     (2 (=> pn set-type (pathname-field-from-string field)))
	     (3 (=> pn set-version (pathname-version-from-string field)))
	     )))
      (setf i j)
      )
    pn
    ))

(de pathname-bite (pn i)
  (let* ((len (string-length pn))
	 (ch (string-fetch pn i))
	 )
    (cond ((= ch #\<)
	   (setf i (+ i 1))
	   (while (< i len)
	     (setf ch (string-fetch pn i))
	     (setf i (+ i 1))
	     (if (= ch #\>) (exit))
	     )
	   )
	  (t
	   (while (< i len)
	     (setf ch (string-fetch pn i))
	     (setf i (+ i 1))
	     (if (= ch #\:) (exit))
	     (if (= ch #\.) (exit))
	     )))
    i
    ))

(de pathname-field-from-string (s)
  (cond ((StringP s)
	 (cond ((string-empty? s) 'UNSPECIFIC)
	       ((string= s "*") 'WILD)
	       (t s)
	       ))
	(t s)))

(de pathname-version-from-string (s)
  (cond ((StringP s)
	 (cond ((string-empty? s) NIL)
	       ((string= s "-2") 'OLDEST)
	       ((string= s "0") 'NEWEST)
	       ((string= s "*") 'WILD)
	       ((string-is-integer s) (string2integer s))
	       (t s)
	       ))
	(t s)))

(de pathname-empty-field? (x)
  (string-empty? (pathname-field-to-string x))
  )

(de pathname-field-to-string (x)
  (cond ((StringP x) x)
	((eq x 'OLDEST) "-2")
	((eq x 'NEWEST) "0")
	((eq x 'UNSPECIFIC) "")
	((eq x 'WILD) "*")
	((null x) "")
	(t (BldMsg "%w" x))))

(de string-is-integer (s)
  (for (from i 0 (string-upper-bound s))
       (always (DigitP (string-fetch s i)))
       ))
