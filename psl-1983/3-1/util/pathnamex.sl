%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% PathNameX.SL - Useful Functions involving Pathnames
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        27 September 1982
% Revised:     4 March 1983
%
% 4-Mar-83 Alan Snyder
%  Added maybe-pathname function.
% 4-Feb-83 Alan Snyder
%  Added pathname-without-name function.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load pathnames))

(de maybe-pathname (x)
  % Attempt to convert X to a pathname.  If not possible, return NIL.
  (let ((result (errset (pathname x) NIL)))
    (when (listp result) (car result))
    ))

(de pathname-without-name (pn)
  % Return a pathname like PN but with no NAME, TYPE, or VERSION.

  (setf pn (pathname pn))
  (make-pathname 'host (pathname-host pn)
		 'device (pathname-device pn)
		 'directory (pathname-directory pn)
		 ))

(de pathname-without-type (pn)
  % Return a pathname like PN but with no TYPE or VERSION.

  (setf pn (pathname pn))
  (make-pathname 'host (pathname-host pn)
		 'device (pathname-device pn)
		 'directory (pathname-directory pn)
		 'name (pathname-name pn)
		 ))

(de pathname-without-version (pn)
  % Return a pathname like PN but with no VERSION.

  (setf pn (pathname pn))
  (make-pathname 'host (pathname-host pn)
		 'device (pathname-device pn)
		 'directory (pathname-directory pn)
		 'name (pathname-name pn)
		 'type (pathname-type pn)
		 ))

(de pathname-set-default-type (pn typ)
  % Return a pathname like PN, except that if PN specifies no TYPE,
  % then with type TYP and no version.

  (setf pn (pathname pn))
  (cond ((not (pathname-type pn))
	 (make-pathname 'host (pathname-host pn)
			'device (pathname-device pn)
			'directory (pathname-directory pn)
			'name (pathname-name pn)
			'type typ
			))
	(t pn)))

(de pathname-set-type (pn typ)
  % Return a pathname like PN, except with type TYP and no version.

  (setf pn (pathname pn))
  (make-pathname 'host (pathname-host pn)
		 'device (pathname-device pn)
		 'directory (pathname-directory pn)
		 'name (pathname-name pn)
		 'type typ
		 ))

