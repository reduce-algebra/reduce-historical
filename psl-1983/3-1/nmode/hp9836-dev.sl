%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% HP9836-DEV.SL - HP9836 NMODE Development Support (not normally loaded)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 January 1983
% Revised:     4 April 1983
%
% 4-Apr-83 Alan Snyder
%  Changes relating to keeping NMODE source and binary files in separate
%  directories.
% 16-Mar-83 Alan Snyder
%  New function: window-ftp.
% 14-Mar-83 Alan Snyder
%  Changed nmode-compile and window-compile to take any number of arguments.
%  New function: nmode-ftp.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-strings extended-char))
(bothtimes (load strings common))

(fluid '(nmode-source-prefix
	 nmode-binary-prefix
	 window-source-prefix
	 window-binary-prefix
	 ))

(setf prinlevel 3)
(setf prinlength 10)

(dn nmode-compile (s-list)
  (for (in s s-list)
       (do (nmode-compile-1 s))
       ))

(de nmode-compile-1 (s)
  (setf s (nmode-fixup-name s))
  (let ((object-name (string-concat nmode-binary-prefix s))
	(source-name (string-concat nmode-source-prefix
				    (string-concat s ".sl")))
	)
    (compile-lisp-file source-name object-name)
    ))

(dn window-compile (s-list)
  (for (in s s-list)
       (do (window-compile-1 s))
       ))

(de window-compile-1 (s)
  (setf s (nmode-fixup-name s))
  (let ((object-name (string-concat window-binary-prefix s))
	(source-name (string-concat window-source-prefix
				    (string-concat s ".sl")))
	)
    (compile-lisp-file source-name object-name)
    ))

(de pu-compile (s)
  (let ((object-name (string-concat "pl:" s))
	(source-name (string-concat "pu:" (string-concat s ".sl")))
	)
    (compile-lisp-file source-name object-name)
    ))

(de phpu-compile (s)
  (let ((object-name (string-concat "pl:" s))
	(source-name (string-concat "phpu:" (string-concat s ".sl")))
	)
    (compile-lisp-file source-name object-name)
    ))

(de nmode-compile-all ()
  (for (in s nmode-file-list)
       (do (nmode-compile s))
       ))

(de window-compile-all ()
  (for (in s window-file-list)
       (do (window-compile s))
       ))

(dn nmode-ftp (s-list)
  (let* ((sout (open-output "FTP-NMODE"))
	 (dummy (make-string 1 0))
	 )
    (=> sout putl "XTERM")
    (string-store dummy 0 128)
    (=> sout puts dummy)
    (for (in s s-list)
	 (do (nmode-ftp-1 s sout))
	 )
    (=> sout putl "")
    (=> sout close)
    ))

(de nmode-ftp-1 (s sout)
  (=> sout puts "S") % Send command
  (=> sout putl (string-concat nmode-source-prefix (nmode-fixup-name s) ".sl"))
  (=> sout putl (string-concat "n:" s ".sl"))
  )

(dn window-ftp (s-list)
  (let* ((sout (open-output "FTP-WINDOW"))
	 (dummy (make-string 1 0))
	 )
    (=> sout putl "XTERM")
    (string-store dummy 0 128)
    (=> sout puts dummy)
    (for (in s s-list)
	 (do (window-ftp-1 s sout))
	 )
    (=> sout putl "")
    (=> sout close)
    ))

(de window-ftp-1 (s sout)
  (=> sout puts "S") % Send command
  (=> sout putl (string-concat window-source-prefix (window-fixup-name s) ".sl"))
  (=> sout putl (string-concat "n:" s ".sl"))
  )
