%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% HP9836-DEV.SL - HP9836 NMODE Development Support (not normally loaded)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 January 1983
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-strings fast-int extended-char))
(bothtimes (load strings common))

(fluid '(nmode-source-prefix window-source-prefix))

(setf nmode-source-prefix "n:")
(setf window-source-prefix "w:")

(setf prinlevel 3)
(setf prinlength 10)

(de nmode-compile (s)
  (setf s (nmode-fixup-name s))
  (let ((object-name (string-concat nmode-source-prefix s))
	(source-name (string-concat nmode-source-prefix
				    (string-concat s ".sl")))
	)
    (compile-lisp-file source-name object-name)
    ))

(de window-compile (s)
  (setf s (nmode-fixup-name s))
  (let ((object-name (string-concat window-source-prefix s))
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
