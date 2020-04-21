%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% WINDOWS-VAX.SL - Vax-Unix Windows Stuff (intended only for Vax version)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        4 April 1983
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load fast-strings fast-int))
(bothtimes (load strings common))

(fluid '(window-file-list window-source-prefix window-binary-prefix))

(if (or (unboundp 'window-source-prefix) (null window-source-prefix))
  (setf window-source-prefix "$pw/"))

(if (or (unboundp 'window-binary-prefix) (null window-binary-prefix))
  (setf window-binary-prefix "$pwb/"))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Stuff for Building WINDOWS:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de window-fixup-name (s) s)

(de window-load-all ()
  (for (in s window-file-list)
       (do (window-load s))
       ))

(de window-load (s)
  (window-faslin window-binary-prefix s)
  )

(de window-faslin (directory-name module-name)
  (setf module-name (window-fixup-name module-name))
  (setf module-name (string-concat module-name ".b"))
  (let ((object-name (string-concat directory-name module-name)))
    (if (filep object-name)
      (faslin object-name)
      (continuableerror 99
       (bldmsg "Unable to FASLIN %w" object-name)
       (list 'faslin object-name)
       ))))

(setf window-file-list
  (list
   "hp2648a"
   "physical-screen"
   "shared-physical-screen"
   "virtual-screen"
   "vt52x"
   ))
