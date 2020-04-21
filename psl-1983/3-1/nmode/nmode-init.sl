%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NMODE-INIT.SL - NMODE Initialization
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        24 August 1982
% Revised:     11 March 1983
%
% 11-Mar-83 Alan Snyder
%  Buffer-Create-Unselectable -> Create-Unnamed-Buffer.
%  Create buffer browser.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects))

(fluid '(lisp-interface-mode
	 input-mode
	 nmode-main-buffer
	 nmode-output-buffer
	 nmode-input-buffer
	 nmode-initialized
	 ))

(setf nmode-initialized NIL)

(de nmode-initialize ()
  (cond ((not nmode-initialized)
	 (nmode-initialize-extended-input)
	 (nmode-initialize-modes)
	 (nmode-initialize-buffers) % modes must be initialized previously
	 (nmode-initialize-screen-layout) % buffers must be init previously
	 (nmode-initialize-kill-ring)
	 (create-buffer-browser)
	 (enable-nmode-break)
	 (setf nmode-initialized T)
	 )))

(de nmode-initialize-buffers ()
  (if (null nmode-main-buffer)
    (setf nmode-main-buffer
      (buffer-create "MAIN" lisp-interface-mode)))
  (if (null nmode-output-buffer)
    (setf nmode-output-buffer
      (buffer-create "OUTPUT" lisp-interface-mode)))
  (if (null nmode-input-buffer)
    (setf nmode-input-buffer
      (create-unnamed-buffer input-mode)))
  )
