%
% Windows.SL - Window Collection Manipulation Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        12 July 1982
%
% This file contains functions that manipulate the set of existing
% windows.  It is intended that someday EMODE will be reorganized
% so that all such functions will eventually be in this file.
%
% This file requires COMMON.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(WindowList CurrentWindowDescriptor CurrentBufferName
	 BufferPreviousBuffer WindowsBufferName))

(de window-kill-buffer ()
  % This function disassociates the current window with the buffer
  % currently associated with that window.  If the buffer is not
  % associated with any other window, it is killed.  A new buffer
  % is selected to become associated with the window.  The preferred
  % choice is the buffer's "previous buffer".

  (prog (buffer-needed preferred-buffer detached-buffer)
    (setf detached-buffer WindowsBufferName)
    (SelectBuffer detached-buffer) % allow access to buffer variables
    (setf preferred-buffer BufferPreviousBuffer)
    (setf buffer-needed nil)
    (for
      (in WindowDescriptor WindowList)
      (when (neq WindowDescriptor CurrentWindowDescriptor))
      (while (not buffer-needed))
      (do (if (and (atsoc 'WindowsBufferName WindowDescriptor)
		   (eq (cdr (atsoc 'WindowsBufferName WindowDescriptor))
		       detached-buffer))
	    (setf buffer-needed t)))
      )
    (if (not buffer-needed)
        (buffer-kill detached-buffer))
    (select-buffer-if-existing preferred-buffer)
    (setf WindowsBufferName CurrentBufferName)
    (EstablishCurrentMode)
    (if (not buffer-needed) 
      (write-prompt (BldMsg "Buffer %w deleted." detached-buffer)))
    ))
