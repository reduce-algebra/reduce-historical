%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Buffer-IO.SL - PSL I/O to and from NMODE buffers
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        26 August 1982
% Revised:     18 February 1983
%
% Adapted from Will Galway's EMODE
%
% 18-Feb-83 Alan Snyder
%   Fix to adjust an exposed window when displaying output.
% 16-Feb-83 Alan Snyder
%   Recode using objects; add output cache for efficiency.
%   Remove time-since-last-redisplay check (it causes a 2X slowdown);
%   now display output only after Newline or cache full.
%   Declare -> Declare-Flavor.
% 30-Dec-82 Alan Snyder
%   Add declarations for buffers and windows; use fast-vectors (for efficiency).
% 27-Dec-82 Alan Snyder
%   Use generic arithmetic for Time (for portability); reformat.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects fast-vectors))

(fluid '(nmode-current-window *nmode-init-running))

(DefConst MaxChannels 32) % Maximum number of channels supported by PSL.

(defflavor buffer-channel
  (
   (editor-function NIL)	% NIL or a function to obtain new input
   (input-buffer NIL)		% NIL or a buffer to obtain input from
   (input-position NIL)		% the current read pointer
   (output-buffer NIL)		% NIL or a buffer to send output to
   (output-cache NIL)		% cache of output (for efficiency)
   output-cache-pos		% pointer into output cache
   )
  ()
  (settable-instance-variables)
  )

(fluid '(buffer-channel-vector))

(when (or (not (BoundP 'buffer-channel-vector)) (null buffer-channel-vector))
  (setf buffer-channel-vector (MkVect (const MaxChannels)))
  )

(fluid '(*outwindow		% T => expose output window on output
	 ))

(setf *outwindow T)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(declare-flavor text-buffer input-buffer output-buffer)
(declare-flavor buffer-window w)
(declare-flavor buffer-channel bc)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de OpenBufferChannel (input-buffer output-buffer Editor)

  % Open a channel for buffer I/O.  Input-Buffer and Output-Buffer may be buffer
  % objects or NIL.  Input will be read from the current location in the Input
  % Buffer.  Output will be inserted at the current location in the Output
  % Buffer.  Editor may be a function object (ID) or NIL.  The Editor function
  % can be used if you want something to "happen" every time a reader begins to
  % read from the channel.  If Editor is NIL, then the reader will simply
  % continue reading from the current location in the input buffer.

  (setf SpecialWriteFunction* 'buffer-print-character)
  (setf SpecialReadFunction* 'buffer-read-character)
  (setf SpecialCloseFunction* 'buffer-channel-close)
  (let ((chn (open "buffers" 'SPECIAL))
	(bc (make-instance 'buffer-channel))
	)
    (vector-store buffer-channel-vector chn bc)
    (=> bc set-input-buffer input-buffer)
    (=> bc set-input-position (and input-buffer (=> input-buffer position)))
    (=> bc set-output-buffer output-buffer)
    (=> bc set-editor-function Editor)
    chn
    ))

(de buffer-channel-close (chn)
  % Close up an NMODE buffer channel.
  (vector-store buffer-channel-vector chn NIL)
  )

(de buffer-channel-set-input-buffer (chn input-buffer)
  (let ((bc (vector-fetch buffer-channel-vector chn)))
    (when bc
      (=> bc set-input-buffer input-buffer)
      (=> bc set-input-position (=> input-buffer position))
      )))

(de buffer-channel-set-input-position (chn bp)
  (let ((bc (vector-fetch buffer-channel-vector chn)))
    (when bc
      (=> bc set-input-position bp)
      )))

(de buffer-channel-set-output-buffer (chn output-buffer)
  (let ((bc (vector-fetch buffer-channel-vector chn)))
    (when bc
      (=> bc set-output-buffer output-buffer)
      )))

(de buffer-print-character (chn ch)
  (let ((bc (vector-fetch buffer-channel-vector chn)))
    (when bc
      (=> bc putc ch)
      )))

(de buffer-channel-flush (chn)
  (let ((bc (vector-fetch buffer-channel-vector chn)))
    (when bc
      (=> bc flush)
      )))

(defmethod (buffer-channel flush) ()
  % If there is output lingering in the output cache, then append it to the
  % output buffer and return T.  Otherwise return NIL.

  (when (and output-buffer output-cache (> output-cache-pos 0))
    (let ((old-pos (=> output-buffer position)))
      (=> output-buffer move-to-buffer-end)
      (=> output-buffer insert-string
	  (substring output-cache 0 output-cache-pos))
      (=> output-buffer set-position old-pos)
      (setf output-cache-pos 0)
      T
      )))

(defmethod (buffer-channel refresh) ()
  % If this channel is being used for output, then refresh the display of that
  % output.  The buffer will automatically be exposed in a window (if
  % requested by the *OutWindow flag), the output cache will be flushed, the
  % display window will be adjusted, and the screen refreshed.

  (when output-buffer
    (if (and *OutWindow
	     (not *nmode-init-running)
	     (not (buffer-is-displayed? output-buffer)))
      (nmode-expose-output-buffer output-buffer))
    (let ((window-list (find-buffer-in-exposed-windows output-buffer)))
      (when window-list
	(=> self flush)
	(nmode-adjust-output-window (car window-list))
	))))

(defmethod (buffer-channel put-newline) ()
  (=> self flush)
  (let ((old-pos (=> output-buffer position)))
    (=> output-buffer move-to-buffer-end)
    (=> output-buffer insert-eol)
    (=> output-buffer set-position old-pos)
    )
  (=> self refresh)
  )

(defmethod (buffer-channel putc) (ch)
  % "Print" character CH by appending it to the output buffer.
  (if (= ch #\EOL)
    (=> self put-newline)
    (when output-buffer
      (when (null output-cache)
	(setf output-cache (make-string 200 #\space))
	(setf output-cache-pos 0)
	)
      (string-store output-cache output-cache-pos ch)
      (setf output-cache-pos (+ output-cache-pos 1))
      (when (>= output-cache-pos 200)
	(=> self flush)
	(=> self refresh)
	))))

(de nmode-adjust-output-window (w)
  (let ((output-buffer (=> w buffer)))
    (=> w set-position (=> output-buffer buffer-end-position))
    (nmode-adjust-window w)
    (if (=> w exposed?) (nmode-refresh))
    ))

(de buffer-read-character (chn)
  (let ((bc (vector-fetch buffer-channel-vector chn)))
    (when bc
      (=> bc getc)
      )))

(defmethod (buffer-channel getc) ()

  % Read a character from the input buffer; advance over that character.
  % Return End Of File if at end of buffer or if no buffer.  If the "read
  % point" equals the "buffer cursor", then the "buffer cursor" will be
  % advanced also.

  (if (not input-buffer)
    #\EOF
    % Otherwise (there is an input buffer)
    (let* ((old-position (=> input-buffer position))
	   (was-at-cursor (buffer-position-equal input-position old-position))
	   result
	   )
      (=> input-buffer set-position input-position)
      (if (=> input-buffer at-buffer-end?)
	(setf result #\EOF)
	% Otherwise (not at end of buffer)
	(setf result (=> input-buffer next-character))
	(=> input-buffer move-forward)
	(setf input-position (=> input-buffer position))
	)
      (if (not was-at-cursor)
	(=> input-buffer set-position old-position))
      (if *ECHO (=> self putc result))
      result
      )))

(de MakeInputAvailable ()
  % THIS IS THE MAGIC FUNCTION invoked by READ, and other "reader functions".
  % IN* is a FLUID (actually GLOBAL) variable.
  (let ((bc (vector-fetch buffer-channel-vector IN*)))
    (when bc
      (=> bc run-editor)
      )))

(defmethod (buffer-channel run-editor) ()
  (if editor-function (apply editor-function (list IN*)))
  NIL
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(undeclare-flavor input-buffer output-buffer)
(undeclare-flavor w)
(undeclare-flavor bc)
