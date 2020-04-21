% Some people desire a different date format on the status line.  By 
%  setting *DateSelect* to the appropriate value (see Clockdatetime in
%  exec), this will be done.
(Global '(*DateSelect*))

(defmethod (window-label &rewrite) ()
  % Unconditionally rewrite the entire label.
  (let ((buffer (=> window buffer)))
    (setf screen (=> window screen))
    (setf buffer-name (=> buffer name))
    (setf buffer-mode (=> buffer mode))
    (setf minor-modes nmode-minor-modes)
    (setf buffer-file (=> buffer file-name))
    (setf buffer-top (=> window buffer-top))
    (setf buffer-left (=> window buffer-left))
    (setf buffer-size (=> buffer visible-size))
    (setf buffer-modified (=> buffer modified?))
    (setf current-window nmode-major-window)
    (if PromptString* (setf prompt-string PromptString*))
    (let ((old-enhancement (=> screen default-enhancement)))
      (=> screen set-default-enhancement label-enhancement)
      (setf pos 0)
      (if (eq window current-window)
       (progn 
         (cond ((telerayp) (=> self &write-char 132)))
	 (=> self &write-string "NMODE ")
         (cond ((telerayp) (=> self &write-char 136))))
       (progn 
         (cond ((telerayp) (=> self &write-char 136)))
	 (=> self &write-string "      ")
         (cond ((telerayp) (=> self &write-char 136)))))
      (=> self &write-string (concat (clocktimedate *DateSelect*)
				     " "))
      (=> self &write-string (getloadaverage))
      (=> self &write-string (=> buffer-mode name))
      (if (and minor-modes (eq window current-window))
	(let ((leader-string " ("))
	  (for (in minor-mode minor-modes)
	       (do 
		(=> self &write-string leader-string)
		(setf leader-string " ")
		(=> self &write-string (=> minor-mode name))
		))
	  (=> self &write-string ")")
	  ))
      % Omit the buffer name if it is directly derived from the file name.
      (cond ((or (not buffer-file)
		 (not (string= buffer-name
			       (filename-to-buffername buffer-file))))
	     (=> self &write-string " [")
	     (=> self &write-string buffer-name)
	     (=> self &write-string "]")
	     ))
      (when buffer-file
	(=> self &write-string " ")
	(=> self &write-string buffer-file)
	)
      (when (> buffer-left 0)
	(=> self &write-string " >")
	(=> self &write-string (BldMsg "%d" buffer-left))
	)
      (cond
       ((and (= buffer-top 0) (<= buffer-size (=> window height)))
	% The entire buffer is showing on the screen.
	% Do nothing.
	)
       ((= buffer-top 0)
	% The window is showing the top end of the buffer.
	(=> self &write-string " --TOP--")
	)
       ((>= buffer-top (- buffer-size (=> window height)))
	% The window is showing the bottom end of the buffer.
	(=> self &write-string " --BOT--")
	)
       (t % Otherwise...
	(let ((percentage (/ (* buffer-top 100) buffer-size)))
	  (=> self &write-string " --")
	  (=> self &write-char (+ #/0 (/ percentage 10)))
	  (=> self &write-char (+ #/0 (// percentage 10)))
	  (=> self &write-string "%--")
	  )))
      (if buffer-modified
	(=> self &write-string " *"))
      (when (and (StringP prompt-string) (eq buffer nmode-output-buffer))
	(=> self &write-string " ")
	(=> self &advance-pos (- width (string-length prompt-string)))
	(=> screen set-default-enhancement prompt-enhancement)
	(=> self &write-string prompt-string)
	)
      (=> screen clear-to-eol maxrow pos)
      (=> screen set-default-enhancement old-enhancement)
      )))

(de telerayp nil (eq terminal-type 7))


