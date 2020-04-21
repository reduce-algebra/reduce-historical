%
% Buffer.SL - Individual Buffer Manipulation Functions
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        27 July 1982
%
% This file contains functions that manipulate individual buffers.
% It is intended that someday EMODE will be reorganized
% so that all such functions will eventually be in this file.
%
% This file requires COMMON.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(CurrentLine CurrentBufferSize CurrentLineIndex point))

(de char-blank? (ch)
  (or (= ch (char space)) (= ch (char tab))))

(de current-line-length () (length CurrentLine))

(de current-line-empty () (= (length CurrentLine) 0))

(de current-line-blank? ()
  (for (in ch CurrentLine)
       (always (char-blank? ch))
       ))

(de at-buffer-end? ()
  (and (current-line-is-last?) (= point (current-line-length))))

(de at-buffer-start? ()
  (and (= CurrentLineIndex 0) (= point 0)))

(de current-line-is-last? ()
  (>= CurrentLineIndex (- CurrentBufferSize 1)))

(de current-line-is-first? ()
  (= CurrentLineIndex 0))

(de current-line-fetch (n) (car (pnth CurrentLine (+ n 1))))
(de current-line-store (n c)
  (setf CurrentLine (InsertListEntry (DeleteListEntry CurrentLine n) n c)))

(de current-buffer-size ()

  % Return the number of lines in the current buffer.  Note that if the
  % buffer does not end with an incomplete line, then its last line will
  % be empty.  (See CURRENT-BUFFER-VISIBLE-SIZE, which corrects for this
  % anomaly.)

  CurrentBufferSize)

(de current-buffer-visible-size ()

  % Return the visible number of lines in the current buffer.  In other words,
  % don't count the last line if it is empty, since that is just an artifact of
  % the buffer representation.

  (let* ((buffer-size CurrentBufferSize)
	 (last-line-index (- buffer-size 1))
	 )
    (if (= CurrentLineIndex last-line-index)  % CurrentLine hack!
	(if CurrentLine buffer-size (- buffer-size 1))
	(if (>= (size (GetBufferText last-line-index)) 0)
	    buffer-size (- buffer-size 1))
	)))

(de current-buffer-goto (line-number char-number)
  (SelectLine line-number)
  (setf point char-number)
  )

(de move-to-next-line ()
  (let ((next-index (+ CurrentLineIndex 1)))
    (cond ((< next-index CurrentBufferSize)
	     (SelectLine next-index) (setf point 0))
	  (t (setf point (length CurrentLine)) (PutLine))
    )))

(de move-to-previous-line ()
  (let ((next-index (- CurrentLineIndex 1)))
    (cond ((>= next-index 0)
	     (SelectLine next-index) (setf point 0))
	  (t (setf point 0) (PutLine))
    )))

