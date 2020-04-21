%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Physical-Screen.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 August 1982
% Revised:     20 December 1982
%
% Adapted from Will Galway's EMODE Virtual Screen package.
%
% A physical screen is a rectangular character display.  Changes to the physical
% screen are made using the Write operation.  These changes are saved and sent
% to the actual display only when REFRESH or FULL-REFRESH is performed.
% FULL-REFRESH should be called to initialize the state of the display.
%
% 20-Dec-82 Alan Snyder
%   Added cached terminal methods to improve efficiency.
%
% 3-Mar-83 17:40:36, Edit by GALWAY
%   Inserted calls to FlushStdOutputBuffer, to make refresh work on the
%   Vax.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors display-char))

(de create-physical-screen (display-terminal)
  (make-instance 'physical-screen 'terminal display-terminal))

(defflavor physical-screen 
  (height                % number of rows (0 indexed)
   maxrow                % highest numbered row
   width                 % number of columns (0 indexed)
   maxcol                % highest numbered column
   cursor-row            % desired cursor position after refresh
   cursor-column         % desired cursor position after refresh
   changed-row-range     % bounds on rows where new-image differs from display
   changed-column-ranges % bounds on columns in each row
   terminal              % the display terminal
   new-image             % new image (after refresh)
   displayed-image       % image on the display terminal
   update-line-method    % terminal's update-line method
   move-cursor-method    % terminal's move-cursor method
   get-char-method       % terminal's get-character method
   convert-char-method   % terminal's convert-character method
   )
  ()
  (gettable-instance-variables height width cursor-row cursor-column)
  (initable-instance-variables terminal)
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private Macros:

(defmacro image-fetch (image row col)
  `(vector-fetch (vector-fetch ,image ,row) ,col))
(defmacro image-store (image row col value)
  `(vector-store (vector-fetch ,image ,row) ,col ,value))

(defmacro range-create ()
  `(cons 10000 0))
(defmacro range-cons (min max)
  `(cons ,min ,max))
(defmacro range-min (r)
  `(car ,r))
(defmacro range-max (r)
  `(cdr ,r))
(defmacro range-set-min (r x)
  `(rplaca ,r ,x))
(defmacro range-set-max (r x)
  `(rplacd ,r ,x))
(defmacro range-reset (r)
  `(let ((*r* ,r))
     (rplaca *r* 10000) (rplacd *r* 0)))
(defmacro range-empty? (r)
  `(< (range-max ,r) (range-min ,r)))
(defmacro range-within? (r x) 
  `(and (<= (range-min ,r) ,x) (<= ,x (range-max ,r))))
(defmacro range-extend (r x)
  `(let ((*r* ,r) (*x* ,x))
     % New minimum if x < old minimum
     (if (< *x* (range-min *r*)) (range-set-min *r* *x*))
     % New maximum if x > old maximum.
     (if (> *x* (range-max *r*)) (range-set-max *r* *x*))
     ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Public methods:

(defmethod (physical-screen ring-bell) ()
  (=> terminal ring-bell))

(defmethod (physical-screen enter-raw-mode) ()
  (=> terminal enter-raw-mode))

(defmethod (physical-screen leave-raw-mode) ()
  (=> terminal leave-raw-mode))

(defmethod (physical-screen get-character) ()
  (apply get-char-method (list terminal)))

(defmethod (physical-screen convert-character) (ch)
  (apply convert-char-method (list terminal ch)))

(defmethod (physical-screen normal-enhancement) ()
  (=> terminal normal-enhancement))

(defmethod (physical-screen highlighted-enhancement) ()
  (=> terminal highlighted-enhancement))

(defmethod (physical-screen supported-enhancements) ()
  (=> terminal supported-enhancements))

(defmethod (physical-screen write) (ch row col)
  (when (~= ch (image-fetch new-image row col))
    (image-store new-image row col ch)
    (range-extend changed-row-range row)
    (range-extend (vector-fetch changed-column-ranges row) col)
    ))

(defmethod (physical-screen set-cursor-position) (row col)
  (setf cursor-row row)
  (setf cursor-column col))

(defmethod (physical-screen refresh) (breakout-allowed)
  (for (from row (range-min changed-row-range)
	     (range-max changed-row-range))
       (for break-count 0 (+ break-count 1))
       (with changed-columns breakout)
       (until (and breakout-allowed
		   (= (& break-count 3) 0) % test every 4 lines
		   (input-available?)
		   (setf breakout T)))
       (do
	(setf changed-columns (vector-fetch changed-column-ranges row))
	(when (not (range-empty? changed-columns))
	  (apply update-line-method
		 (list terminal
		       row
		       (vector-fetch displayed-image row)
		       (vector-fetch new-image row)
		       changed-columns
		       ))
	  (range-reset changed-columns)
          (FlushStdOutputBuffer)))
       (finally
	(range-set-min changed-row-range row)
	(if (range-empty? changed-row-range)
	  (range-reset changed-row-range))
	(if (not (or breakout
		     (and breakout-allowed (input-available?))))
	  (apply move-cursor-method
		 (list terminal cursor-row cursor-column)))

        % Perhaps the "move-cursor-method" should do the flushing?
        (FlushStdOutputBuffer)
	)
       ))

(defmethod (physical-screen full-refresh) (breakout-allowed)
  (=> terminal erase)
  (for (from row 0 maxrow)
       (with line range)
       (do (setq range (vector-fetch changed-column-ranges row))
	   (range-set-min range 0)
	   (range-set-max range maxcol)
	   (setf line (vector-fetch displayed-image row))
	   (for (from col 0 maxcol)
		(do (vector-store line col (char space)))
	        )
	   ))
  (range-set-min changed-row-range 0)
  (range-set-max changed-row-range maxrow)
  (=> self refresh breakout-allowed)
  )

(defmethod (physical-screen write-to-stream) (s)
  (for (from row 0 maxrow)
       (with line)
       (do (setf line (vector-fetch displayed-image row))
	   (for (from col 0 maxcol)
		(do (=> s putc (dc-character-code (vector-fetch line col))))
	        )
	   (=> s put-newline)
	   ))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private methods:

(defmethod (physical-screen init) (init-plist) % For internal use only!
  (setf height (=> terminal height))
  (setf maxrow (- height 1))
  (setf width (=> terminal width))
  (setf maxcol (- width 1))
  (setf cursor-row 0)
  (setf cursor-column 0)
  (setf displayed-image (=> self create-image))
  (setf new-image (=> self create-image))
  (setf changed-row-range (range-create))
  (setf changed-column-ranges (MkVect maxrow))
  (for (from row 0 maxrow)
       (do (vector-store changed-column-ranges row (range-create))))
  (setf update-line-method (object-get-handler terminal 'update-line))
  (setf move-cursor-method (object-get-handler terminal 'move-cursor))
  (setf get-char-method (object-get-handler terminal 'get-character))
  (setf convert-char-method (object-get-handler terminal 'convert-character))
  )

(defmethod (physical-screen create-image) ()
  (let ((image (MkVect maxrow))
	(line (MkVect maxcol))
	)
    (for (from col 0 maxcol)
	 (do (vector-store line col (char space)))
	 )
    (for (from row 0 maxrow)
	 (do (vector-store image row (copyvector line)))
	 )
    image))
