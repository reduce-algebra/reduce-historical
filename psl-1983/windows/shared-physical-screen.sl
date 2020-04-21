%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Shared-Physical-Screen.SL
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        17 August 1982
% Revised:     22 February 1983
%
% Inspired by Will Galway's EMODE Virtual Screen package.
%
% A shared-physical-screen is a rectangular character display whose display
% area is shared by a number of different owners.  An owner can be any object
% that supports the following operations:
%
%  Assert-Ownership () - assert ownership of all desired screen locations
%  Send-Changes (break-ok) - send all changed contents to the shared screen
%  Send-Contents (break-ok) - send entire contents to the shared screen
%  Screen-Cursor-Position () - return desired cursor position on screen
%
% Each character position on the physical screen is owned by a single owner.
% Each owner is responsible for asserting ownership of those character
% positions it wishes to be able to write on.  The actual ownership of each
% character position is determined by a prioritized list of owners.  Owners
% assert ownership in reverse order of priority; the highest priority owner
% therefore appears to "overlap" all other owners.
%
% A shared physical screen object provides an opaque interface: no access to
% the underlying physical screen object should be required.
%
% 22-Feb-83 Alan Snyder
%  Declare -> Declare-Flavor.
% 27-Dec-82 Alan Snyder
%  Changed SELECT-PRIMARY-OWNER and REMOVE-OWNER to avoid redundant
%  recomputation (and screen rewriting).
% 21-Dec-82 Alan Snyder
%  Efficiency hacks: Special tests for owners that are virtual-screens.
%  Added methods: &GET-OWNER-CHANGES, &GET-OWNER-CONTENTS, and
%  &ASSERT-OWNERSHIP.
% 16-Dec-82 Alan Snyder
%  Bug fix: SET-SCREEN failed to update size (invoked the wrong method).
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects))
(CompileTime (load fast-int fast-vectors))
  
(de create-shared-physical-screen (physical-screen)
  (make-instance 'shared-physical-screen 'screen physical-screen))

(defflavor shared-physical-screen (
  height                % number of rows (0 indexed)
  maxrow                % highest numbered row
  width                 % number of columns (0 indexed)
  maxcol                % highest numbered column
  (owner-list NIL)	% prioritized list of owners (lowest priority first)
  (recalculate T)	% T => must recalculate ownership
  owner-map		% maps screen location to owner (or NIL)
  screen                % the physical-screen
  )
  ()
  (gettable-instance-variables height width)
  (initable-instance-variables screen)
  )

(declare-flavor physical-screen screen)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private Macros:

(defmacro map-fetch (map row col)
  `(vector-fetch (vector-fetch ,map ,row) ,col))
(defmacro map-store (map row col value)
  `(vector-store (vector-fetch ,map ,row) ,col ,value))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Public methods:

(defmethod (shared-physical-screen ring-bell) ()
  (=> screen ring-bell))

(defmethod (shared-physical-screen enter-raw-mode) ()
  (=> screen enter-raw-mode))

(defmethod (shared-physical-screen leave-raw-mode) ()
  (=> screen leave-raw-mode))

(defmethod (shared-physical-screen get-character) ()
  (=> screen get-character))

(defmethod (shared-physical-screen convert-character) (ch)
  (=> screen convert-character ch))

(defmethod (shared-physical-screen normal-enhancement) ()
  (=> screen normal-enhancement))

(defmethod (shared-physical-screen highlighted-enhancement) ()
  (=> screen highlighted-enhancement))

(defmethod (shared-physical-screen supported-enhancements) ()
  (=> screen supported-enhancements))

(defmethod (shared-physical-screen write-to-stream) (s)
  (=> screen write-to-stream s))

(defmethod (shared-physical-screen set-screen) (new-screen)
  (setf screen new-screen)
  (=> self &new-screen)
  )

(defmethod (shared-physical-screen owner) (row col)

  % Return the current owner of the specified screen location.

  (if recalculate (=> self &recalculate-ownership))
  (if (and (>= row 0) (<= row maxrow) (>= col 0) (<= col maxcol))
    (map-fetch owner-map row col)))

(defmethod (shared-physical-screen select-primary-owner) (owner)

  % Make the specified OWNER the primary owner (adding it to the list of owners,
  % if not already there).

  (when (not (eq (lastcar owner-list) owner)) % redundancy check
    (setf owner-list (DelQIP owner owner-list))
    (setf owner-list (aconc owner-list owner))
    (when (not recalculate)
      (=> self &assert-ownership owner)
      (=> self &get-owner-contents owner nil)
      (=> self &update-cursor owner)
      )))

(defmethod (shared-physical-screen remove-owner) (owner)

  % Remove the specified owner from the list of owners.  The owner will lose
  % ownership of his screen area.  Screen ownership will be recalculated in its
  % entirety when necessary (to determine the new ownership of the screen area).

  (when (memq owner owner-list) % redundancy check
    (setf owner-list (DelQIP owner owner-list))
    (setf recalculate T)
    ))

(defmethod (shared-physical-screen refresh) (breakout-allowed)

  % Update the screen: obtain changed contents from the owners,
  % send it to the screen, refresh the screen.

  (if recalculate
    (=> self &recalculate-ownership)
    (=> self &get-owners-changes breakout-allowed)
    )
  (=> screen refresh breakout-allowed))

(defmethod (shared-physical-screen full-refresh) (breakout-allowed)

  % Just like REFRESH, except that the screen is cleared first.  This operation
  % should be used to initialize the state of the screen when the program
  % starts or when uncontrolled output may have occured.

  (if recalculate
    (=> self &recalculate-ownership)
    (=> self &get-owners-changes breakout-allowed)
    )
  (=> screen full-refresh breakout-allowed))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Semi-Private methods

% The following methods are for use only by owners to perform the
% AssertOwnership operation when invoked by this object:

(defmethod (shared-physical-screen set-owner) (row col owner)
  (if (and (>= row 0) (<= row maxrow) (>= col 0) (<= col maxcol))
    (map-store owner-map row col owner)))

(defmethod (shared-physical-screen set-owner-region) (row col h w owner)
  % This method provided for convenience and efficiency.
  (let ((last-row (+ row (- h 1)))
	(last-col (+ col (- w 1)))
	(map owner-map)
	)
    (cond ((and (<= row maxrow) (<= col maxcol) (>= last-row 0) (>= last-col 0))
	   (if (< row 0) (setf row 0))
	   (if (< col 0) (setf col 0))
	   (if (> last-row maxrow) (setf last-row maxrow))
	   (if (> last-col maxcol) (setf last-col maxcol))
	   (for (from r row last-row)
		(do (for (from c col last-col)
			 (do
			  (map-store map r c owner))
			 )))))))

% The following method is for use only by owners:

(defmethod (shared-physical-screen write) (ch row col owner)

  % Conditional write: write the specified character to the specified location
  % only if that location is owned by the specified owner.  The actual display
  % will not be updated until REFRESH or FULL-REFRESH is performed.

  (if (and (>= row 0) (<= row maxrow) (>= col 0) (<= col maxcol))
    (progn
      (if recalculate (=> self &recalculate-ownership))
      (if (eq owner (map-fetch owner-map row col))
        (=> screen write ch row col)))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Private methods:

(defmethod (shared-physical-screen init) (init-plist)
  (=> self &new-screen)
  )

(defmethod (shared-physical-screen &new-screen) ()
  (setf height (=> screen height))
  (setf width (=> screen width))
  (=> self &new-size)
  )

(defmethod (shared-physical-screen &new-size) ()
  (if (< height 0) (setf height 0))
  (if (< width 0) (setf width 0))
  (setf maxrow (- height 1))
  (setf maxcol (- width 1))
  (setf owner-map (mkvect maxrow))
  (for (from row 0 maxrow)
       (do (iputv owner-map row (mkvect maxcol))))
  (setf recalculate t))

(defmethod (shared-physical-screen &recalculate-ownership) ()

  % Reset ownership to NIL, then ask all OWNERS to assert ownership.
  % Then ask all OWNERS to send all contents.

  (let ((map owner-map))
    (for (from r 0 maxrow)
	 (do (for (from c 0 maxcol)
		  (do (map-store map r c NIL))))))
  (for (in owner owner-list)
       (do (=> self &assert-ownership owner)))
  (setf recalculate NIL)
  (=> self &get-owners-contents))

(defmethod (shared-physical-screen &get-owners-changes) (breakout-allowed)

  % Ask all OWNERS to send any changed contents.

  (for (in owner owner-list)
       (with last-owner)
       (do (=> self &get-owner-changes owner breakout-allowed)
	   (setf last-owner owner))
       (finally
	 (if last-owner (=> self &update-cursor last-owner)))
       )
  )

(defmethod (shared-physical-screen &get-owner-changes) (owner breakout-allowed)
  (if (eq (object-type owner) 'virtual-screen) % hack for efficiency
    (virtual-screen$send-changes owner breakout-allowed)
    (=> owner send-changes breakout-allowed)
    ))
  
(defmethod (shared-physical-screen &get-owners-contents) (breakout-allowed)

  % Ask all OWNERS to send all of their contents; unowned screen area
  % is blanked.

  (let ((map owner-map))
    (for (from r 0 maxrow)
	 (do (for (from c 0 maxcol)
		  (do (if (null (map-fetch map r c))
			  (=> screen write #\space r c)))))))
  (for (in owner owner-list)
       (with last-owner)
       (do (=> self &get-owner-contents owner breakout-allowed)
	   (setf last-owner owner))
       (finally
	 (if last-owner (=> self &update-cursor last-owner)))
       )
  )

(defmethod (shared-physical-screen &get-owner-contents) (owner breakout-allowed)
  (if (eq (object-type owner) 'virtual-screen) % hack for efficiency
    (virtual-screen$send-contents owner breakout-allowed)
    (=> owner send-contents breakout-allowed)
    ))
  
(defmethod (shared-physical-screen &assert-ownership) (owner)
  (if (eq (object-type owner) 'virtual-screen) % hack for efficiency
    (virtual-screen$assert-ownership owner)
    (=> owner assert-ownership)
    ))
  
(defmethod (shared-physical-screen &update-cursor) (owner)
  (let ((pair (if (eq (object-type owner) 'virtual-screen)
		(virtual-screen$screen-cursor-position owner)
		(=> owner screen-cursor-position)
		)))
    (if (PairP pair)
      (=> screen set-cursor-position (car pair) (cdr pair)))))
  
(undeclare-flavor screen)
