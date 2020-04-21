%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% VS-SUPPORT.SL - "Fast" routines to support the "virtual-screen" package.
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        6 August 1982
%
% This revised version takes advantage of TerminalClearEOL.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load fast-vector))

(de RewriteChangedCharacters (oldline newline RowLocation LeftCol RightCol)

  % A rather specialized routine to look for characters that differ between
  % oldline and newline, and to write those out to the screen.  The search is
  % limited to run from LeftCol to RightCol.  RowLocation is simply passed on
  % to WritePhysicalCharacter.

  (prog (last-nonblank-column)

    % Check to see whether a Clear-EOL is appropriate.  It is appropriate if
    % the rightmost changed character has been changed to a BLANK, and the
    % remainder of the line is blank.  If this is the case, we determine the
    % column to clear from, clear out the old line, and (after outputting prior
    % changed characters), do the Clear-EOL.

    % Find out where the rightmost changed character actually is:

    (while (and (WLEQ LeftCol RightCol)
	        (WEQ (igets newline RightCol) (igets oldline RightCol)))
      (setf RightCol (WDifference RightCol 1))
      )
    (if (WGreaterP LeftCol RightCol) (return NIL)) % No change at all!

    % If the rightmost changed character is being changed to a space, then find
    % out if the rest of the line is blank.  If it is, then set the variable
    % LAST-NONBLANK-COLUMN to the appropriate value and clear out OLDLINE in
    % preparation for a later ClearEOL.  Otherwise, LAST-NONBLANK-COLUMN
    % remains NIL.

    (if (WEQ (igets newline RightCol) (char space))
      (progn
        (setf last-nonblank-column (size newline))
        (while (and (WGEQ last-nonblank-column 0)
		    (WEQ (igets newline last-nonblank-column) (char space))
		    )
          (setf last-nonblank-column (WDifference last-nonblank-column 1))
          )
        (if (WLessP last-nonblank-column RightCol)
	    (while (> RightCol last-nonblank-column)
	      (iputs oldline RightCol (char space))
	      (setf RightCol (WDifference RightCol 1))
	      )
	    )))

    % Output all changed characters (other than those that will be taken care
    % of by ClearEOL):

    (while (WLEQ LeftCol RightCol)
      (let ((ch (igets newline LeftCol)))
        (if (WNEQ ch (igets oldline LeftCol))
	  (WritePhysicalCharacter ch RowLocation LeftCol)
	  ))
      (setf LeftCol (wplus2 LeftCol 1))
      )

    % Do the ClearEOL, if that's what we decided to do.

    (if last-nonblank-column
      (progn
	(MoveToPhysicalLocation RowLocation (WPlus2 last-nonblank-column 1))
	(TerminalClearEOL)
	))
  ))
