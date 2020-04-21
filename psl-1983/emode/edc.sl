% A simple desk calculator to run under EMODE.  In this mode all the
% numbers in the buffer are summed up, any other characters are inserted
% and ignored, the total is given as the last line of the OUT_WINDOW buffer..
(load useful)    % Need useful so that FOR loops work!

% Insert a character, and then sum up all the lines in the buffer.
(DE InsertAndTotal ()
  (progn
    (InsertSelfCharacter)
    (FindBufferTotal)))

(DE DeleteBackwardAndTotal ()
  (progn
    (!$DeleteBackwardCharacter)
    (FindBufferTotal)))

(DE DeleteForwardAndTotal ()
  (progn
    (!$DeleteForwardCharacter)
    (FindBufferTotal)))

(DE kill_line_and_total ()
  (progn
    (kill_line)
    (FindBufferTotal)))

(DE insert_kill_buffer_and_total ()
  (progn
    (insert_kill_buffer)
    (FindBufferTotal)))

(DE FindBufferTotal ()
  (prog (total save-point save-line-index itm)
    % Remember our spot in the buffer.
    (setf save-point point)
    (setf save-line-index CurrentLineIndex)

    (setf total 0)
    % Move to the start of the buffer.
    (!$BeginningOfBuffer)
    % Read from, and write to, EMODE buffers.
    (SelectEmodeChannels)

    % Find the total.
    (while (not (EndOfBufferP (NextIndex CurrentLineIndex)))
      (progn
        % NOTE that READ would loose badly here--since it calls
        % MakeInputAvailable here, and thus call EMODE recursively.
        (setf itm (ChannelRead IN*))
        (cond
          ((NumberP itm)
            (setf total (plus total itm))))))


    % Now, show the total in the OUT_WINDOW buffer.
    (prog (old-point old-line-index old-buffer)
      (setf old-buffer CurrentBufferName)
      (SelectBuffer 'OUT_WINDOW)
      (!$EndOfBuffer)      % Move to end of the buffer.
      (setf old-point point)
      (setf old-line-index CurrentLineIndex)
      % Move to beginning of previous line.
      (!$BackwardLine)
      (!$BeginningOfLine)
      % Delete the old text
      (delete_or_copy T CurrentLineIndex point old-line-index old-point)
      % Print the total (to the output buffer)
      (PRINT total)
      (SelectBuffer old-buffer))

    % Finally, restore the original point and mark.
    (SelectLine save-line-index)
    (setf point save-point)))

% Establish keyboard bindings for Desk Calculator mode.
(DE SetDCmode ()
  (progn
    % Make most characters insert and then find total.
    (for (from i 32 126 1)
      (do
        (SetKey i 'InsertAndTotal)))

    (SetKey (char TAB) 'InsertAndTotal)

    % Inherit the rest of the bindings from "text mode"
    (for (in itm TextDispatchList)
      (do
        (SetKey (car itm) (cdr itm))))

    % Then, rebind (some of?) the folks who actually modify stuff.
    (SetKey (char (cntrl D)) 'DeleteForwardAndTotal)
    (SetKey (char (cntrl K)) 'kill_line_and_total)
    (SetKey (char DELETE) 'DeleteBackwardAndTotal)
    (SetKey (char (cntrl Y)) 'insert_kill_buffer_and_total)))

(setf DCMode '(RlispInterfaceDispatch SetDCmode BasicDispatchSetup))

% This code must be run AFTER starting up EMODE.
(prog (old-buffer)
  (setf old-buffer CurrentBufferName)
  (CreateBuffer 'DC DCMode)
  (SelectBuffer 'DC)
  (!$CRLF)
  (insert_string "0")
  (!$CRLF)
  (!$BeginningOfBuffer)
  (SelectBuffer old-buffer))
