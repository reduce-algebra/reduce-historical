%
% CUSTOMIZE-RLISP-FOR-EMODE.SL - "customizations" to support EMODE.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        14 July 1982
% Copyright (c) 1982 University of Utah
%

% This file makes a few changes to the "innards" of RLISP to customize it
% for the building of EMODE.  Also adds a few utilities that should
% (perhaps) become part of the standard PSL.

% Set things up so SETF knows about IGETV and IGETS.  ("Fast" string and
% vector accessors.)
(BothTimes       % BothTimes?
  (progn
    (put 'IGETV 'ASSIGN-OP 'IPUTV)
    (put 'IGETS 'ASSIGN-OP 'IPUTS)))

% Return true  is x is a "list".  (I.e., a pair or NIL.)
(de listp (x)
  (or (null x) (pairp x)))

% Return lst with its first n entries dropped.
(de tail (lst n)
  (cond
    ((null lst) NIL)
    ((eqn n 0) lst)
    (T (tail (cdr lst) (sub1 n)))))

% Routines for reading from and printing into strings.
(fluid
  '(
    string_for_read_from_string
    index_for_string
    string_input_channel
    string_output_channel
    print_dest_string
    print_indx
    flush_output))

% Set up the channels at load time.
(LoadTime
  (progn
    (setf SpecialWriteFunction* 'ReadOnlyChannel)
    (setf SpecialReadFunction* 'channel_read_from_string)
    (setf SpecialCloseFunction* 'DummyClose)
    (setf string_input_channel (open "string_reader" 'SPECIAL))

    (setf SpecialWriteFunction* 'channel_write_into_string)
    (setf SpecialReadFunction* 'WriteOnlyChannel)
    (setf string_output_channel (open "string_writer" 'SPECIAL))))

% READ from a string.  Argument is a fluid.
(de read_from_string (string_for_read_from_string)
  (prog (index_for_string  value)
    (setf index_for_string 0)    % index_for_string is also fluid.

    % Kludge to flush out input channel.
    (ChannelUnReadChar string_input_channel 0)
    % Read the value from the "magic" string reading channel.
    % Use ErrorSet to catch problems (such as trying to read an unbalanced
    % expression).  Rebind fluid !*BREAK to prevent a break loop if the
    % read fails.
    (let ((*BREAK NIL))
      (setf value
        (ErrorSet
          `(channelRead ,string_input_channel)
          T      % Allow error messages to be printed
          NIL))) % but, don't print backtrace stuff.

    (return
      (cond
        ((pairp value) (car value))
        % If there was an error in reading the string, just return NIL???
        % Or, pass the error on down?
        (T NIL)))))

% Ignore the channel argument, read next character from string in fluid
% "string_for_read_from_string", if any.  Return an end of file if none
% left.
(de channel_read_from_string (chn)
  (prog (val)
    (cond
      % If past end of string, return an EOF.
      ((GreaterP index_for_string (size string_for_read_from_string))
        (return (char EOF))))

    % Otherwise, return the appropriate character from the string.
    (setf val (indx string_for_read_from_string  index_for_string))
    (setf index_for_string (add1 index_for_string))

    (return val)))

% PrintF into the string "print_dest_string", starting at index
% "print_indx".  (Both of which are FLUIDS.)  Return the "printed into"
% string.  This code should probably be made more efficient (SysLispified?)
% someday.  Also, the number of legal arguments is sort of flakey.  Roughly
% modeled after the code for BldMsg.
(de PrintF_into_string
  (print_dest_string   print_indx  format
    arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)

  (prog old_outchan
    % Switch to special channel for printing into strings.
    (setf old_outchan OUT*)
    (setf OUT* string_output_channel)

    % Kludge to clear the line position counter
    (setf flush_output T)
    (WriteChar (char EOL))

    (setf flush_output NIL)
    % Now use PrintF to the appropriate "magic" channel.
    (PrintF format arg1 arg2 arg3 arg4 arg5 arg6 arg7 arg8 arg9 arg10)

    % Select original channel
    (setf OUT* old_outchan)

    % Return the printed into string.
    (return print_dest_string)))

(de channel_write_into_string (chn chr)
% Ignore the channel argument, write character into fluid
% "print_dest_string", at location print_indx.
% We're careful to check bounds, since bad things could happen if we try to
% print an error message during this process!
  (cond
    % If "flush" flag is clear, and everything is within bounds. 
    ((and
       (null flush_output)
       (leq 0 print_indx)
       (leq print_indx (size print_dest_string)))
      % then print into the string
      (progn
        (setf (indx print_dest_string print_indx) chr)
        (setf print_indx (add1 print_indx))))))

% Dummy routine to close up channel I/O.
(de DummyClose (chn)
  NIL)
