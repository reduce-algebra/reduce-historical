%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Input from strings
%%% Cris Perdue
%%% 12/1/82
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load if fast-int))

(fluid '(channel-string channel-string-pos))

%%% Takes two arguments: a string and a function.
%%% The function must take 1 argument.  With-input-from-string
%%% will call the function and pass it a channel number.  If the
%%% function takes input from the channel (which is the point of
%%% all this), it will receive successive characters from the
%%% string as its input.
%%%
%%% This is not currently unwind-protected.

(defun with-input-from-string (str fn)
  (let ((specialreadfunction* 'string-readchar)
	(specialwritefunction* 'readonlychannel)
	(specialclosefunction* 'null)
	(channel-string str) (channel-string-pos 0))
    (let ((chan (open "" 'special))
	  value)
	(setq value (apply fn (list chan)))
	(close chan)
	value)))

%%% This is similar to with-input-from-string, but the string
%%% passed in is effectively padded on the right with a single
%%% blank.  No storage allocation is performed to give this
%%% effect.

(defun with-input-from-terminated-string (str fn)
  (let ((specialreadfunction* 'string-readchar-terminated)
	(specialwritefunction* 'readonlychannel)
	(specialclosefunction* 'null)
	(channel-string str)
	(channel-string-pos 0))
    (let ((chan (open "" 'special))
	  value)
      (setq value (apply fn (list chan)))
      (close chan)
      value)))

%%% Reads from the string.  The string is effectively padded with
%%% a blank at the end so if the expression in the string is for
%%% example a single token, it need not be followed by a terminator.

(defun string-read (str)
  (with-input-from-terminated-string str 'channelread))

%%% Reads a single token from the string using channelreadtoken.
%%% The string need contain no terminator character; a blank is
%%% provided if necessary by string-readtoken.

(defun string-readtoken (str)
  (with-input-from-terminated-string str 'channelreadtoken))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal routines.

(defun string-readchar (chan)
  (if (> channel-string-pos (size channel-string)) then
      $eof$
      else
      (prog1
       (indx channel-string channel-string-pos)
       (setq channel-string-pos (+ channel-string-pos 1)))))

%%% Includes hack that tacks on a blank for termination of READ
%%% and friends.

(defun string-readchar-terminated (chan)
  (if (<= channel-string-pos (size channel-string)) then
      (prog1
       (indx channel-string channel-string-pos)
       (setq channel-string-pos (+ channel-string-pos 1)))
      elseif (= channel-string-pos (+ 1 (size channel-string))) then
      (prog1
       32			% Blank
       (setq channel-string-pos (+ channel-string-pos 1)))
      else
      $eof$))

