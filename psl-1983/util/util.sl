%
% UTIL.SL - General Utility/Support functions
% 
% Author:      Nancy Kendzierski
%              Hewlett-Packard/CRC
% Date:        23 September 1982
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load common strings objects))

(fluid '(nmode-terminal))

(defun integer$parse (str)
  % Return an integer corresponding to the string -- not the characters
  %  in the string, but the number in the string.
  (prog (i negative error ch num)
    (setf i 0)
    (setf num 0)
    (if (<= (string-length str) 0) (return NIL))
    (setf ch (indx str 0))
    (cond ((= ch (char -)) (let () (setf negative t)
				   (setf i (add1 i))))
	  ((= ch (char +)) (setf i (add1 i))))
    (if (>= i (string-length str)) (return NIL))
    (for (from i i (size str)) (do 
      (setq ch (indx str i))
      (cond ((or (< ch (char 0)) (> ch (char 9)))
	     (exit (setq error t)))
	    (t (setq num (+ (* num 10) (- ch (char 0))))))))
    (cond (error (return NIL))
	  (negative (return (setq num (minus num))))
	  (t (return num)))))

(defun integer$unparse (num)
  % Return an ASCII string version of the integer.
  (let ((str "") (negative nil) temp)
    (cond ((< num 0) (setf negative t) (setf num (minus num))))
    (while (> num 0)
      (setq temp (divide num 10))
      (setq num (car temp))
      (setq str (string-concat (string (+ (cdr temp) (char 0))) str)))
    (cond ((equal str "") "0")
	  (negative (string-concat "-" str))
	  (t str))
    ))

(defun integer-base$parse (base str)
  % Return an integer corresponding to the string -- not the characters
  %  in the string, but the number in the string.
  (prog (i negative error ch num max-digit)
    (setf max-digit (+ #\0 (- base 1)))
    (setf i 0)
    (setf num 0)
    (if (<= (string-length str) 0) (return NIL))
    (setf ch (indx str 0))
    (cond ((= ch (char -)) (let () (setf negative t)
				(setf i (add1 i))))
	  ((= ch (char +)) (setf i (add1 i))))
    (if (>= i (string-length str)) (return NIL))
    (for (from i i (size str)) (do 
      (setq ch (indx str i))
      (cond ((or (< ch (char 0)) (> ch max-digit))
	     (exit (setq error t)))
	    (t (setq num (+ (* num base) (- ch (char 0))))))))
    (cond (error (return NIL))
	  (negative (return (setq num (minus num))))
	  (t (return num)))))

(defun integer-base$unparse (base num)
  % Return an ASCII string version of the integer.
  (let ((str "") (negative nil) temp)
    (cond ((< num 0) (setf negative t) (setf num (minus num))))
    (while (> num 0)
      (setq temp (divide num base))
      (setq num (car temp))
      (setq str (string-concat (string (+ (cdr temp) (char 0))) str)))
    (cond ((equal str "") "0")
	  (negative (string-concat "-" str))
	  (t str))
    ))

(defun LoadSoftKey (key mode command label)
  % Load a soft key on an HP264X terminal
  %   key:      0 <= key <= 8
  %   mode:     'N 'L or 'T
  %   command:  string (maximum 80 characters)
  %   label:    string (maximum 80 characters)
  (prog (cmd command-size label-size restore-echo?)
    (setq cmd (string 27 38))  % Escape-& is soft-key command prefix start.
    %  Set up proper mode.
    (cond ((= mode 'N) (setq cmd (concat cmd "f0a")))
	  ((= mode 'L) (setq cmd (concat cmd "f1a")))
	  ((= mode 'T) (setq cmd (concat cmd "f2a")))
	  (t (return "Illegal mode") ))
    %  Set up soft-key number.
    (if (or (< key 0) (> key 8)) (return "Illegal soft-key number"))
    (setq cmd (string-concat cmd (integer$unparse key) "k"))
    %  Set up label length, command length, and command.
    (setq label-size (+ 1 (size label)))
    (if (> label-size 80) (return "Label too long"))
    (setq command-size (+ 1 (size command)))
    (if (> command-size 80) (return "Command too long"))

    (setq cmd (string-concat cmd
			     (integer$unparse label-size)
			     "d"
			     (integer$unparse command-size)
                             "L"
			     label
			     command))
    %  Turn echoing off, if necessary.
    (cond ((not (=> nmode-terminal raw-mode))
	   (=> nmode-terminal enter-raw-mode)
	   (setq restore-echo? t)))
    %  Output the string of command characters.
    (for (from i 0 (size cmd)) (do (pbout (indx cmd i))))
    (if restore-echo? (=> nmode-terminal leave-raw-mode))
    ))
