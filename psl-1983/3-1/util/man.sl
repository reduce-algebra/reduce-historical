%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MAN -- an online PSL reference manual facility.
%%%        Principal features are easy access to the index and
%%%        a command to jump directly from a line in the index
%%%        to the place in the manual referred to.
%%% 
%%% Author: Cris Perdue
%%% Date: 12/1/82
%%%
%%% This package is still under development.
%%% An index browsing mode is contemplated, also use of a specialized
%%% representation of the reference manual.
%%% A concept index browser and a table of contents browser
%%% are contemplated as extensions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Edit by Cris Perdue,  8 Feb 1983 1145-PST
% Modified to use functions now defined in their own modules.

(compiletime (load fast-int if extended-char))

(imports '(nmode string-search string-input))

%%% Defines 2 new nondestructive commands for text mode,
%%% which seems to make them apply in LISP mode as well.
%%% One is M-!, which takes you to information about the
%%% subject of interest in the chapter and page referred to
%%% by the next index reference.
%%% The other is C-X I, which does a "Find File" on the file
%%% containing the function index to the PSL manual.

(add-to-command-list
 'read-only-text-command-list (x-char M-!!) 'index-browse-command)
(add-to-command-list
 'read-only-text-command-list (x-chars C-X i) 'get-index-buffer)
(nmode-establish-current-mode)

(fluid '(manual-chapters manual-file-template))

% 0-TITLEPAGE
% 00-PREFACE
% 000-CONTENTS

%%% A list of strings, each containing the base name of a chapter
%%% of the manual.  The first member of this list must be
%%% referred to as chapter 1 in index references, and similarly
%%% for other elements of the list.

(setq manual-chapters '(
"01-INTRODUCTION"
"02-GETSTART"
"03-RLISP"
"04-DATATYPES"
"05-NUMBERS"
"06-IDS"
"07-LISTS"
"08-STRINGS"
"09-FLOWOFCONTROL"
"10-FUNCTIONS"
"11-INTERP"
"12-GLOBALS"
"13-IO"
"14-TOPLOOP"
"15-ERRORS"
"16-DEBUG"
"17-EDITOR"
"18-UTILITIES"
"19-COMPLR"
"20-DEC20"
"21-SYSLISP"
"22-IMPLEMENTATION"
"23-PARSER"
"24-BIBLIO"
"25-FUN-INDEX"
"26-TOP-INDEX"
))

%%% This variable is a template for the name of a file that is
%%% part of the manual.  Actual manual file names are obtained by
%%% substituting a name from the name list into this template.

(setq manual-file-template "plpt:%w.lpt")

(defun get-index-buffer ()
  (find-file (bldmsg manual-file-template "25-FUN-INDEX")))

%%% This function gets the name that information is desired for,
%%% gets the chapter and page of the "next" index reference after
%%% point, does a "Find File" on the appropriate manual file,
%%% goes to the appropriate page, and searches for an occurrence
%%% of the key string.

(defun index-browse-command ()
  (let ((l (=> nmode-current-buffer current-line)))
    (let ((key (get-key l))
	  (dotpos (get-dot-pos l (=> nmode-current-buffer char-pos)))
	  digitpos endpos chapter page)

      %% The first "." coming after point and with a digit on either
      %% side is used as the "." of the index entry.
      %% Contiguous digits to either side of the "." are taken
      %% to be chapter and page of the reference.
      %% This allows the user to distinguish between different
      %% index references even on the same line.
      (if (or (null key) (null dotpos)) then (ding)
	  else
	  (setq digitpos
		%% Search for non-digit or beginning of line.
		%% Position of earliest digit is returned.
		(for (from i (- dotpos 2) 0 -1)
		     (do (if (not (digitp (indx l i))) then
			     (return (+ i 1))))
		     (finally (return 0))))
	  (setq chapter (string-read (substring l digitpos dotpos)))

	  %% Endpos is set to position of first non-digit after
	  %% the page number, or end of line position, if all digits
	  %% to end of line.
	  (setq endpos (search-in-string-fn 'not-digitp l (+ dotpos 1)))
	  (if (null endpos) then (setq endpos (+ (isizes l) 1)))

	  (setq page (string-read (substring l (+ dotpos 1) endpos)))

	  (find-file (bldmsg manual-file-template
			     (nth manual-chapters chapter)))
	  (move-to-buffer-start)
	  %% Skip over pages preceding the desired one.
	  (for (from i 1 (- page 1))
	       (do (forward-search "")
		   (move-over-characters 1)))
	  %% Search for an occurrence of the key string.
	  %% This part should perhaps be refined to only move to
	  %% a place within the page of interest.
	  %% Note that forward-search expects the key to be entirely
	  %% upper case and leaves point at the beginning of the string
	  %% if found.
	  (forward-search (string-upcase key))))))

%%% The key is taken to be a substring of the line string.
%%% The key starts at the first nonblank character and runs
%%% up to the first occurrence of either ". " or " .".  This
%%% is dependent on the precise format of index files produced
%%% by Scribe.
%%% This function is capable of returning NIL.

(defun get-key (line)
  (let ((p1 (string-search ". " line))
	(p2 (string-search " ." line)))
    (let ((end-pos (if (and p1 p2) then (min p1 p2)
		       elseif (and p1 (null p2)) then p1
		       elseif (and p2 (null p1)) then p2
		       else nil))
	  (key-pos (search-in-string-fn 'nonblank line 0)))
      (if (and key-pos end-pos) then
	  (substring line key-pos end-pos)
	  else nil))))

%%% Searches for a dot which must be at or after "start".
%%% The dot must be surrounded by a digit on either side.
%%% NIL is returned if none found.

(defun get-dot-pos (line start)
  (for (for dotpos
	    (string-search-from "." line start)
	    (string-search-from "." line (+ dotpos 1)))
       (while dotpos)
       (do (if (and (digitp (indx line (- dotpos 1)))
		    (digitp (indx line (+ dotpos 1)))) then
	       (return dotpos)))))

(defun not-digitp (c)
  (not (digitp c)))

(defun nonblank (c)
  (neq c #\SPACE))

%%% The position of the first character of the domain for which
%%% testfn returns true and whose index is at least "start" is
%%% returned.  If none such exists, NIL is returned.

(defun search-in-string-fn (testfn domain start)
  (if (not (stringp domain)) then
      (error 0 "Arg to search-in-string-fn not a string"))
  (for (from i start (isizes domain))
       (do (if (funcall testfn (igets domain i)) then
	       (return i)))
       (finally (return nil)))) 
