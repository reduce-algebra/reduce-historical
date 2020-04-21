%
% PATHIN.SL - Rlisp IN function with a search path
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        26 July 1982
% Copyright (c) 1982 University of Utah
%

% PATHIN(filename-tail:string):none			EXPR
%
% PATHIN allows the use of a directory search path with the Rlisp IN function.
% The fluid variable PATHIN* should be a list of strings, which are directory
% names.  These will be successively concatenated onto the front of the
% string argument to PATHIN until an existing file is found.  If one is found,
% IN will be invoked on the file.  If not, a continuable error occurs.
% E.g, if PATHIN* is ("" "/usr/src/cmd/psl/" "/u/smith/"), (pathin "foo.red")
% will attempt to open "foo.red", then "/usr/src/cmd/psl/foo.red", and finally
% "/u/smith/foo.red".

(bothtimes (fluid '(pathin*)))

(compiletime (flag '(pathin-aux) 'internalfunction))

(loadtime (flag '(pathin) 'ignore)) % just like IN, gets done while compiling

(loadtime (if (null pathin*) (setq pathin* '(""))))
	% acts like IN until path is changed

(de pathin (filename-tail)
  (pathin-aux filename-tail pathin*))

(de pathin-aux (filename-tail search-path-list)
  (if (null search-path-list)
      (conterror 99 "File not found in path" (pathin filename-tail))
      (let ((test-file (concat (first search-path-list) filename-tail)))
	   (if (filep test-file)
	       (evin (list test-file))
	       (pathin-aux filename-tail (rest search-path-list))))))
