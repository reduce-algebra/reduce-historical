%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% NMODE-20.SL - DEC-20 NMODE Stuff (intended for DEC-20 Version Only)
%
% Author:	Jeffrey Soreff
%		Hewlett-Packard/CRC
% Date:		24 January 1983
% Revised:      25 January 1983
%
% 25-Jan-83 Alan Snyder
%  Add version of actualize-file-name that ensures that transiently-created
%  file has delete access.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de current-date-time () % Stolen directly from Nancy Kendzierski
  % Date/time in appropriate format for the network mail header
  (let ((date-time (MkString 80)))
    (jsys1 date-time -1 #.(bits 5 7 10 12 13) 0 (const jsODTIM))
    (recopystringtonull date-time)))

(de actualize-file-name (file-name)
  % If the specified file exists, return its "true" (and complete) name.
  % Otherwise, return the "true" name of the file that would be created if one
  % were to do so.  (Unfortunately, we have no way to do this except by actually
  % creating the file and then deleting it!)  Return NIL if the file cannot be
  % read or created.

  (let ((s (attempt-to-open-input file-name)))
    (cond ((not s)
	   (setf s (attempt-to-open-output
		    (string-concat file-name ";P777777") % so we can delete it!
		    ))
	   (when s
	     (setf file-name (=> s file-name))
	     (=> s close)
	     (file-delete-and-expunge file-name)
	     file-name
	     )
	   )
	  (t
	   (setf file-name (=> s file-name))
	   (=> s close)
	   file-name
	   ))))

