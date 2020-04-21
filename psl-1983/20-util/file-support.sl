%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% File-Support.SL - System-Dependent Support for File Primitives (TOPS-20)
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        16 September 1982
%
% This file contains support functions used in the implementation of file
% primitives for TOPS-20.  The existence of the functions in this file should
% be ignored when writing system-independent code.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load jsys common pathnames))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% JFN Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de jfn-truename (jfn)
  (let ((file-name (make-string 200 #\space)))
    (jsys1 file-name jfn #.(bits 2 5 8 11 14 35) 0 (const jsJFNS))
    (recopystringtonull file-name)
    ))

(de jfn-deleted? (jfn)
  (if (integerp jfn)
    (not (= (LAnd (Jsys4 jfn #.(xword 1 1) 4 0 (const jsGTFDB))
		  (bits 3)) 0))))

(de jfn-write-date (jfn)
  (if (integerp jfn)
    (Jsys4 jfn #.(xword 1 8#14) 4 0 (const jsGTFDB))))

(de jfn-read-date (jfn)
  (if (integerp jfn)
    (Jsys4 jfn #.(xword 1 8#15) 4 0 (const jsGTFDB))))

(de jfn-byte-count (jfn)
  (if (integerp jfn)
    (Jsys4 jfn #.(xword 1 8#12) 4 0 (const jsGTFDB))))

(de jfn-page-count (jfn)
  (if (integerp jfn)
    (lowhalfword (Jsys4 jfn #.(xword 1 8#11) 4 0 (const jsGTFDB)))))

(de jfn-original-author (jfn)
  (if (integerp jfn)
    (let ((str (make-string 100 0)))
      (Jsys0 (xword 0 jfn) str 0 0 (const jsGFUST))
      (recopystringtonull str)
      )))

(de jfn-author (jfn)
  (if (integerp jfn)
    (let ((str (make-string 100 0)))
      (Jsys0 (xword 1 jfn) str 0 0 (const jsGFUST))
      (recopystringtonull str)
      )))

(de jfn-delete (jfn)
  (if (integerp jfn)
      (jsys0 jfn 0 0 0 (const jsDELF))
      ))

(de jfn-delete-and-expunge (jfn)
  (if (integerp jfn)
      (jsys0 (xword 2#010000000000000000 jfn) 0 0 0 (const jsDELF))
      ))

(de jfn-undelete (jfn)
  (if (integerp jfn)
      (jsys0 (xword 1 jfn) #.(bits 3) 0 0 (const jsCHFDB))
      ))

(de jfn-release (jfn)
  (if (integerp jfn)
      (jsys0 jfn 0 0 0 (const jsRLJFN))
      ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% GTJFN Functions:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de attempt-to-get-jfn (file-name the-bits)
  (setf file-name (namestring file-name))
  (let ((jfn (ErrorSet
	      (list 'jsys1 the-bits file-name 0 0 (const jsGTJFN)) nil nil)
	))
      (cond
	((listp jfn) (car jfn))
	)))
