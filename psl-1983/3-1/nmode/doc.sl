%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Doc.SL - NMODE On-line Documentation
% 
% Author:      Jeffrey Soreff
%              Hewlett-Packard/CRC
% Date:        15 February 1983
% Revised:     8 April 1983
%
% 8-April-83 Jeff Soreff
%   Altered doc-filter-predicate and apply-filter method to adhere to the
%   "return list of self" convention (see code for apply filter method).
%   Declare-flavor was used to preserve efficiency of doc-filter-predicate.
% 31-Mar-83 Jeff Soreff
%   Altered set-up-documentation to remove interaction with FRL.
%   A use of channelread was replaced with nmode-read-and-evaluate-file.
% 14-Mar-83 Alan Snyder
%   Convert for changes in browser mechanism.  Clear modified flag of
%   documentation buffer.  Fixup external declarations and load statement.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects extended-char fast-strings numeric-operators))
(on fast-integers)

% External variables:

(fluid '(text-mode))

% Internal static variables:

(fluid '(view-mode
	 doc-obj-list
	 doc-browser-mode
	 doc-browser-command-list
	 doc-browser-documentation-text
	 doc-browser-help-text
	 doc-filter-argument-list
	 doc-text-file
	 reference-text-file
	 doc-text-buffer))

(setf doc-obj-list nil)
(setf doc-text-file "SS:<PSL.NMODE-DOC>FRAMES.LPT")
(setf reference-text-file "SS:<PSL.NMODE-DOC>COSTLY.SL")

(setf doc-browser-help-text
  ["? View Edit Filter uNdo-filter Ignore Quit"])

(setf doc-browser-documentation-text
  ["The Documentation Browser displays documentation on NMODE."
   "Terminology: the current item is the item pointed at by the cursor."
   "The View (V) and Edit (E) commands both display the current item."
   "In split-screen mode, Edit selects the bottom window while View does not."
   "The Filter (F) command asks for a string and removes all items that"
   "do not match the string."
   "The Ignore (I) command removes the current item from the display."
   "The uNdo-Filter (N) command restores the items removed by the most"
   "recent Filter command or by the most recent series of Ignore commands."
   "The Quit (Q) command exits the browser."
   ])

(de set-up-documentation ()
  (when (null doc-obj-list)
    (setf doc-text-buffer (create-unnamed-buffer text-mode))
    (insert-file-into-buffer doc-text-buffer doc-text-file)
    (=> doc-text-buffer set-modified? NIL)
    (nmode-read-and-evaluate-file reference-text-file)
    (let ((browser (create-nmode-documentation-browser)))
      (=> browser set-items doc-obj-list)
      )
    NIL
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Documentation Browser Commands
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(setf view-mode
    (nmode-define-mode
     "View"
     '((nmode-define-commands Read-Only-Text-Command-List)
       (nmode-define-commands Read-Only-Terminal-Command-List)
       (nmode-define-commands Window-Command-List)
       (nmode-define-commands Essential-Command-List)
       (nmode-define-commands Basic-Command-List)
       (nmode-define-commands
	(list (cons (x-char Q) 'select-previous-buffer-command)))
       )))

(setf Doc-Browser-Mode (nmode-define-mode "Doc-Browser" '(
  (nmode-define-commands Doc-Browser-Command-List)
  (nmode-establish-mode Read-Only-Text-Mode)
  )))

(setf Doc-Browser-Command-List
  (list
   (cons (x-char ?) 'browser-help-command)
   (cons (x-char F) 'doc-filter-command)
   (cons (x-char E) 'browser-edit-command)
   (cons (x-char I) 'browser-ignore-command)
   (cons (x-char N) 'browser-undo-filter-command)
   (cons (x-char V) 'browser-view-command)
   (cons (x-char Q) 'browser-exit-command)
   (cons (x-char SPACE) 'move-down-command)
   ))

(de doc-obj-compare (obj1 obj2)
  (let ((indx1 (doc-browse-obj$index obj1))
	(indx2 (doc-browse-obj$index obj2)))
    (< indx1 indx2)))

(de doc-filter-command ()
  (let ((browser (current-browser))
	(doc-filter-argument-list 
	 (list (prompt-for-string 
		"Search for what string in a command's name or references?"
		""))))
    (=> browser filter-items #'doc-filter-predicate)
    ))

(declare-flavor doc-browse-obj doc-obj obj-temp)

(de doc-filter-predicate (doc-obj)
  (let* ((old-name (=> doc-obj name))
	 (ref-list (=> doc-obj ref-list))
	 (pattern (string-upcase (first doc-filter-argument-list)))
	 (pattern-length (string-length pattern))
	 (name-list (cons old-name 
			  (for (in ref ref-list)
			       (with name-list obj-temp)
			       (collect
				(let ((obj-temp (eval ref)))
				  (=> obj-temp name))
				name-list)
			       (returns name-list)))))
    (for (in name name-list)
	 (with found)
	 (do (when (let ((limit (- (string-length name) pattern-length))
			 (char-pos 0))
		     (while (<= char-pos limit)
		       (if (pattern-matches-in-line pattern name char-pos)
			 (exit char-pos))
		       (incr char-pos)))
	       (setf found t)))
	 (returns found))))

(undeclare-flavor doc-obj obj-temp)

(de create-nmode-documentation-browser ()
  (create-browser 'DOCUMENTATION-BROWSER "Documentation" "NMODE"
		  doc-browser-mode (create-unnamed-buffer view-mode)
		  ["NMODE Documentation Browser Subsystem" ""]
		  doc-browser-documentation-text
		  doc-browser-help-text
		  () #'doc-obj-compare)
  )

(de apropos-command ()
  (let* ((doc-filter-argument-list 
	  (list (prompt-for-string 
		 "Search for what string in a command's name or references?"
		 "")))
	 (jnk (set-up-documentation))
	 (browser (or (find-browser 'DOCUMENTATION-BROWSER "NMODE")
		      (create-nmode-documentation-browser)
		      )))
    (=> browser set-items doc-obj-list)
    (=> browser filter-items #'doc-filter-predicate)
    (browser-enter browser)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% The doc-browse-obj (documentation-browser-object) flavor:
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(defflavor doc-browse-obj
  (
   name
   type
   index
   (start-line NIL)
   (end-line NIL)
   (ref-list ())
   )
  ()
  initable-instance-variables
  gettable-instance-variables
  )

(defmethod (doc-browse-obj display-text) ()
  (string-concat (id2string type) ": " name))

(defmethod (doc-browse-obj view-buffer) (buffer)
  (unless buffer 
    (setf buffer (create-unnamed-buffer view-mode)))
  (=> buffer reset)
  (if (not (and start-line end-line))
    (=> buffer insert-string
	"Sorry, no documentation is availible on this topic.")
    (=> buffer insert-text
	(cdr (=> doc-text-buffer extract-region 
		 NIL (cons start-line 0) (cons end-line 0)))))
  (=> buffer move-to-buffer-start)
  (=> buffer set-modified? nil)
  buffer)

(defmethod (doc-browse-obj update) ()
  T
  )

(defmethod (doc-browse-obj cleanup) ()
  NIL)

(defmethod (doc-browse-obj apply-filter) (filter)
  (apply filter (list self)))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(off fast-integers)
