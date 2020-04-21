%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Doc.SL - NMODE On-line Documentation
% 
% Author:      Jeffrey Soreff
%              Hewlett-Packard/CRC
% Date:        15 February 1983
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load objects
		 extended-char
		 fast-vectors
		 fast-strings
		 fast-int
		 stringx))

% External variables:

(fluid '(
	 nmode-current-buffer
	 nmode-current-window
	 doc-obj-list
	 ))

(setf doc-obj-list nil)

% Internal static variables:

(fluid '(view-mode
	 doc-browser-mode
	 doc-browser-command-list
	 doc-filter-argument-list
	 doc-text-file
	 reference-text-file
	 doc-text-buffer))

(setf doc-text-file "SS:<PSL.NMODE-DOC>FRAMES.LPT")
(setf reference-text-file "SS:<PSL.NMODE-DOC>COSTLY.SL")

(de set-up-documentation ()
  (setf doc-text-buffer (buffer-create-default "+DOCTEXT"))
  (insert-file-into-buffer doc-text-buffer doc-text-file)
  (let ((ref-chan (open reference-text-file 'input)))
    (eval (channelread ref-chan))
    (close ref-chan)))

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
   (cons (x-char ?) 'doc-browser-help)
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

(de doc-browser-help ()
  (write-message "Quit Edit Filter uNdo-filter Ignore View"))

(de doc-filter-command ()
  (let ((browser (=> nmode-current-buffer get 'browser))
	(doc-filter-argument-list 
	 (list (prompt-for-string 
		"Search for what string in a command's name or references?"
		""))))
    (=> browser filter-items #'doc-filter-predicate)))

(de doc-filter-predicate (old-name ref-list)
  (let* ((pattern (string-upcase (first doc-filter-argument-list)))
	 (pattern-length (string-length pattern))
	 (name-list (cons old-name 
			  (for (in ref ref-list)
			       (with name-list)
			       (collect (=> (eval ref) name) name-list)
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

(de apropos-command ()
  (let* ((doc-filter-argument-list 
	  (list (prompt-for-string 
		 "Search for what string in a command's name or references?"
		 "")))
	 (blist (buffer-create "+DOCLIST" doc-browser-mode))
	 (bitem (buffer-create "+DOCITEM" view-mode))
	 (jnk   (if (null doc-obj-list) (set-up-documentation)))
	 (browser
	  (create-browser blist bitem 
			  ["Documentation Browser Subsystem"
			   ""] doc-obj-list #'doc-obj-compare)))
    (=> browser select-item (car doc-obj-list))
    (=> browser filter-items #'doc-filter-predicate)
    (browser-enter blist)
    (doc-browser-help)))

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
    (setf buffer (buffer-create-default "+DOCITEM")))
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

(defmethod (doc-browse-obj cleanup) ()
  NIL)

(defmethod (doc-browse-obj apply-filter) (filter)
  (apply filter (list name ref-list)))
