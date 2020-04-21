%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Browser-Browser.SL - Browser Browser Subsystem
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        14 March 1983
% Revised:     12 April 1983
%
% This file implements the browser browser subsystem.
%
% 12-April-83 Jeff Soreff
%  Bug fix: R and S commented out of the command list, pending sort
%  implementations.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(BothTimes (load extended-char fast-strings))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% External variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(read-only-text-mode))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Internal static variables:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(fluid '(browser-browser-mode
	 browser-browser-command-list
	 browser-browser-documentation-text
	 browser-browser-help-text
	 nmode-browser-prototypes
	 ))

(setf browser-browser-help-text
  ["? View-documentation Browse Kill uN/Ignore Quit"])

(setf browser-browser-documentation-text
  ["The Browser Browser displays all existing browsers, as well as"
   "prototypes for browsers that can be created.  The Browse (B) command"
   "given when the cursor points at an existing browser will select"
   "that browser.  The Browse (B) command given when the cursor points"
   "at a prototype browser will cause a new browser of that kind to be"
   "created, possibly after requesting additional information."
   "The View-Documentation (V) command will display information about"
   "the browser or prototype browser pointed at by the cursor."
   "The Kill (K) command will kill the browser pointed at by the cursor."
   "The Ignore (I) command will remove the pointed-at browser from the display."
   "The uNignore (N) command will restore all Ignored browsers to the display."
   "The Quit (Q) command will exit the browser browser."
   ])

(setf browser-browser-mode (nmode-define-mode "Browser-Browser" '(
  (nmode-define-commands browser-browser-command-list)
  (nmode-establish-mode Read-Only-Text-Mode)
  )))

(setf browser-browser-command-list
  (list
   (cons (x-char ?) 'browser-help-command)
   (cons (x-char B) 'browser-browser-browse-command)
   (cons (x-char I) 'browser-ignore-command)
   (cons (x-char K) 'browser-kill-command)
   (cons (x-char N) 'browser-undo-filter-command)
   (cons (x-char Q) 'browser-exit-command)
%   (cons (x-char R) 'browser-browser-reverse-sort) % not implemented!
%   (cons (x-char S) 'browser-browser-sort) % not implemented!
   (cons (x-char V) 'browser-view-command)
   (cons (x-char SPACE) 'move-down-command)
   ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de browser-browser-command ()
  % Bring up the browser browser subsystem.
  (let ((browser (or (find-browser 'BROWSER-BROWSER "")
		     (create-browser-browser)
		     )))
    (browser-enter browser)
    ))

(de create-browser-browser ()
  % Create the browser browser subsystem.
  % The set of items is created when the browser buffer is selected.

  (let* ((b (create-unnamed-buffer browser-browser-mode))
	 (header-text (vector "NMODE Browsers" ""))
	 )
    (let ((browser (create-browser
		    'BROWSER-BROWSER "Browsers" "" browser-browser-mode
		    NIL header-text browser-browser-documentation-text
		    browser-browser-help-text
		    () #'browser-browser-name-sorter)
		   ))
      (=> browser set-select-function 'browser-update)
      (=> browser set-update-function 'browser-browser-update)
      (=> browser put 'browser-list ())
      browser
      )))

(de browser-browser-update (browser)
  % Add any new browsers to the browser browser.

  (let* ((old-browser-list (=> browser get 'browser-list))
	 (new-browser-list (delq browser (all-browsers)))
	 (old-prototype-list (=> browser get 'prototype-list))
	 (new-prototype-list nmode-browser-prototypes)
	 (old-current-item (=> browser current-item))
	 (new-items
	  (append
	   (for (in br new-browser-list)
	       (when (not (memq br old-browser-list)))
	       (collect (create-browser-browser-item br))
	       )
	   (when (not (eq old-prototype-list new-prototype-list))
	     (for (in pr new-prototype-list)
		  (when (not (memq pr old-prototype-list)))
		  (collect pr)
		  ))
	   )))
    (=> browser add-items new-items)
    (=> browser put 'browser-list new-browser-list)
    (=> browser put 'prototype-list new-prototype-list)
    (=> browser select-item old-current-item)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Special Browser Browser commands:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de browser-browser-browse-command ()
  (let ((item (browser-current-item)))
    (cond ((not item) (Ding))
	  ((eq (object-type item) 'BROWSER-BROWSER-ITEM)
	   (browser-enter (=> item browser))
	   )
	  (t (=> item instantiate))
	  )))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Sorting Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de browser-browser-name-sorter (b1 b2)
  (let* ((text1 (=> b1 display-text))
	 (text2 (=> b2 display-text))
	 )
    (StringSortFN text1 text2)
    ))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The browser-browser-item flavor:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de create-browser-browser-item (b)
  (make-instance 'browser-browser-item
		 'browser b
		 ))

(defflavor browser-browser-item
  (display-text
   browser
   )
  ()
  (gettable-instance-variables)
  (initable-instance-variables)
  )

(defmethod (browser-browser-item init) (init-plist)
  (=> self &update-display-text)
  )

(defmethod (browser-browser-item &update-display-text) ()
  (let* ((kind-string (=> browser browser-kind-string))
	 (info-string (=> browser browser-info-string))
	 )
    (setf display-text (string-concat " " kind-string))
    (when (and info-string (not (string-empty? info-string)))
      (setf display-text (string-concat display-text " (" info-string ")")))
    ))

(defmethod (browser-browser-item update) ()
  (when (browser-is-active? browser)
    (=> self &update-display-text)
    T
    ))

(defmethod (browser-browser-item kill) ()
  (kill-browser browser)
  )

(defmethod (browser-browser-item view-buffer) (x)
  (=> browser documentation-buffer)
  )

(defmethod (browser-browser-item cleanup) ()
  )

(defmethod (browser-browser-item apply-filter) (filter)
  (apply filter (list browser))
  )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The browser-browser-prototype-item flavor:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(de define-browser-prototype (create-function display-text documentation-text)
  (let ((item (create-browser-browser-prototype-item create-function
						     display-text
						     documentation-text
						     )))
    (setf nmode-browser-prototypes
      (cons item nmode-browser-prototypes))
    ))

(de create-browser-browser-prototype-item (create-fcn display-text doc-text)
  (make-instance 'browser-browser-prototype-item
		 'create-function create-fcn
		 'display-text display-text
		 'documentation-text doc-text
		 ))

(defflavor browser-browser-prototype-item
  (display-text
   create-function
   documentation-text
   documentation-buffer
   )
  ()
  (gettable-instance-variables display-text)
  (initable-instance-variables display-text create-function documentation-text)
  )

(defmethod (browser-browser-prototype-item init) (init-plist)
  (setf display-text (string-concat "Prototype: " display-text))
  (setf documentation-buffer (create-unnamed-buffer read-only-text-mode))
  (=> documentation-buffer insert-text documentation-text)
  (=> documentation-buffer insert-eol)
  (=> documentation-buffer set-modified? NIL)
  (=> documentation-buffer move-to-buffer-start)
  (=> documentation-buffer set-label-string
      (string-concat "(Documentation on " display-text ")"))
  )

(defmethod (browser-browser-prototype-item update) ()
  T
  )

(defmethod (browser-browser-prototype-item kill) ()
  NIL
  )

(defmethod (browser-browser-prototype-item view-buffer) (x)
  documentation-buffer
  )

(defmethod (browser-browser-prototype-item cleanup) ()
  )

(defmethod (browser-browser-prototype-item apply-filter) (filter)
  T
  )

(defmethod (browser-browser-prototype-item instantiate) ()
  (apply create-function '())
  )
