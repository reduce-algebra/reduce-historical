%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Buffer.SL - Auxiliary Functions for manipulating the current buffer.
% 
% Author:      Alan Snyder
%              Hewlett-Packard/CRC
% Date:        20 August 1982
% Revised:     16 February 1983
%
% 16-Feb-83 Alan Snyder
%   Declare -> Declare-Flavor.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(CompileTime (load objects))

(fluid '(nmode-current-buffer))

(declare-flavor text-buffer nmode-current-buffer)

(de buffer-get-position ()
  % Return the "current position" in the current buffer as a BUFFER-POSITION
  % object.
  (=> nmode-current-buffer position))

(de buffer-set-position (bp)
  % Set the "current position" in the current buffer from the specified
  % BUFFER-POSITION object.  Clips the line-position and char-position.

  (if bp (=> nmode-current-buffer set-position bp)))

(de current-buffer-goto (line-number char-number)
  % Set the "current position" in the current buffer.
  % Clips the line-position and char-position.
  (=> nmode-current-buffer goto line-number char-number))

(de current-line-pos ()
  % Return the "current line position" in the current buffer.
  (=> nmode-current-buffer line-pos))

(de set-line-pos (n)
  % Set the "current line position" in the current buffer.
  % Clips the line-position and char-position.
  (=> nmode-current-buffer set-line-pos n))

(de current-char-pos ()
  % Return the "current character position" in the current buffer.
  (=> nmode-current-buffer char-pos))

(de set-char-pos (n)
  % Set the "current character position" in the current buffer.
  % Clips the specified position to lie in the range 0..line-length.
  (=> nmode-current-buffer set-char-pos n))

(de current-display-column ()
  % Return the column index corresponding to the current character position
  % in the display of the current line.  In other words, what screen column
  % should the cursor be in (ignoring horizontal scrolling)?
  (map-char-to-column (current-line) (current-char-pos)))

(de set-display-column (n)
  % Adjust the character position within the current buffer so that
  % the current display column will be the smallest possible value
  % not less than N.  (The display column may differ than N because
  % certain characters display in multiple columns.)
  (set-char-pos (map-column-to-char (current-line) n)))

(de current-buffer-size ()
  % Return the number of lines in the current buffer.
  % This count may include a fake empty line at the end of the buffer.
  (=> nmode-current-buffer size))

(de current-buffer-visible-size ()
  % Return the apparent number of lines in the current buffer.
  % The fake empty line that may be present at the end of the
  % buffer is not counted.
  (=> nmode-current-buffer visible-size))

(de current-line ()
  % Return the current line in the current buffer (as a string).
  (=> nmode-current-buffer fetch-line (current-line-pos)))

(de current-line-replace (s)
  % Replace the current line of the current buffer with the specified string.
  (=> nmode-current-buffer store-line (current-line-pos) s))

(de current-buffer-fetch (n)
  % Return the line at line position N within the current buffer.
  (=> nmode-current-buffer fetch-line n))

(de current-buffer-store (n l)
  % Store the line L at line position N within the current buffer.
  (=> nmode-current-buffer store-line n l))

(de set-mark (bp)
  % PUSH the specified position onto the ring buffer of marks associated with
  % the current buffer.  The specified position thus becomes the current "mark".
  (=> nmode-current-buffer set-mark bp))

(de set-mark-from-point ()
  % PUSH the current position onto the ring buffer of marks associated with
  % the current buffer.  The current position thus becomes the current "mark".
  (=> nmode-current-buffer set-mark-from-point))

(de current-mark ()
  % Return the current mark associated with the current buffer.
  (=> nmode-current-buffer mark))

(de previous-mark ()
  % POP the current mark off the ring buffer of marks associated with the
  % current buffer. Return the new current mark.
  (=> nmode-current-buffer previous-mark))

(de reset-buffer ()
  % Reset the contents of the current buffer to empty and "not modified".
  (=> nmode-current-buffer reset))

(de extract-region (delete-it bp1 bp2)

  % Delete (if delete-it is non-NIL) or copy (otherwise) the text between
  % position BP1 and position BP2.  Return the deleted (or copied) text as a
  % pair (CONS direction-of-deletion vector-of-strings).  The returned
  % direction is +1 if BP1 <= BP2, and -1 otherwise.  The current position is
  % set to the beginning of the region if deletion is performed.

  (=> nmode-current-buffer extract-region delete-it bp1 bp2))

(de extract-text (delete-it bp1 bp2)

  % Delete (if delete-it is non-NIL) or copy (otherwise) the text between
  % position BP1 and position BP2.  Return the deleted (or copied) text as a
  % vector-of-strings.  The current position is set to the beginning of the
  % region if deletion is performed.

  (cdr (=> nmode-current-buffer extract-region delete-it bp1 bp2)))

(de current-line-length ()
  % Return the number of characters in the current line.
  (=> nmode-current-buffer current-line-length))

(de current-line-empty? ()
  % Return T if the current line contains no characters.
  (=> nmode-current-buffer current-line-empty?))

(de current-line-blank? ()
  % Return T if the current line contains no non-blank characters.
  (=> nmode-current-buffer current-line-blank?))

(de at-line-start? ()
  % Return T if we are positioned at the start of the current line.
  (=> nmode-current-buffer at-line-start?))

(de at-line-end? ()
  % Return T if we are positioned at the end of the current line.
  (=> nmode-current-buffer at-line-end?))

(de at-buffer-start? ()
  % Return T if we are positioned at the start of the buffer.
  (=> nmode-current-buffer at-buffer-start?))

(de at-buffer-end? ()
  % Return T if we are positioned at the end of the buffer.
  (=> nmode-current-buffer at-buffer-end?))

(de current-line-is-first? ()
  % Return T if the current line is the first line in the buffer.
  (=> nmode-current-buffer current-line-is-first?))

(de current-line-is-last? ()
  % Return T if the current line is the last line in the buffer.
  (=> nmode-current-buffer current-line-is-last?))

(de current-line-fetch (n)
  % Return the character at character position N within the current line.
  % An error is signalled if N is out of range.
  (=> nmode-current-buffer current-line-fetch n))

(de current-line-store (n c)
  % Store the character C at char position N within the current line.
  % An error is signalled if N is out of range.
  (=> nmode-current-buffer current-line-store n c))

(de move-to-buffer-start ()
  % Move to the beginning of the current buffer.
  (=> nmode-current-buffer move-to-buffer-start))

(de move-to-buffer-end ()
  % Move to the end of the current buffer.
  (=> nmode-current-buffer move-to-buffer-end))

(de move-to-start-of-line ()
  % Move to the beginning of the current line.
  (=> nmode-current-buffer move-to-start-of-line))

(de move-to-end-of-line ()
  % Move to the end of the current line.
  (=> nmode-current-buffer move-to-end-of-line))

(de move-to-next-line ()
  % Move to the beginning of the next line.
  % If already at the last line, move to the end of the line.
  (=> nmode-current-buffer move-to-next-line))

(de move-to-previous-line ()
  % Move to the beginning of the previous line.
  % If already at the first line, move to the beginning of the line.
  (=> nmode-current-buffer move-to-previous-line))

(de move-forward ()
  % Move to the next character in the current buffer.
  % Do nothing if already at the end of the buffer.
  (=> nmode-current-buffer move-forward))

(de move-backward ()
  % Move to the previous character in the current buffer.
  % Do nothing if already at the start of the buffer.
  (=> nmode-current-buffer move-backward))

(de next-character ()
  % Return the character to the right of the current position.
  % Return NIL if at the end of the buffer.
  (=> nmode-current-buffer next-character))

(de previous-character ()
  % Return the character to the left of the current position.
  % Return NIL if at the beginning of the buffer.
  (=> nmode-current-buffer previous-character))

(de insert-character (c)
  % Insert character C at the current position in the buffer and advance past
  % that character.
  (=> nmode-current-buffer insert-character c))

(de insert-eol ()
  % Insert a line-break at the current position in the buffer and advance to
  % the beginning of the newly-formed line.
  (=> nmode-current-buffer insert-eol))

(de insert-line (l)
  % Insert the specified string as a new line in front of the
  % current line.  Advance past the newly inserted line.
  (=> nmode-current-buffer insert-line l))

(de insert-string (s)
  % Insert the string S at the current position.  Advance past the
  % newly-inserted string.  Note: S must not contain EOL characters!
  (=> nmode-current-buffer insert-string s))

(de insert-text (v)

  % V is a vector of strings similar to LINES (e.g., the last string in V is
  % considered to be an unterminated line).  Thus, V must have at least one
  % element.  Insert this stuff at the current position and advance past it.

  (=> nmode-current-buffer insert-text v))

(de delete-next-character ()
  % Delete the next character.
  % Do nothing if at the end of the buffer.
  (=> nmode-current-buffer delete-next-character))

(de delete-previous-character ()
  % Delete the previous character.
  % Do nothing if at the beginning of the buffer.
  (=> nmode-current-buffer delete-previous-character))

(undeclare-flavor nmode-current-buffer)
