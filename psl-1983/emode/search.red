%
% SEARCH.RED - Search utilities for EMODE
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%

% These routines to implement minimal string searches for EMODE.  Searches
% are non-incremental, limited to single line patterns, and always ignore
% case.  This file also includes routines for moving over other patterns
% (words, etc.).

%%%%% Changes: %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% AS 7/15/82
% - Fixed skip_backward_blanks to behave properly at the beginning
%   of the buffer (loop termination test was incorrect).
% - Use sleep primitive for insert_matching_paren.

FLUID '(
    last_search_string
    );

Symbolic Procedure forward_string_search();
% Invoked from keyboard, search forward from point for string, leave
% "point" unchanged if not found.
begin scalar strng;
   % Get search string, update default.
    strng :=
      last_search_string :=
        prompt_for_string("Forward search: ", last_search_string);

    if buffer_search(strng, 1) then	% 1 for forward search, and if found
       for i := 0:size(strng) do	% move to end of string.
	  !$ForwardCharacter();
end;

Symbolic Procedure reverse_string_search();
% Invoked from keyboard, search backwards from point for string, leave
% "point unchanged if not found.
begin scalar strng;
    strng :=
      last_search_string :=
        prompt_for_string("Reverse Search: ", last_search_string);

    !$Backwardcharacter();	% Back up before starting search.
    if not buffer_search(strng, -1) then	% -1 for backward search
       !$ForwardCharacter();	% restore point if not found.
end;

Symbolic Procedure buffer_search(strng,dir);
% Search in buffer for strng.  "Ding" and leave point unchanged if
% not found, return NIL if not found.  dir is +1 for forward, -1
% for backward.
begin scalar search_point, search_lineindex, found, within_buffer;
    PutLine();                      % Make sure line is "saved" in buffer

    % Start at current location in the buffer.
    search_lineindex := CurrentLineIndex;
    search_point := min(point, size GetBufferText(search_lineindex));
    within_buffer :=  not EndOfBufferP(search_lineindex);

    while within_buffer
          and not (found := subscript(strng,
                                       GetBufferText(search_lineindex),
                                       search_point,
                                       dir))
    do
    <<
        % Move to "beginning" of "next" line
        if dir > 0 then
        <<
            within_buffer := not EndOfBufferP(NextIndex search_lineindex);
            if within_buffer then
            <<
                search_lineindex := NextIndex(search_lineindex);
                search_point := 0;
            >>;
        >>
        else
        <<
            within_buffer := not BeginningOfBufferP(search_lineindex);
            if within_buffer then
            <<
                search_lineindex := PreviousIndex(search_lineindex);
                search_point := size GetBufferText(search_lineindex);
            >>;
        >>;
    >>;

    if found then
    <<
        SelectLine(search_lineindex);
        point := found;
    >>
    else
        Ding();

    return found;
end;

Symbolic Procedure subscript(pattern,strng,start,dir);
% Locate pattern in strng, starting at "start", searching in direction
% "dir" (+1 for forward search, -1 for backward search).
% Return NIL if not found, otherwise return the subscript of the first
% matching character.
begin scalar found;
    while 0 <= start and start <= size strng
          and not (found := is_substring(pattern,strng,start))
    do
        start := start + dir;

    return
    if found then
        start
    else
        NIL;
end;

Symbolic Procedure RaiseChar(ch);
% Return character code for upper case version of character.
% (ch is a character code.)
    if ch < char lower 'a or ch > char lower 'z then
        ch
    else
        ch - char lower 'a + char 'A;

Symbolic Procedure is_substring(substrng,strng,start);
% Return T if substrng occurs as substring of strng, starting at "start".
% Ignore case differences.
begin scalar i;
    i := 0;

    while i <= size(substrng) and i+start <= size(strng)
          and RaiseChar substrng[i] = RaiseChar strng[i+start]
    do
        i := i + 1;

    return
        i > size(substrng);   % T if all chars matched, false otherwise.
end;

FLUID '(paren_depth);

Symbolic Procedure adjust_depth(ch);
% Adjust paren_depth based on the character.
    if ch = char !( then
        paren_depth := paren_depth + 1
    else if ch = char !) then
        paren_depth := paren_depth - 1;


Symbolic Procedure skip_forward_blanks();
% Skip over "blanks", return the first non-blank character seen.
begin scalar ch;
    while
       not (EndOfBufferP(NextIndex CurrentLineIndex)
            and point = length CurrentLine)
      AND
      % 17 means "ignore".
           CurrentScanTable!*[ch := CurrentCharacter()] = 17
    do
        !$ForwardCharacter();

    return ch;
end;

Symbolic Procedure skip_backward_blanks();
% Skip backwards over "blanks", return the first non-blank character seen.
begin scalar ch, flg;
    flg := T;
    while
       not (BeginningOfBufferP(CurrentLineIndex) and point = 0)
      AND
          flg
    do
    <<
        !$BackwardCharacter();
        % 17 means "ignore".
        flg :=  CurrentScanTable!*[ch := CurrentCharacter()] = 17
    >>;

    % Position "cursor" to the right of the terminating character.
    if not(BeginningOfBufferP(CurrentLineIndex) AND point = 0) then
        !$ForwardCharacter();

    return ch;
end;

Symbolic Procedure forward_word();
% Move forward one "word", starting from point.
begin scalar ch;
    while
        not (EndOfBufferP(NextIndex CurrentLineIndex)
            and point = length CurrentLine)
     AND
     % Scan for start of word.
         not(LetterP(ch := skip_forward_blanks()) OR DigitP(ch))
     do
         !$ForwardCharacter();

    % Now, scan for end of word.
    while
        not (EndOfBufferP(NextIndex CurrentLineIndex)
            and point = length CurrentLine)
       AND
        (LetterP(ch := CurrentCharacter()) OR DigitP(ch))
    do
        % Can't be a paren, so don't bother to count.
        !$ForwardCharacter();
end;

Symbolic Procedure backward_word();
% Move backward one "word", starting from point.
begin scalar ch,flg;
    flg := T;
    % Scan for the start of a word (a "letter" or digit).
    while   flg
          AND
            not(BeginningOfBufferP(CurrentLineIndex) AND point = 0)
    do
    <<
        !$BackwardCharacter();
        flg := not (LetterP(ch := CurrentCharacter()) OR DigitP(ch));
    >>;

    % Now, scan for "end" of identifier.
    flg := T;
    while   flg
          AND
              not(BeginningOfBufferP(CurrentLineIndex) AND point = 0)
    do
    <<
        !$BackwardCharacter();
        flg := (LetterP(ch := CurrentCharacter()) OR DigitP(ch));
    >>;

    % Position "cursor" to the right of the terminating character.
    if not(BeginningOfBufferP(CurrentLineIndex) AND point = 0) then
        !$ForwardCharacter();
end;

Symbolic Procedure LetterP(ch);
% Note that we don't use
    ch < 128 and CurrentScanTable!*[ch] equal 10;       % 10 means "a letter".

Symbolic Procedure forward_sexpr();
% Move forward over a set of balanced parenthesis (roughly speaking).
begin scalar ch, cline, cpoint, paren_depth;    % paren_depth is FLUID.
    % Remember our spot.
    cline := CurrentLineIndex;
    cpoint := point;
    paren_depth := 0;
    ch := skip_forward_blanks();
    adjust_depth(ch);

    if paren_depth > 0 then % Skip over balanced parens, if first thing was
                            % a paren.
    <<
        while not (EndOfBufferP(NextIndex CurrentLineIndex)
                    and point = length CurrentLine)
            AND
              paren_depth > 0
        do
        <<
            !$ForwardCharacter();
            adjust_depth CurrentCharacter();
        >>;

        % Complain, and avoid moving point, if match not found.
        if paren_depth > 0  then
        <<
            ding();
            PutLine();
            point := cpoint;
            GetLine(cline);
        >>
        else
            !$ForwardCharacter();       % Skip over trailing right paren.
    >>
    % Otherwise (paren not first character seen), just skip a word.
    else
        forward_word()
end;

Symbolic Procedure backward_sexpr();
% Move backwards over a set of balanced parenthesis (roughly speaking).
begin scalar ch, flg, cline, cpoint, paren_depth;    % paren_depth is FLUID.
    % Remember our spot.
    cline := CurrentLineIndex;
    cpoint := point;
    paren_depth := 0;
    ch := skip_backward_blanks();
    flg := T;

    if ch = char !) then    % Skip over balanced parens, if first thing was
                            % a paren.
    <<
        while not(BeginningOfBufferP(CurrentLineIndex) AND point = 0)
            AND
              flg
        do
        <<
            !$BackwardCharacter();
            adjust_depth CurrentCharacter();
            flg := paren_depth < 0; % (< 0, since this is backwards search! )
        >>;

        % Complain, and avoid moving point, if match not found.
        if paren_depth < 0  then
        <<
            ding();
            PutLine();
            point := cpoint;
            GetLine(cline);
        >>;

    >>
    % if a left paren, just back up slightly (a bit of a KLUDGE).
    else if ch = char !( then
        !$BackwardCharacter()
    % Otherwise (paren not first character seen), just skip a word.
    else
        backward_word();
end;

Symbolic Procedure insert_matching_paren();
% Insert a right parenthesis, back up to a matching left parenthesis, pause
% there a "second" and then come back to current location.
begin scalar cline, cpoint, flg, timer, paren_depth;
    InsertCharacter char !);    % (Or, InsertSelfCharacter?)

    cline := CurrentLineIndex;
    cpoint := point;
    paren_depth := 0;
    flg := T;

    while
        not(BeginningOfBufferP(CurrentLineIndex) AND point = 0)
      AND
        flg
    do
    <<
        !$BackwardCharacter();
        adjust_depth CurrentCharacter();
        flg := paren_depth < 0;
    >>;

    if flg then                 % No match found
        ding()
    else
    <<
        optional_refresh();     % Show where we are, if no typeahead.
        % "pause" for 1/2 sec (30/60ths) or until character is typed.
        sleep!-until!-timeout!-or!-input(30);
    >>;

    % Go back to original spot.
    point := cpoint;
    SelectLine(cline);
end;
