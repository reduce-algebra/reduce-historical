%
% MOVE-STRINGS.RED - "Fast" string copying utilities.
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%


% Utilities for moving subranges of strings around (and other related
% operations).  Written in SysLisp for speed.  (Modeled after
% PI:STRING-OPS.RED and PI:COPIERS.RED.)  

% Equivalent routines for vectors should be added (one of these days).

on SysLisp;

syslsp procedure MoveSubstringToFrom(DestString, SourceString,
                                     DestIndex, SourceIndex,
                                     SubrangeLength);
% Quite a few arguments there, but should be clear enough?  Returns the
% modified destination string.
% WARNING--this version screws up when destination and source overlap
% (movement of one subrange of a string to another subrange of the same
% string.)
begin scalar rawsrc, rawdst, isrc, idst, maxindx, len, i;
    isrc := IntInf SourceIndex;
    idst := IntInf DestIndex;
    rawsrc := StrInf SourceString;
    rawdst := StrInf DestString;
    len := IntInf SubrangeLength;

    % Get upper bound on how far to copy--don't go past end of destination
    % or source, or subrange.
    % We want (i + idst) <= StrLen rawdst AND (i + isrc) <= StrLen rawsrc
    % AND i < SubrangeLength.  (Strictly less than SubrangeLength, since i
    % starts at 0.)   maxindx is the appropriate bound on i.

    maxindx := (StrLen rawdst) - idst;

    if maxindx >= len then
        maxindx := len-1;

    if maxindx > (StrLen rawsrc) - isrc then
        maxindx := (StrLen rawsrc) - isrc;

    i := 0;
loop:
        % if we've run out of stuff, quit.
        if i > maxindx then
            goto loopex;

        % Otherwise, copy the string.
        StrByt(rawdst, i + idst) := StrByt(rawsrc, i + isrc);

        i := i+1;
        goto loop;

loopex:

    return DestString;
end;

syslsp procedure FillSubstring(DestString, DestIndex, SubrangeLength, chr);
% Fill a subrange of a string with a character code.
begin scalar rawdst, rawchr, idst,len, maxindx, i;
    idst := IntInf DestIndex;
    rawdst := StrInf DestString;
    rawchr := IntInf chr;
    len := IntInf SubrangeLength;

    maxindx := StrLen rawdst;
    if maxindx >= len then
        maxindx := len-1;

    i := 0;
loop:
        % if we've run out of stuff, quit.
        if i > maxindx then
            goto loopex;

        % Copy the character into the destination.
        StrByt(rawdst, i + idst) := rawchr;

        i := i+1;
        goto loop;

loopex:

    return DestString;
end;

off SysLisp;
