.so pndoc:nman
.part NM-MISC manual
@Chapter[Miscellaneous Commands]
  This chapter covers some miscellaneous commands which don't fit
naturally into earlier chapters.

@keyindex{M-X Insert Date}
@fncindex{insert-date-command}
  M-X Insert Date (@fnc{insert-date-command}) inserts the current date into
the text in the current buffer.  The mark is put after the inserted date and
point is left unchanged.

@keyindex{M-X Make Space}
@fncindex{nmode-gc}
  M-X Make Space (@fnc{nmode-gc}) reclaims any wasted internal space.
It also indicates the remaining amount of free space.

@keyindex{M-X Write Screen}
@fncindex{write-screen-command}
  M-X Write Screen (@fnc{write-screen-command}) writes a copy of the
current screen to a file.
