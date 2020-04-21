.so pndoc:nman
.part NM-SUBSYSTEMS manual
@Chapter[Moving Up And Down Levels] 
  Subsystems and recursive editing levels are two states in which
you are temporarily doing something other than editing the visited
file as usual.  For example, you might be editing the arguments
prompted for by a M-X command, or using a browser.
@Section[Subsystems]
@node("subsystems")
  A @dfn[subsystem] is an NMODE function which is an interactive program in
its own right: it reads commands in a language of its own, and
displays the results.  You enter a subsystem by typing an NMODE
command which invokes it.  
Once entered, the subsystem usually runs until a
specific command to exit the subsystem is typed.
An example of an
NMODE subsystem is the buffer-browser, invoked by typing C-X C-B.

  The commands understood by a subsystem are usually not like NMODE
commands, because their purpose is something other than editing text.
In the buffer-browser, for instance,
the commands are tailored to moving up and down
a list of the existing buffers, reordering this list in various ways,
and to deleting buffers.
In NMODE, most commands are
Control or Meta characters because printing characters insert
themselves.  In most subsystems, there is no insertion of text, so
non-Control non-Meta characters can be the commands.

  While you are inside a subsystem, the mode line identifies the subsystem
by identifying the mode of the current buffer.
The special properties of the subsystem are due to the kinds of commands
that are available in this mode, and to the keys that the mode associates
with them.
Because each buffer has its own associated mode at any given time, if
a user moves out of the buffer associated with the subsystem into an
ordinary text buffer, he/she will have left the subsystem, even though
he/she will not have used the normal command for doing so.

  Because each subsystem implements its own commands, we cannot
guarantee anything about them.  However, there are conventions for
what certain commands ought to do:
@DoubleWideCommands{
Space	Moves downwards, like C-N in NMODE.

Q	Exits normally.

Help or ?	Prints documentation on the subsystem's commands.
}
Not all of these necessarily exist in every subsystem, however.
@Section[Recursive Editing Levels]
@node("recursive")
@Index{Recursive Editing Level}
@Index{Mode Line}
  A @dfn[recursive editing level] is a state in which part of the
execution of one command involves doing some editing.  You may be
editing the file you are working on, or you may be editing completely
something totally different from what you were working on at top
level.  Currently, the completion of extended commands, the preparation
of prompted input strings, and the examination of buffers in the
kill-some-buffers-command function all involve recursive editing levels
within which the full power of NMODE is available.
@Section[Exiting Levels; Exiting NMODE]
@index[stop]
@index[C-X C-Z]
@index[C-] L]
@fncindex{exit-nmode}
@fncindex{nmode-exit-to-superior}
@index{exiting}
  On the hp9836, <STOP> will exit from NMODE to the hp9836 workstation top
level command interpreter.
C-X C-Z will exit from
NMODE into the PSL interpreter,
as will C-] L (Lisp-L) in Lisp mode.
