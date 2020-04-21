.so pndoc:nman
.part NM-BROWSERS manual
@Chapter(Browser Subsystems)
@node("browsers")
@section[General Features of NMODE Browsers]
NMODE has a number of subsytems called browsers.
Among NMODE's browsers are
a buffer browser, 
a file browser,
a documentation browser,
and a browser browser.
A browser is a subsystem that displays a list of objects
and allows the user to select particular objects
from the list for viewing or editing.
The user can select objects by placing the cursor on their line.
The object pointed to by the cursor is considered the current object.
The list of the names of these objects is displayed immediately upon
entering the browser in question.
Because of NMODE's multiple window features,
the list of objects in the browser can often be displayed
at the same time as a portion of one of the objects.
In the buffer browser, for instance, it is possible to view
a buffer's contents in the lower window while still displaying the
list of all buffers in the upper window.
@subsection[Commands Common to Browser Subsystems]
@fncindex{browser-ignore-command}
@fncindex{browser-help-command}
@fncindex{browser-undo-filter-command}
@fncindex{browser-view-command}
@fncindex{browser-edit-command}
A number of commands are common to all the browser subsystems.
For instance,
in all the browsers the list of objects displayed
can be shortened selectively.
The I command (@fnc{browser-ignore-command})
will remove the current object
from the list.
The filter command F (which function is invoked depends on the browser)
will remove a set of objects, typically those
matching a user-supplied string in some way.
The options availible in the filter command differ from browser to browser.
They can always be displayed by typing ? after entering the filter
command with an F.
The list of objects can be restored to its former size by using the N
command (@fnc{browser-undo-filter-command}).

Other common commands are the
E command (@fnc{browser-edit-command}) and
the V command (@fnc{browser-view-command}).
They allow closer examination of the objects listed in the browser.
The current object is displayed when the view or edit command is given.
In split screen mode, edit will select the bottom window while
view does not.
Split screen mode can be activated by giving an argument to E or V.
In the buffer and file browsers, edit and view can be used to initiate
actual alteration of a buffer or file.
The buffer and file browsers are often used, in fact, to easily locate
and enter buffers and files with long names that the user has forgotten.
After editing a file or buffer one can escape back to the browser with
C-M-L.
Similarly, one can escape back out of any browser with a quit, Q,
command (which function is invoked depends on the browser).

As can be seen from these examples,
browser commands are often single printing characters, which are not
self-inserting in browser modes.
The browser helps users keep track of commands by
displaying an information line at the bottom of the screen.
This line shows the commands available in the browser,
with the character that invokes the command capitalized.
In addition to this cue the browsers provide
a line or two of on-line documentation
about each command.
This information can be displayed by typing ?
(@fnc{browser-help-command})
to the browser's top level.
@section[Invoking Browsers]
@keyindex{M-X Apropos}
@fncindex{apropos-command}
@keyindex{C-X C-B}
@fncindex{buffer-browser-command}
@keyindex{C-X D}
@fncindex{dired-command}
@keyindex{M-X Edit Directory}
@fncindex{edit-directory-command}
@keyindex{M-X List Browsers}
@fncindex{browser-browser-command}
  Each browser can be entered with a particular command.
The documentation browser can be entered with M-X Apropos
(@fnc{apropos-command}).
The buffer browser can be entered with C-X C-B
(@fnc{buffer-browser-command}).
The file browser can be entered through either C-X D
(@fnc{dired-command}) or through M-X Edit Directory
(@fnc{edit-directory-command}).
The browser-browser can be entered through M-X List Browsers
(@fnc{browser-browser-command}).
On the HP9836, several of these commands are availible through soft keys.

Another way to enter most of the browsers is to enter the browser-browser
and then create or visit a particular browser
with the B command (@fnc{browser-browser-browse-command}).
This will visit an existing browser, or create a new browser from a
browser template (possibly prompting the user for some input in the process).
