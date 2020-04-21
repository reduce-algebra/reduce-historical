% Loads "first half" of files necessary to build EMODE.
% Assumes that the "default directory" contains all the necessary files.

imports '(strings);   % Strings library needed at runtime.
in "temporary-emode-fixes.red"$
in "customize-rlisp-for-emode.sl"$    % Must be first?
in "envsel.sl"$   % Support for "environments"
in "dispch.sl"$  % "keyboard" dispatch support
in "emode1.red"$  % Bunches of stuff
in "misc-emode.sl"$       % miscellaneous utilities and commands
in "sleep.sl"$    % Utility to "sleep" until time limit or character typed.
in "ring-buffer.sl"$      % General "ring buffer" utilities
in "buffers.sl"$          % Misc stuff for manipulating EMODE buffers.
in "buffer-position.sl"$  % Utilities for handling "point" within buffer.
in "query-replace.sl"$    % Implements query-replace command.


in "window.sl"$
in "windows.sl"$
in "buffer.sl"$
