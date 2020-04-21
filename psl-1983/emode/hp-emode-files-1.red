% Loads "first half" of files necessary to build EMODE.
% Assumes that the "default directory" contains all the necessary files.

imports '(strings jsys);   % These libraries needed at runtime.
in "temporary-emode-fixes.red"$
in "customize-rlisp-for-emode.sl"$    % Must be first?
in "envsel.sl"$   % Support for "environments"
in "dispch.sl"$  % "keyboard" dispatch support
in "emode1.red"$  % Bunches of stuff
in "ring-buffer.sl"$
in "buffer-position.sl"$
in "query-replace.sl"$
in "buffers.sl"$
in "window.sl"$
in "windows.sl"$
in "dired.sl"$
in "sleep.sl"$
in "buffer.sl"$
