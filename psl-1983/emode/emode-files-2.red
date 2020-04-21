% Loads "second half" of files necessary to build EMODE.
% Assumes that the "default directory" contains all the necessary files.

% Utilities for getting prompted input, and general management of
% MODE/PROMPT/MESSAGE lines.
in "prompting.sl"$

in "search.red"$   % Utilities for string search.

in "move-strings.red"$     % "Fast" string utilities.
in "vs-support.sl"$       % Some more "fast" support for V-SCREEN
                                % (Virtual Screen) package.
in "v-screen.sl"$
in "refresh.red"$          % Screen/windows/refresh stuff

in "fileio.sl"$           % I/O routines for reading/writing EMODE
                           % buffers.

in "rface.red"$    % Special "mode" for executing Rlisp/Lisp
in "hp-emodex.sl"$ % Contributions from Hewlett Packard (Alan Snyder).
