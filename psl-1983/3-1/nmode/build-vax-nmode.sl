% A sketchy build file for NMODE binaries.  Probably best executed from within
% NMODE itself.  (Won't necessarily run "sequentially"--it should work, but
% it's never been tried, as of 29-mar-83.)

% NOTE:  need to build window stuff first, see $pw/VAX-SOURCES/build-windows.sl.

(off usermode)    % Avoid queries about redefining functions.

% NOTE: there are several problems with the PSL compiler (and LAP) that cause 
% problems when compiling NMODE (29-mar-83).  The following "patches"
% should fix things (on HP VENUS) until the compiler gets fixed up.

(setq options* NIL)    % Force reloading of files.
(load compiler)
% Fix problems with ASHL, etc.
(faslin "/vb/griss/vax-lap-fix.b")

% Avoid problem with cmacro expansion for the SUBSTRING function (cmacro
% seems silly anyway, overkill for imagined efficiency).  (Note that the
% cmacro isn't really at fault, it simply brings out the real problem(s)
% with the compiler.)
(load common)
(remprop 'substring 'cmacro)

(setf old-directory (pwd))

% Connect to the destination directory for the binaries.
(cd "$pn/BINARIES")

% Augment the directories used to lookup LOAD modules.
(setf loaddirectories*
      (append 
       '("" "$pn/BINARIES/" "$pw/BINARIES/")
       (delete "" loaddirectories*)))


(faslout "browser")
(dskin "$pn/browser.sl")
(faslend)

(faslout "browser-support")
(dskin "$pn/browser-support.sl")
(faslend)

(faslout "buffer")
(dskin "$pn/buffer.sl")
(faslend)

(faslout "buffer-io")
(dskin "$pn/buffer-io.sl")
(faslend)

(faslout "buffer-position")
(dskin "$pn/buffer-position.sl")
(faslend)

(faslout "buffer-window")
(dskin "$pn/buffer-window.sl")
(faslend)

(faslout "buffers")
(dskin "$pn/buffers.sl")
(faslend)

(faslout "case-commands")
(dskin "$pn/case-commands.sl")
(faslend)

(faslout "command-input")
(dskin "$pn/command-input.sl")
(faslend)

(faslout "commands")
(dskin "$pn/commands.sl")
(faslend)

(faslout "defun-commands")
(dskin "$pn/defun-commands.sl")
(faslend)

(faslout "dispatch")
(dskin "$pn/dispatch.sl")
(faslend)

(faslout "extended-input")
(dskin "$pn/extended-input.sl")
(faslend)

(faslout "fileio")
(dskin "$pn/fileio.sl")
(faslend)

(faslout "incr")
(dskin "$pn/incr.sl")
(faslend)

(faslout "indent-commands")
(dskin "$pn/indent-commands.sl")
(faslend)

(faslout "kill-commands")
(dskin "$pn/kill-commands.sl")
(faslend)

(faslout "lisp-commands")
(dskin "$pn/lisp-commands.sl")
(faslend)

(faslout "lisp-indenting")
(dskin "$pn/lisp-indenting.sl")
(faslend)

(faslout "lisp-interface")
(dskin "$pn/lisp-interface.sl")
(faslend)

(faslout "lisp-parser")
(dskin "$pn/lisp-parser.sl")
(faslend)

(faslout "m-x")
(dskin "$pn/m-x.sl")
(faslend)

(faslout "m-xcmd")
(dskin "$pn/m-xcmd.sl")
(faslend)

(faslout "modes")
(dskin "$pn/modes.sl")
(faslend)

(faslout "mode-defs")
(dskin "$pn/mode-defs.sl")
(faslend)

(faslout "move-commands")
(dskin "$pn/move-commands.sl")
(faslend)

(faslout "nmode-attributes")
(dskin "$pn/nmode-attributes.sl")
(faslend)

(faslout "nmode-break")
(dskin "$pn/nmode-break.sl")
(faslend)

(faslout "nmode-init")
(dskin "$pn/nmode-init.sl")
(faslend)

(faslout "nmode-parsing")
(dskin  "$pn/nmode-parsing.sl")
(faslend)

% Use Vax version of sources.
(faslout "nmode-vax")
(dskin  "$pn/VAX-SOURCES/nmode-vax.sl")
(faslend)

(faslout "prompting")
(dskin "$pn/prompting.sl")
(faslend)

(faslout "query-replace")
(dskin "$pn/query-replace.sl")
(faslend)

(faslout "reader")
(dskin "$pn/reader.sl")
(faslend)

(faslout "rec")
(dskin "$pn/rec.sl")
(faslend)

(faslout "screen-layout")
(dskin "$pn/screen-layout.sl")
(faslend)

(faslout "search")
(dskin "$pn/search.sl")
(faslend)

% Use Vax version of sources.
(faslout "set-terminal")
(dskin "$pn/VAX-SOURCES/set-terminal.sl")
(faslend)

(faslout "softkeys")
(dskin "$pn/softkeys.sl")
(faslend)
  
(faslout "structure-functions")
(dskin "$pn/structure-functions.sl")
(faslend)

(faslout "terminal-input")
(dskin "$pn/terminal-input.sl")
(faslend)

(faslout "text-buffer")
(dskin "$pn/text-buffer.sl")
(faslend)

(faslout "text-commands")
(dskin "$pn/text-commands.sl")
(faslend)

(faslout "window")
(dskin "$pn/window.sl")
(faslend)

(faslout "window-label")
(dskin "$pn/window-label.sl")
(faslend)

(faslout "autofill")
(dskin "$pn/autofill.sl")
(faslend)

(faslout "browser-browser")
(dskin "$pn/browser-browser.sl")
(faslend)

(faslout "buffer-browser")
(dskin "$pn/buffer-browser.sl")
(faslend)

%* (faslout "dired")
%* (dskin "$pn/dired.sl")
%* (faslend)

(faslout "doc")
(dskin "$pn/doc.sl")
(faslend)

(cd old-directory)

