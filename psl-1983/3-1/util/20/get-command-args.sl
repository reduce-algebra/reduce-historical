%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GET-COMMAND-ARGS -- get command line arguments
%%%
%%% Author: Cris Perdue
%%%  5 Apr 1983 1320-PST
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(load parse-command-string get-command-string)

%%% Returns a list of strings which are the command line
%%% arguments to the program that was run.  Program name is not
%%% included.  The code per se is not machine-dependent, but the
%%% idea of getting a "command string" is so.
(de get-command-args ()
  (parse-command-string (get-command-string)))
