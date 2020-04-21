%
% ENVSEL.SL - Utilities for switching between "environments".
% 
% Author:      William F. Galway
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        8 June 1982
% Copyright (c) 1982 University of Utah
%

% Utilities for switching between environments in EMODE.  Both buffers and
% windows are examples of environments.  Currently an environment is just
% an association list of (name . value)'s.

% Store variable bindings in association list.
(DE SaveEnv (env)
  (progn
    (for (in binding-pair env)
      % Replace the cdr with the value of the car.
      (do
        (RPLACD binding-pair (eval (car binding-pair)))))

    % Return the updated environment.
    env))

% Establish ("restore") the bindings stored in association list "env"
(DE RestoreEnv (env)
  (for (in binding-pair env)
    (do
      (set (car binding-pair) (cdr binding-pair)))))
