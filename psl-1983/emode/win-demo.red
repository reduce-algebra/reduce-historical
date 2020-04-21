procedure BufferNames;
 Mapcar(WindowList,'cdar);

BufferNames();

procedure FindWindowName N;
 FindWindowField('WindowsBufferName,N);


procedure FindWindowField(F,N);
 begin scalar x;
   x:=WindowList;
  l: if null x then return NIL;
     if Cdr atsoc(F,car x) eq N then return car x;
     x:=cdr x;
     goto l
  end;

procedure SelectName N;
 Begin scalar x;
 x:=FindWindowName N;
 SelectWindow x;
end;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Following stuff is used to set up a BREAK window

<<
    % Create the BREAK buffer
    BreakBuffer:=CreateBuffer('!B!r!e!a!k, eval DefaultMode);

    % Create the window to look into the buffer.
    BreakWindow :=
        FramedWindowDescriptor('!B!r!e!a!k,
                               % Starts at column 50, Near top of screen
                               Coords(50,1),
                               % Dimensions are roughly 29 by 10?
                               Coords(28,9));

    % Set up the buffer text.

    SelectBuffer '!B!r!e!a!k;

    !$CRLF();
    Insert_string("q % To quit");
    !$CRLF();

    Insert_string("t % To traceback");
    !$CRLF();

    Insert_string("i % Trace interpreted stuff");
    !$CRLF();

    Insert_string("r % Retry");
    !$CRLF();

    Insert_string("c %Continue,");
    !$CRLF();
    Insert_string("  %using last value");
    !$CRLF();

    DeselectBuffer '!B!r!e!a!k;


    CopyD('Oldbreak,'Break);
    Flag('(Break),'User);
>>;

procedure Break;
 Begin Scalar W;
    W:=CurrentWindowdescriptor;
    SelectWindow BreakWindow$
    !$BeginningOfBuffer();   % Place point at start of buffer.

    % Transfer control to the original break handler.
    Catch(NIL, OldBreak() );

    % When finished, "pop" our screen off of the physical screen.
    DeselectScreen CurrentVirtualScreen;

    SelectWindow W; % Back to the window we originally had.
%    If !*QuitBreak then StdError "exit";  % ????
 end;


car 1; % Execute this to test the system.
