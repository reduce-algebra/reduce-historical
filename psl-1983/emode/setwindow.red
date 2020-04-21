 Procedure OneWindow();
% Dispatch to this routine to enter one-window mode.
    if MajorWindowCount neq 1 then      % If not already one-window
    <<
        % Setup windows for one window mode.
        Setup_Windows
            list(
              % Window one looks into current buffer, other arguments are
              % location of upper left corner, and the size (0 indexed).
              WindowDescriptor(1, CurrentBufferName,
                               ScreenBase,    % Upper left corner
                               % Size uses entire width, leaves room for
                               % three one line windows at the bottom
                               Coords(Column ScreenDelta,
                                       Row(ScreenDelta) - 3)),

              % Window 1001 looks into the "mode line" buffer.
              WindowDescriptor(1001, 'MODE_LINE,
                               % Base is two lines above bottom
                               Coords(Column ScreenBase,
                                       Row ScreenBase + Row ScreenDelta - 2),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0)),

              % Window 1002 looks into the "prompt line" buffer.
              WindowDescriptor(1002, 'PROMPT_BUFFER,
                               % Base is one line above bottom
                               Coords(Column ScreenBase,
                                       Row ScreenBase + Row ScreenDelta - 1),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0)),


              % Window 1003 looks into the "message buffer", used for error
              % messages and general stuff.
              WindowDescriptor(1003, 'MESSAGE_BUFFER,
                               % Base is at bottom
                               Coords(Column ScreenBase,
                                       Row ScreenBase + Row ScreenDelta),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0))
        );

        % Wierd, the code seems to usually work without the following call.
        % Needs to be rethought.
        SelectWindow 1;
        FullRefresh();  % A kludge, sigh.
        MajorWindowCount := 1;
    >>;

FLUID '(Fraction2);

Symbolic Procedure TwoWindows();
% Dispatch to this routine to enter two-window mode.
    if MajorWindowCount neq 2 then
    begin scalar MidPoint,frac1,lines;
        % Use roughly half (later to be a variable) the screen, allow for a
        % dividing line of dashes and 3 one line windows at the bottom.

        % MidPoint is location of dividing line of dashes, wrt ScreenBase.
        frac1:=Fraction2;
        if not(FloatP frac1 and frac1<0.9 and frac1 >0.1) then frac1:=0.5;
        lines:=(Row ScreenDelta - 3);
        MidPoint := Fix (frac1 * lines);
        if Midpoint <= 2  then Midpoint:=2;
        Setup_Windows
            list(
              % Window one looks into current buffer
              WindowDescriptor(1, CurrentBufferName,
                               ScreenBase,
                               Coords(Column ScreenDelta,
                               MidPoint - 1)),

              % Window 1000 looks into the dividing line of dashes
              WindowDescriptor(1000, 'DASHES,
                               Coords(Column ScreenBase, MidPoint),
                               Coords(Column ScreenDelta, 0)),

              % Window 2 always looks into the 'ALTERNATE_WINDOW buffer,
              % until we can figure out a better way of handling the
              % situation.
              WindowDescriptor(2, 'ALTERNATE_WINDOW,
                               Coords(Column ScreenBase, MidPoint + 1),
                               % Run down to the bottom, minus 3 one line
                               % windows.
                               Coords(Column ScreenDelta,
                                      Row ScreenDelta - MidPoint - 4)),

              % Window 1001 looks into the "mode line" buffer.
              WindowDescriptor(1001, 'MODE_LINE,
                               % Base is two lines above bottom
                               Coords(Column ScreenBase,
                                       Row ScreenBase + Row ScreenDelta - 2),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0)),

              % Window 1002 looks into the "prompt line" buffer.
              WindowDescriptor(1002, 'PROMPT_BUFFER,
                               % Base is one line above bottom
                               Coords(Column ScreenBase,
                                       Row ScreenBase + Row ScreenDelta - 1),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0)),


              % Window 1003 looks into the "message buffer", used for error
              % messages and general stuff.
              WindowDescriptor(1003, 'MESSAGE_BUFFER,
                               % Base is at bottom
                               Coords(Column ScreenBase,
                                       Row ScreenBase + Row ScreenDelta),
                               % a single line (so delta row = 0)
                               Coords(Column ScreenDelta, 0))
        );

        % Wierd, the code seems to usually work without the following call.
        % Needs to be rethought.
        SelectWindow 1;
        FullRefresh();  % A kludge, sigh.
        MajorWindowCount := 2;
    end;

Fraction2 :=0.5;

procedure ResetEmode(rows,cols,f);
  if cols >=10 and cols<=79
    and rows>=6 and rows <=60 then
     <<ScreenDelta:= Cols . Rows;
       If FloatP F and F>=0.1 and F <=0.9 then Fraction2:=F;
       if MajorWindowCount =1 then <<MajorWindowCount:=0;
                                     OneWindow()>>
      else
       if MajorWindowCount = 2 then <<MajorWindowCount:=0;
                                     TwoWindows()>>
    >>;

procedure resetrows(r);
 resetScreen(car ScreenDelta,r);


procedure SetEmode(rows,cols,f);
 Begin Scalar !*EMODE;
   if cols >=10 and cols<=79
      and rows>=6 and rows <=60 then
          ScreenDelta:= Cols . Rows;
   If FloatP F and f>=0.1 and f<=0.9 then Fraction2:=f;
   !*EMODE:=T;
    FreshEmode();
 End;

