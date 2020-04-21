% simple demo of tools for menus and break windows
% MLG and WFG

Symbolic Procedure MakeMenu();
% Setup the Menu Window
begin scalar oldbuffer;
    % Create the MENU buffer
    MenuBuffer:=CreateBuffer('MENU, eval DefaultMode);

    % Create (but don't "select") the window to look into the buffer.
    MenuWindow :=
        FramedWindowDescriptor('MENU,
                               % Starts at column 50,  Row 13
                               Coords(50,13),
                               Coords(25,7));

    % Set up the buffer text.
    oldbuffer := CurrentBufferName;
    SelectBuffer 'MENU;
    append_line("ERASE(); % the screen");
    append_line("ExitMenu();");
    append_line("KillMenu();");
    !$CRLF();

    % "Pop" back to original buffer.
    SelectBuffer oldbuffer;

    % Define a new key binding (for text mode) for popping up the menu.
    SetTextKey(Char Cntrl H, 'Menu);
end;

Procedure KillMenu(); % Exit and Wipe MENU
 <<!*KillMenu:=T; Throw('!$MENU!$,0)>>;

Procedure ExitMenu(); % Exit and LEAVE Menu
  <<!*KillMenu:=NIL; Throw('!$MENU!$,0)>>;

Fluid '(!*KillMenu);

procedure MenuReader();
   TopLoop('ReformXread,'NoPrint,'EVAL,"Menu","");

Procedure NoPrint x;
 X;

procedure Menu;
Begin Scalar W;
    % Need to select EMODE channels, since MENU is typically invoked while
    % "old" channels are selected.
    SelectEMODEChannels();

    W:=CurrentWindowdescriptor;
    SelectWindow MenuWindow$
    !$BeginningOfBuffer();   % Place point at start of buffer.

    % Transfer control to the menu reader.
    Catch('!$MENU!$, MenuReader() );

    % When finished, "pop" our screen off of the physical screen.
    If !*KillMenu then DeselectScreen CurrentVirtualScreen;

    SelectWindow W; % Back to the window we originally had.
end;

