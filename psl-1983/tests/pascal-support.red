{ XXX Support Routines, Test Version 
  M. L. Griss, and S. Lowder 9 July 1982 
}

 Var  Ctime:Integer;             { For CPU Time }

 Procedure XXX_Init(var c:integer);
  begin
    WriteLn(Output, ' Init the XXX package ',c);
    Ctime :=10*SysClock;  { First Call on Timer }
  end;

 Procedure XXX_PutC(var c:integer);
  begin
    Write(Output,chr(c));
  end;

 Procedure XXX_GetC(var c:integer);
  var ch:char;
  begin
    read(keyboard,ch);
    c := ord(ch);
  end;

 Procedure XXX_TimC(var c:integer);
  var i:integer;
  begin
    i:=10* SysClock;      {Call timer again}
    c := i-Ctime;
    Writeln(Output,' Ctime ', i, c);
    Ctime := i;
  end;

 Procedure XXX_Quit(var c:integer);       { close files, cleanup and exit }
  begin
    Writeln(Output,' Quitting ');
    ESCAPE(0);    { "normal" exit, ie HALT}
  end;

 Procedure XXX_Err(var c:integer);
  begin
    Writeln(Output,' XXX Error call Number: ', c);
    ESCAPE(c);
  end;

 Procedure XXX_PutI(var c:integer);   { Print an Integer }
  begin
    Writeln(Output,' PutI: ', c);
  end;


end. 
