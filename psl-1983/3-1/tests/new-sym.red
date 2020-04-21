%  Replacements for functions in usual xxx-CROSS.EXE which only read/write
%  xxx.SYM if flags !*symread/!*symwrite are T;  otherwise symbols are
%  assumed to be already loaded (read case) or the cross-compiler is to
%  be saved intact with symbols (write case).


lisp procedure ASMEnd;
<<  off SysLisp;
    if !*MainFound then
    <<  CompileUncompiledExpressions();
%	WriteInitFile();
	InitializeSymbolTable() >>
    else WriteSymFile();
    CodeFileTrailer();
    Close CodeOut!*;
    DataFileTrailer();
    Close DataOut!*;
    Close InitOut!*;
    RemD 'Lap;
    PutD('Lap, 'EXPR, cdr GetD 'OldLap);
    DFPRINT!* := NIL;
    !*DEFN := NIL;
    WriteSaveFile()
 >>;

lisp procedure ReadSymFile();
    if !*symread then
       LapIN InputSymFile!*
    else off usermode;

lisp procedure WriteSymFile();
begin scalar NewOut, OldOut;
    if !*symwrite then <<
       OldOut := WRS(NewOut := Open(OutputSymFile!*, 'OUTPUT));
       print list('SaveForCompilation,
	          MkQuote('progn . car ToBeCompiledExpressions!*));
       SaveIDList();
       SetqPrint 'NextIDNumber!*;
       SetqPrint 'StringGenSym!*;
       MapObl function PutPrintEntryAndSym;
       WRS OldOut;
       Close NewOut; >>;
end;

lisp procedure WriteSaveFile();
    if !*symsave and (null !*mainfound) then 
% restore some initial conditions
      <<!*usermode := nil;
      DataExporteds!* := DataExternals!* := nil;
      CodeExporteds!* := CodeExternals!* := nil;
      !*MainFound:= nil;
% save the cross-compiler with symbol tables intact
      dumplisp(cross!-compiler!-name)
      >>;
!*symwrite := !*symread := nil;
!*symsave := T;



