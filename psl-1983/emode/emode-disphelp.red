% Stolen from PI:HELP.RED--modified to run under EMODE.

lisp procedure DisplayHelpFile F;	%. Type help about 'F'
begin scalar NewIn, C, !*Echo;
    (lambda(!*Lower);
	F := BldMsg(HelpFileFormat!*, F))(T);
    NewIn := ErrorSet(list('Open, MkQuote F, '(quote Input)), NIL, NIL);

    if not PairP NewIn then
	ErrorPrintF("*** Couldn't find help file %r", F)
    else
    <<  NewIn := car NewIn;
        SelectBuffer('ALTERNATE_WINDOW);
        read_channel_into_buffer(NewIn);         % (Closes NewIn when done.)
    >>;
end;
