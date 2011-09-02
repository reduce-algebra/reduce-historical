% ----------------------------------------------------------------------
% $Id: ioto.red,v 1.4 2003/12/02 15:27:36 sturm Exp $
% ----------------------------------------------------------------------
% Copyright (c) 1995-1999 Andreas Dolzmann and Thomas Sturm
% ----------------------------------------------------------------------
% $Log: ioto.red,v $
% Revision 1.4  2003/12/02 15:27:36  sturm
% Introduced ioto_nterpri to avoid ugly linebreaks in verbosity output.
%
% Revision 1.3  1999/03/22 15:22:20  dolzmann
% Changed copyright information.
% Corrected comments.
%
% Revision 1.2  1999/01/17 15:32:20  dolzmann
% Added comments.
%
% Revision 1.1  1996/04/30 12:06:42  sturm
% Merged ioto, lto, and sfto into rltools.
%
% Revision 1.1  1996/03/22 11:58:08  sturm
% Moved and renamed. Previously iotopsl.red.
%
% Revision 1.5  1996/03/09 13:34:44  sturm
% Added use of !#-macros for resolving Lisp dependencies.
% Minor modifications in procedure ioto_realtime.
%
% Revision 1.4  1996/03/04 17:20:02  sturm
% Added procedure ioto_prtmsg.
% Tried to achive CSL compatibility:
% Added procedures ioto_pslp, ioto_flush.
% Used SL function posn instead of system call.
% Under CSL, ioto_realtime should return "???" now.
%
% Revision 1.3  1995/08/30  08:10:33  sturm
% Added procedure procedure ioto_cplu. :-)
%
% Revision 1.2  1995/07/07  10:55:51  sturm
% Added procedure ioto_realtime.
%
% Revision 1.1  1995/06/21  14:32:12  dolzmann
% Initial check-in.
%
% ----------------------------------------------------------------------
lisp <<
   fluid '(ioto_rcsid!* ioto_copyright!*);
   ioto_rcsid!* := "$Id: ioto.red,v 1.4 2003/12/02 15:27:36 sturm Exp $";
   ioto_copyright!* := "Copyright (c) 1995-1999 by A. Dolzmann and T. Sturm"
>>;

module ioto;
% Input/Output tools.

fluid '(ioto_realtime!* datebuffer);

ioto_realtime!* := 0;

procedure ioto_prin2(l);
   % Input/Output tools prin2. [l] is an atom or a list. Returns ANY.
   % Prints either the atom [l] or each element in the list [l]
   % without any space between the elements. The output is not
   % buffered.
   ioto_prin21(l,nil,nil,nil);

procedure ioto_tprin2(l);
   % Input/Output tools conditional terpri prin2. [l] is an atom or a
   % list. Returns ANY. Equivalent to [ioto_cterpri();ioto_prin2(l)].
   ioto_prin21(l,t,nil,nil);

procedure ioto_prin2t(l);
   % Input/Output tools prin2 conditional terpri. [l] is an atom or a
   % list. Returns ANY. Equivalent to [ioto_prin2(l);ioto_cterpri()].
   ioto_prin21(l,nil,t,nil);

procedure ioto_tprin2t(l);
   % Input/Output tools conditional terpri prin2 conditional terpri.
   % [l] is an atom or a list. Returns ANY. Equivalent to
   % [ioto_cterpri();ioto_prin2(l);ioto_cterpri()].
   ioto_prin21(l,t,t,nil);

procedure ioto_prtmsg(l);
   % Input/Output tools print message. [l] is an atom or a list.
   % Returns ANY. Prints either the atom [l] or each element in the
   % list [l] seperated by one space between the elements. The output
   % is not buffered. Does before and after the printing an
   % [ioto_cterpri].
   ioto_prin21(l,t,t,t);

procedure ioto_prin21(l,flg1,flg2,spc);
   % Input/Output tools prin2 subroutine. [l] is an atom or a list;
   % [flg1], [flg2], and [spc] are Boolean. Returns ANY.
   <<
      if l and atom l then l := {l};
      if flg1 then ioto_cterpri();
      for each x in l do <<
 	 prin2 x;
 	 if spc then prin2 " "
      >>;
      ioto_flush();
      if flg2 then ioto_cterpri()
   >>;

procedure ioto_cterpri();
   % Input/Output tools conditional terpri. No parameter. Returns ANY.
   % Does a [terpri()] if the cursor is not on the beginning of a
   % line.
   if posn()>0 then
      terpri();

procedure ioto_nterpri(n);
   if posn() + n > linelength nil then
      terpri();

fluid '(fancy!-switch!-on!* fancy!-switch!-off!*);

procedure ioto_cplu(s,c);
   % Input/Output tools conditional plural. [s] is a string; [c] is
   % Boolean. Returns a string. Appends a ``s'' to [s], provided that
   % [c] is non-[nil].
   if c then compress reversip('!" . '!s . cdr reversip explode s) else s;

procedure ioto_realtime();
   % Input/Output tools real time. No parameter. Returns wall clock
   % seconds since previous call.
   begin scalar aa,res;
      aa := ioto_datestamp();
      res := aa - ioto_realtime!*;
      ioto_realtime!* := aa;
      return res
   end;

procedure ioto_flush();
   % Input/Output flush. No parameter. Returns ANY. Flushes the output
   % buffer.
!#if (memq 'psl lispsystem!*)
   <<
      flushbuffer out!*;
      channelflush out!*
   >>;
!#else
      flush();
!#endif

procedure ioto_datestamp();
   % Input/Output datestamp. No parameter. Returns an integer the
   % number of secons since an fixed date.
!#if (memq 'psl lispsystem!*)
   <<
      date();
      sys2int wgetv(datebuffer,0)
   >>;
!#else
   datestamp();
!#endif

endmodule;  % [ioto]

end;  % of file
