%**********************************************************************
module crackpvm$
%**********************************************************************
%  Procedures to support parallel computing on PVM 
%  Authors: Thomas Wolf and Winfried Neun 2002

symbolic procedure pvm_activate$
if getd('pvm_mytid) then <<pvm_able:=t;current_dir:=pwd()>>
                    else <<pvm_able:=nil;current_dir:=nil>>$

symbolic procedure pvm_active$
if pvm_able then t else <<
 terpri()$write"PVM is either not active on this computer"$
 terpri()$write"or interactively switched off. Try 'ep'."$
 terpri()$
 nil
>>$

symbolic procedure pvm_try$
if pvm_able and (read_proczaehler()<24) then t
                                        else nil$

%symbolic procedure inc_session$
%begin scalar sess$
% setq(sess,bldmsg("%w%w",session_,"processes"));
% lock sess;
% in sess;
% backup_:=add1 backup_;
% out sess;
% write "off echo$backup_:=",backup_,"$ end$"$
% terpri()$
% shut sess;
% unlock sess;
%end$

symbolic procedure read_proczaehler$
begin scalar zae,zaef$
  zaef:=open("/home/twolf/zaehler",'input);
  zae:=channelread(zaef);
  close(zaef);
  return zae
end$

symbolic procedure add_process(processes,pdes,forg)$
begin scalar s,ss,h$
 %----- passing on the current status
 ss:=level_string(session_)$
 setq(h,bldmsg("%w%w%w",current_dir,ss,"tmp"));
 backup_to_file(pdes,forg,h);
 !*iconic:=nil$  %t;  %not !*batch_mode;
 % =t --> only an icon opens, =nil --> a window opens

 %----- start new process
 s:=remote_process("");
 processes:=cons(s,processes);

 %----- update the counting of processes
 system "/home/twolf/proczaehler plus"$

 %----- print the new level
 remote_write(s,{'list,"Process id: ",s," level: ",ss});
 terpri()$write"A process with id no ",s," has been started."$

 %----- change directory
 remote_call!*(s,'cd,{current_dir},0)$

 %----- load crack
 remote_call!*(s,'load_package,{{"~/red/src5/crack"}},0)$

 %----- specify what crack should load (and do)
 remote_call!*(s,'set,{'old_history,{'rb,h}},0);

 %----- start Crack
 remote_call(s,'crackshell,{'list},0)$

 %----- if interactive then not instant closure of window
 %----- when computation is finished to inspect the result
 %----- so wait for 1000 sec.
 if null !*iconic then remote_call!*(s,'system,{"sleep 1000"},0);

 %----- close the REDUCE session when crackshell() finished
 remote_call!*(s,'system,{"/home/twolf/proczaehler minus"},0)$
 remote_call!*(s,'system,{bldmsg("%w%w","rm ",h)},0)$
 remote_call!*(s,'exitlisp,{'list},0)$

 return processes
end$

symbolic procedure drop_process(processes)$
begin scalar s$
  terpri()$write"The following processes are active: "$
  listprint(processes)$
  terpri()$write"To kill ALL remote processes that have been"$
  terpri()$write"generated by this process enter -1,"$
  terpri()$write"to kill a single process enter its process id: "$
  promptstring!*:=""$
  s:=termread()$
  if s=-1 then <<
    processes:=nil;
    system "/home/twolf/proczaehler init"
  >>      else <<
    delete(s,processes);
    system "/home/twolf/proczaehler minus"
  >>$
  remote_kill(s);
  return processes
end$

symbolic procedure remote_crackmain(pdes,forg)$
begin scalar h,s,ss;

%BUG? Some global variables have to be passed on even if they
%should not be read when returning!?

 %----- passing on the current status
 ss:=level_string(session_)$
 setq(h,bldmsg("%w%w%w",current_dir,ss,"tmp"));
 backup_to_file(pdes,forg,h);
 % !*iconic:=nil$  %t;  %not !*batch_mode;
 % =t --> only an icon opens, =nil --> a window opens

 %----- start new process
 s:=remote_process("");

 %----- update the counting of processes
 system "/home/twolf/proczaehler plus"$

 %----- print the new level
 remote_write(s,{'list,"Process id: ",s," level: ",ss});

 %----- change directory
 remote_call!*(s,'cd,{current_dir},0)$

 %----- load crack
 remote_call!*(s,'load_package,{{"~/red/src5/crack"}},0)$

 %----- specify what crack should load (and do)
 remote_call!*(s,'set,{'old_history,{'rb,h}},0);

 %----- start Crack
 remote_call(s,'crackshell,{'list},0)$

 %----- if interactive then not instant closure of window
 %----- when computation is finished to inspect the result
 %----- so wait for 1000 sec.
 if null !*iconic then remote_call!*(s,'system,{"sleep 1000"},0);

 %----- close the REDUCE session when crackshell() finished
 remote_call!*(s,'system,{"/home/twolf/proczaehler minus"},0)$
 remote_call!*(s,'system,{bldmsg("%w%w","rm ",h)},0)$
 remote_call!*(s,'exitlisp,{},0)$

 %----- changes done at the end of crackmain(), e.g. delete property lists
 level_:=cdr level_;
 for each s in pdes do drop_pde(s,pdes,nil)$
 for each s in forg do
 if pairp s then setprop(cadr s,nil)
            else setprop(     s,nil)$
end$

endmodule$

end$
