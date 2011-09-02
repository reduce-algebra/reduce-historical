module autopatch; % Fetch and update patches fasl file.

% Author: Arthur C. Norman.

% Modifications by: Anthony C. Hearn.

fluid '(!*home);

global '(patch!-url!-list!* personal!-dir!*);

symbolic procedure add!_patch!_url u;
  patch!-url!-list!* := u . patch!-url!-list!*;

add!_patch!_url "http://reduce-algebra.com/support/patches/patches.fsl";

symbolic procedure get!-checksum file;
   begin scalar file1; integer c,checksum;
      if not filep file then return nil;
      file1 := binopen(file,'input);
      for i := 1:16 do <<if (c := readb file1) = !$eof!$ then c := 0;
			 checksum := 256*checksum + c>>;
      return checksum
   end;

symbolic procedure
      write!-patch!-file(dir,checksum,remote!-file,remote!-checksum);
   begin scalar p,w; integer c;
      % Read rest of remote file.
      while (c := readb remote!-file) neq !$eof!$ do p := c . p;
      close remote!-file;
      % Transcribe remote file data into a string.
      w := make!-simple!-string length p;
      c := -1;
      for each x in reversip p do putv!-char(w,c := c + 1,x);
      % Check checksum of data as fetched.
      if md5 w neq remote!-checksum
	then rederr "Checksum on fetched patches is incorrect";
      % Write out updated file.
      p := concat(dir,"/patches.fsl");
      if filep p then rename!-file(p,concat(dir,"/patches.old"));
      binary_open_output p;
      for each x in reversip checksum do binary_prinbyte x;
      for i := 0:upbv w do binary_prinbyte scharn(w,i);
      binary_close_output()
   end;

symbolic procedure rename!-home!-patch!-file;
   (filep x and rename!-file(x,concat(personal!-dir!*,"/patches.old")))
      where x = concat(personal!-dir!*,"/patches.fsl");

symbolic procedure update!_reduce;
   begin scalar c,lisp!-d,p,remote,w; integer remote!-checksum;
      if memq('demo, lispsystem!*) then
         rederr "Update service not available in demo version";
      lisp!-d := get!-lisp!-directory();
   % Find a site with the updates.
      w := patch!-url!-list!*;
      while null remote and w do <<remote:= open!-url car w; w:= cdr w>>;
      if null remote
	then <<terpri();
	 printc "*** Unable to access any of the remote patches files:";
	 for each s in patch!-url!-list!* do <<ttab 4; printc s>>;
	 return nil>>;
   % Fetch 16 bytes of checksum from the start of update file.
      for i := 1:16 do <<if (c := readb remote) = !$eof!$ then c := 0;
			 p := c . p;
			 remote!-checksum := 256*remote!-checksum + c>>;
   % Install updated file if needed.
      if !*home
	then <<if remote!-checksum =
		  (c := get!-checksum concat(lisp!-d,"/patches.fsl"))
		  or (remote!-checksum =
			get!-checksum concat(personal!-dir!*,"/patches.fsl"))
		 then <<terpri();
			printc "*** System is already up-to-date";
			close remote;
			if remote!-checksum = c
			 then rename!-home!-patch!-file();
			return nil>>;
	       write!-patch!-file(personal!-dir!*,p,remote,remote!-checksum)>>
       else if remote!-checksum =
	     get!-checksum concat(lisp!-d,"/patches.fsl")
	then <<terpri();
	       printc "*** System is up-to-date already";
	       close remote;
	       rename!-home!-patch!-file();
	       return nil>>
       else if not file!-writeablep lisp!-d
	then rederr list("Cannot write to",lisp!-d)
       else <<write!-patch!-file(lisp!-d,p,remote,remote!-checksum);
	      rename!-home!-patch!-file()>>;
   % Load new patch file;
    load!-patches!-file()
  end;

flag('(update!_reduce),'opfn);

endmodule;

end;
