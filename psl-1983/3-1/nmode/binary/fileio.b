       ?    VISIT-FILE-COMMAND T    PROMPT-FOR-DEFAULTED-FILENAME     	NMODE-CURRENT-BUFFER ERS     VISIT-FILE   FA    	INSERT-FILE-COMMAND     
INSERT-FILE-DEFAULT-FN GI    	PROMPT-FOR-FILE-NAME N LE    INSERT-FILE-INTO-BUFFER E    WRITE-FILE-COMMAND -    	WRITE-BUFFER-TO-FILE N  E    SAVE-FILE-COMMAND ED    OBJECT-GET-HANDLER      FASTAPPLY     WRITE-PROMPT -B    SAVE-FILE     MODIFIED?     SAVE-FILE-VERSION-COMMAND tten    SAVE-FILE-VERSION o     FIND-FILE-COMMAND ER    FIND-FILE     	WRITE-SCREEN-COMMAND FILE    WRITE-SCREEN-DEFAULT-FN I    WRITE-SCREEN DE    	WRITE-REGION-COMMAND FN E    	BUFFER-GET-POSITION     CURRENT-MARK SI    EXTRACT-REGION     TEXT-IO-DEFAULT-FN W    WRITE-TEXT-TO-FILE D    PREPEND-TO-FILE-COMMAND E    	PREPEND-TEXT-TO-FILE ND E    
APPEND-TO-FILE-COMMAND  E    	APPEND-TEXT-TO-FILE     	DELETE-FILE-COMMAND     NMODE-DELETE-FILE D     DELETE-AND-EXPUNGE-FILE-COMMAND        NMODE-DELETE-AND-EXPUNGE-FILE     
UNDELETE-FILE-COMMAND e:     	NMODE-UNDELETE-FILE     
SAVE-ALL-FILES-COMMAND LE    NMODE-SELECTABLE-BUFFERS     NMODE-COMMAND-ARGUMENT-GIVEN S    BLDMSG        NMODE-Y-OR-N? A    NAME     FILE-NAME     	PRINT-BUFFER-COMMAND )? I    NMODE-PRINT-DEVICE -    PROMPT-FOR-STRING  D    PRINT-BUFFER EV    FILENAME-WITHOUT-VERSION     MAYBE-PATHNAME     NMODE-ERROR ME     THROW         PATHNAME-TYPE      ATTEMPT-TO-MERGE-PATHNAME-DEFAULTS     NAMESTRING         ABORT d pa    FASTBIND      *EMSGP        CATCHSETUP         THROWSIGNAL*       MERGE-PATHNAME-DEFAULTS      NCONS         %UNCATCH      FASTUNBIND         EMSG*         $ERROR$       
READ-FILE-INTO-BUFFER S E    
PATHNAME-DEFAULT-MODE S E    BUFFER-SET-MODE UFFE    
ATTEMPT-TO-OPEN-INPUT S E    READ-STREAM-INTO-BUFFER E    SET-MODIFIED? T    RESET         SET-FILE-NAME -    VISIBLE-SIZE  (    CLOSE         READ-FROM-STREAM BUF    TEXT-MODE     
CREATE-UNNAMED-BUFFER R      INSERT-BUFFER-INTO-BUFFER DEFA    SET-POSITION -P    	SET-MARK-FROM-POINT     INSERT-TEXT R-I    CONTENTS X    POSITION O    WRITE-FILE THOU    
ATTEMPT-TO-OPEN-OUTPUT LE    WRITE-TO-STREAM  UTP    	MOVE-TO-BUFFER-START  %w     MOVE-TO-BUFFER-END E    NMODE-YES-OR-NO? ILE    	ACTUALIZE-FILE-NAME     	NMODE-CURRENT-WINDOW T-GI    	FIND-FILE-IN-WINDOW     	FIND-FILE-IN-BUFFER     	WINDOW-SELECT-BUFFER ate     FIND-FILE-IN-EXISTING-BUFFER :    
FILENAME-TO-BUFFERNAME  F    BUFFER-MAKE-UNIQUE-NAME F    
BUFFER-CREATE-DEFAULT BUF    PATHNAMES-MATCH -BUF    FILE-DELETE -FI    FILE-DELETE-AND-EXPUNGE -    FILE-UNDELETE -    NMODE-REFRESH W    SCREEN ME     OPEN     	CURRENT-BUFFER-SIZE     	CURRENT-BUFFER-FETCH  %w     PRINT-BUFFER-LINE E     CHANNELTERPRI      OUTPUT        CHANNELWRITECHAR        PATHNAME-WITHOUT-VERSION     FILE-NAMESTRING         STRING-UPCASE      PATHNAME      PATHNAME-DEVICE         EQUAL         PATHNAME-DIRECTORY      PATHNAME-NAME      PATHNAME-VERSION        FLUID1        PUTENTRY      EXPR     NMODE-SCREEN -B   *   (    Visit File:  FA            ,      +         Insert File:  E         , B     +        Write File: EFA            ,      +        (No changes need to be written)         >     Bx      ,     x     ,2B   +      >+  >+    <     (No changes need to be written)         >     Bx     .,     x     ,2B   +   ,   ->+  >+     <     Find file:  EFA            3, + 	   .    
Write Screen to File:   E      
   9, B
+    3    
Write Region to File: N E     >   , Bx  ,     x      , Bx      F,     D x     >+    :    Prepend Region to File: E     >   , Bx  ,     x      , Bx      S,     D x     >+    G    
Append Region to File:  E     >   , Bx  ,     x      , Bx      `,     D x     >+    T    Delete File: FA            e, +    a    Delete and Expunge File:             k, +    f    Undelete File:             p, +    l    Save %w in %w (Y or N)? I     >        Lx  6 0V  +   y x     +   z     B6 1V  +   }    +    B  ,        ,3B   +    B  ,        ,3B   +  2@+    B  ,        , B  B~  ,     ~   ,       ,,3B   +   , x      Bx  6 0V  +     +       B+   z>~,~   <x  <      q< < <     Print buffer to device: I        $, B+       >   Bx     2B   +  (  B  /,        ,3B   +  -,     x  >+ <     	Invalid pathname: %w ION     >   Dx  , B, B2B   +  :   A,,  A,3@x  +  ? x  ,         x   , B >~+ <   0    >   Bx   D F H~    -4  
  \, B~2@+  N ~   x  ,, B} ~,  }-4       B~ B}3B   +  R    +  R  *2B   +  V  *6 [1V  +  V    3B   +  X ~   +  Z !,  \,>},~   <}< < !    (New File) T  T    >   Bx   D B  w,         , ,"     x  ,# ,#3B   +  k     x  >~+ $ x   B~  v,     ~   , x   B~  v,         ~   ,  u>~+   ]< $< %< %    File read: %w (%d lines)     Reading file: %w BUF    >   Bx   D    B  ,        , B      ,, x   B~  ,      ~   ,  B~  ,     ~   , x   B~  ,     ~   ,       ,>~+   w< &< &< '  z<     >   Bx   D ',( B ," x   >~+ (    >   Bx   D    B  9,        , B  B~ x   B~  9,     ~   , B~  8 ~,     ~ ~   ,  B~  8,     ~   ,  B~  7,      ~   >}+ < )< )< *< *< +     >   Bx   B  L,        , B x   B  L,        ,2B   +  E    +  K3@+  I ,     x  >~+  x  >~+ +>~,~   < <      >   Bx   B  ^,        , B x   B  ^,        ,2B   +  X    +  ]3@+  [  x  >~+  x  >~+ +>~,~   < <     Write Buffer %w to File:  DEFA     ,>   ,>     l,        ,      k, x  ,      x  >+   _<     Unable to write file: %w     File written: %w (%d lines) E     Writing file: %w UTP RITE    >   Bx   D  ", , B,, B~3B   +   B~  ",     ~   , B~      !,, x   B}  !,     ~ }   , ~ B}   ,     }   , x   B}   ,     }   ,     ~  ,, x   B}  ,         }   , x   B}  ,     ~ }   >}+    ,>}+   l< %< $  o< &< &< ,  s<   u    >   Bx   D ',( B B~  ,,     x   ~   ,  >~+ < *    >   Bx   D ',( B ,"  B~  <,     ~   ,  B~  <,     x   ~   ,  >~+ < *< -    >   Bx   D ',( B ,"  B~  L,     ~   ,  B~  L,     x   ~   ,  >~+ < *< -    Unable to read or create file: %w      Write out changes in %w?     buffer %w     file %w E     >   Bx   D B  w,        ,3B   +  m x   B  v,        ,3B   +  d      v+  i x   B~  u,     ~   ,      u,      t,,.3B   +  m x  , ,.3B   +  q     x  >~+ "   t,>~+   M  Q  T<   U< <           /+ /    Unable to read or create file: %w      ,>  ,>        ,03B   +       x  >+ 0   ,>+   y    (New File) T NT    >   D,. Bx  3B   +  "   $ $bD $n %4$ ",12B   +  # x  ,# B2B   +  2@+  ! x  ,1,2,2 B~ B~  &,     x   ~   , x  ,"     ~,#3@+    ~,$+     %, ~+  "    +  #    >},~    x            < %     >   Bx  , B3B   +  >     B  L6 @0V  +  /    +  /     B~6 @0V  +  > ~ B~  A,     ~   ,     ,33B   +  9 ~+  ?     D6 @0V  +  =   +  =     B~+  0    >},~   <<  <     Unable to delete file: %w ER :    File deleted: %w  UF     >   Bx  ,3 B3B   +  M      Q,,+  O x    Q,, >,~     A  E    Unable to delete file: %w  %w     File deleted and expunged: %w      >   Bx  ,4 B3B   +  _      c,,+  a x    b,, >,~     R  U    Unable to undelete file: %w w     File undeleted: %w       >   Bx  ,4 B3B   +  o      s,,+  q x    s,, >,~     c  g    Unable to write file: %w     File written: %w DOW     >   Bx  ,, B3B   +   / B  ,        , B  ,        , B,5  B~  ,      ~   ,  B~  ,     ~   ,  B~  ,     ~   ,      ,>~+  x    ,>~+   t  w< < &< ,< 5    
Unable to write to %w %w      >   Bx    *-4  
  ;, B2@+  &  : x  ,6, B ,  -4       B6 91V  +  , x    :,,+  8    B,6>     B~    @@    B~ D} ~3l}+  3    +  7 ~,7     ,7 ,8: ~+  0 ,&>},~   <     < 8< !    >   Bx   D@@     $ XbD Yn Y@@    B D~ F~ 3l~+  D    +  W  d"p :    l"  d   "   B}0B  +  MZ$   x  ,9: ~ ~A"  4B V+  I1"  +  O0B  ?+  TZ$  / x  ,9 }F$    x  ,9Z.  .N~+  V     x  ,9: ~: +  B>},~    x               >   Bx  ,# B2B   +  j x  ,, B3B   +  i B  t,        , Bx    B  t,        , x  ,4+  r    +  s B  t,        , Bx    B  t,        , x  >~,~   < &<      ,>   ,3B   +  y,9,:+  y x  >+ :    >   D,; Bx   ,; B x  ,; B ,;     ,<3B   +    x  ,< B ,<     ,<3B   +    x  ,= B ,=     ,<3B   +    x  , B ,     ,<3B   +    x  ,=3B   +      +    *2B   +    ,=3B   +      +    *2B   +    x  ,= B ,=     >~+ <>~,~        ,>   ,3B   +  %,9>+  x  >,~       PRINTER: N  ),>  ),>  (,>  (,>  ',>  ',>  &,>  &,>  %,>  %,>  $ L     B     D     F
Z&    $  #,>Z&    $  #,>Z&    $  ",>Z&    $  ",>Z&  %  $  !,>Z&  1  $  !,>Z&  7  $   ,>Z&  =  $   ,>Z&  J  $  ,>Z&  W  $  ,>Z&  c  $  ,>Z&  i  $  ,>Z&  n  $  ,>Z&  t  $  ,>Z& "  $  ,>Z& %  $  ,>Z& 3  $  ,>Z& B  $  ,>Z& _  $  ,>Z& }  $  ,>Z&   $  ,>Z&   $  ,>Z& :  $  ,>Z& M  $  ,>Z& c  $  ,>Z& w  $  ,>Z& #  $  ,>Z& -  $  ,>Z& =  $  ,>Z& W  $  ,>Z& x  $  ,>Z& ~  $  ,>Z& 	  $  ,>Z& '  $  ,>Z& H  $  ,>Z& Y  $  ,>Z& j  $  ,>Z& z  $  ,>Z&   $  ,>Z& <  $  ,>Z& Z  $  ,>Z& u  $  ,>Z& {  $  ,>Z& !  $  + >< < 3< 1< .< 7< < < < < < 1< 0< /< 	< < < < < < +< < < (< < $< "< < <  < < < < < < < < < 
< 	< < < < <  < ?  &< 
< < < < '< /< < ?< <     4 E0T+ E`  "@E
@ 

^ , *` @"Q X T@,  D* ,  U V  "PA P(@A(D @ U, (P   (D ^ T( D
U    DD
@/x    A 
 A"xA @  P@A ?x @(D" (@D"   Dx      DP ( P
 Q~ PP0
@`
 @      (A 

D@"E~   @"E@ A
T"E PD
Q D" A D @    A
Q(    *E     A"P   E
@ P"/  *AU  "D"|     @(D @ @"DP @x"T AA"D"
" (@ UU*U*UT*
E"QQ(T*
E"QQ(T*
E"QQ(T*
E"QQ(T*
E"QW@   