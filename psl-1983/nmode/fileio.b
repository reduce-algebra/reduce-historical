    1    <    VISIT-FILE-COMMAND      PROMPT-FOR-DEFAULTED-FILENAME     	NMODE-CURRENT-BUFFER         VISIT-FILE         	INSERT-FILE-COMMAND     
INSERT-FILE-DEFAULT-FN       	PROMPT-FOR-FILE-NAME         INSERT-FILE-INTO-BUFFER      WRITE-FILE-COMMAND      	WRITE-BUFFER-TO-FILE         SAVE-FILE-COMMAND       OBJECT-GET-HANDLER      FASTAPPLY     WRITE-PROMPT       SAVE-FILE     MODIFIED?     SAVE-FILE-VERSION-COMMAND         SAVE-FILE-VERSION       FIND-FILE-COMMAND       FIND-FILE     WRITE-SCREEN-PHOTO-COMMAND        WRITE-SCREEN-PHOTO      	WRITE-REGION-COMMAND         	BUFFER-GET-POSITION     CURRENT-MARK       EXTRACT-REGION     TEXT-IO-DEFAULT-FN      WRITE-TEXT-TO-FILE      PREPEND-TO-FILE-COMMAND      	PREPEND-TEXT-TO-FILE         
APPEND-TO-FILE-COMMAND       	APPEND-TEXT-TO-FILE     	DELETE-FILE-COMMAND     NMODE-DELETE-FILE       DELETE-AND-EXPUNGE-FILE-COMMAND        NMODE-DELETE-AND-EXPUNGE-FILE     
UNDELETE-FILE-COMMAND        	NMODE-UNDELETE-FILE     
SAVE-ALL-FILES-COMMAND       NMODE-SELECTABLE-BUFFERS     NMODE-COMMAND-ARGUMENT-GIVEN      BLDMSG        NMODE-Y-OR-N?      NAME     FILE-NAME     PATHNAME-WITHOUT-VERSION     NAMESTRING         PROMPT-FOR-STRING       PATHNAME      PATHNAME-TYPE      ATTEMPT-TO-MERGE-PATHNAME-DEFAULTS     FASTBIND      *EMSGP        CATCHSETUP         THROWSIGNAL*       MERGE-PATHNAME-DEFAULTS      NCONS         %UNCATCH      FASTUNBIND         EMSG*         THROW         ABORT         $ERROR$       
READ-FILE-INTO-BUFFER        
PATHNAME-DEFAULT-MODE        BUFFER-SET-MODE         
ATTEMPT-TO-OPEN-INPUT        READ-STREAM-INTO-BUFFER      SET-MODIFIED?      RESET         SET-FILE-NAME      VISIBLE-SIZE       CLOSE         READ-FROM-STREAM        TEXT-MODE     BUFFER-CREATE-UNSELECTABLE        INSERT-BUFFER-INTO-BUFFER         SET-POSITION       	SET-MARK-FROM-POINT     INSERT-TEXT        CONTENTS      POSITION      WRITE-FILE         
ATTEMPT-TO-OPEN-OUTPUT       NMODE-ERROR        WRITE-TO-STREAM         	MOVE-TO-BUFFER-START         MOVE-TO-BUFFER-END      NMODE-YES-OR-NO?        	ACTUALIZE-FILE-NAME     	NMODE-CURRENT-WINDOW         	FIND-FILE-IN-WINDOW     	FIND-FILE-IN-BUFFER     	WINDOW-SELECT-BUFFER         FIND-FILE-IN-EXISTING-BUFFER      
FILENAME-TO-BUFFERNAME       BUFFER-MAKE-UNIQUE-NAME      
BUFFER-CREATE-DEFAULT        PATHNAMES-MATCH         FILE-DELETE        FILE-DELETE-AND-EXPUNGE      FILE-UNDELETE      NMODE-REFRESH      NMODE-SCREEN       FILE-NAMESTRING         STRING-UPCASE      PATHNAME-DEVICE         EQUAL         PATHNAME-DIRECTORY      PATHNAME-NAME      PATHNAME-VERSION        PATHNAME-HOST      LIST5         XCONS         MAKE-PATHNAME      HOST     DEVICE        DIRECTORY     TYPE     FLUID1        PUTENTRY      EXPR    R   ]    Visit File:                ,      +         Insert File:            , B     +        Write File:                ,      +        (No changes need to be written)         >     Bx      ,     x     ,2B   +      >+  >+    x     (No changes need to be written)         >     Bx     .,     x     ,2B   +   ,   ->+  >+     x     Find file:                 3, + 	   .    
Write Photo to File:                 9,+ 
   3    
Write Region to File:         >   , Bx  ,     x      , Bx      F,     D x     >+    9    Prepend Region to File:       >   , Bx  ,     x      , Bx      S,     D x     >+    F    
Append Region to File:        >   , Bx  ,     x      , Bx      `,     D x     >+    S    Delete File:               e, +    `    Delete and Expunge File:             k, +    e    Undelete File:             p, +    k    Save %w in %w (Y or N)?       >        Lx  6 0V  +   y x     +   y     B6 1V  +   |    +    B  ,        ,3B   +    B  ,        ,3B   +  2@+    B  ,        , B  B~  ,     ~   ,       ,,3B   +   , x      Bx  6 0V  +     +       B+   z>~,~   >x  >      px x x     >   Bx     2B   +  !  B  ),        ,3B   +  ',,     x  >+ x     >   Dx  ,, B3@x  +  1 x  ,         x   ,>+     >   Bx   D F H~    -4  
  L, B~2@+  > ~   x  ,, B} ~, }-4      B~ B}3B   +  B    +  B  *2B   +  F  *6 K1V  +  F    3B   +  H ~   +  J ,  L,>},~   >}x x     (New File)         >   Bx   D B  g,         , ,      x  ,  ,!3B   +  [     x  >~+ ! x   B~  f,     ~   , x   B~  f,         ~   ,  e>~+   Mx "x "x #    File read: %w (%d lines)     Reading file: %w        >   Bx   D    B  ,        , B      ,, x   B~  ,      ~   ,  B~  ,     ~   , x   B~  ,     ~   ,       ,>~+   gx #x $x $  jx     FOO      >   Bx   D %  ,% B , x   >~+ &      >   Bx   D    B  +,        , B  B~ x   B~  +,     ~   , B~  * ~,     ~ ~   ,  B~  *,     ~   ,  B~  ),      ~   >}+ x &x 'x 'x (x (     >   Bx   B  >,        , B x   B  >,        ,2B   +  7    +  =3@+  ; ,     x  >~+  x  >~+ )>~,~   x x      >   Bx   B  P,        , B x   B  P,        ,2B   +  J    +  O3@+  M  x  >~+  x  >~+ )>~,~   x x     Write Buffer %w to File:           ,>   ,>     ^,        ,      ], x  ,      x  >+   Qx     Unable to write file: %w     File written: %w (%d lines)       Writing file: %w             >   Bx   D  , , B,) B~3B   +   B~  ,     ~   , B~      ,, x   B}  ,     ~ }   , ~ B}  ,     }   , x   B}  ,     }   ,     ~  ,, x   B}  ,         }   , x   B}  ,     ~ }   >}+    ,>}+ *  ^x #x "  ax #x $x *  ex   g    FOO      >   Bx   D %   ,% B B~   ,     x   ~   ,  >~+ x '      FOO      >   Bx   D %  2,% B ,  B~  2,     ~   ,  B~  1,     x   ~   ,  >~+ x 'x +  !    FOO      >   Bx   D %  D,% B ,  B~  D,     ~   ,  B~  C,     x   ~   ,  >~+ x 'x +  3    Unable to read or create file: %w      Write out changes in %w?     buffer %w     file %w       >   Bx   D B  o,        ,3B   +  e x   B  n,        ,3B   +  \      n+  a x   B~  m,     ~   ,      m,      l,,,3B   +  e x  , ,,3B   +  i     x  >~+    l,>~+ *  E  I  Lx   Mx x           -+ -    Unable to read or create file: %w      ,>  ,>        ,.3B   +  {     x  >+ .   ~,>+ *  q    (New File)         >   D,, Bx  3B   +  ^  4$ ,/2B   +   x  ,! B2B   +  
2@+   x  ,/,0,0 B~ B~  ,     x   ~   , x  ,      ~, 3@+    ~,!+    , ~+      +      >},~     ~x #     >   Bx  , B      L6 40V  +  "    +  #     B~6 41V  +  &    +  3 ~ B~  5,     ~   ,     ,13B   +  - ~+  3     D6 40V  +  1   +  2     B~+  #>},~   >>  x     Unable to delete file: %w         File deleted: %w         >   Bx  ,1 B3B   +  A      E,,+  C x    E,,* >,~     5  9    Unable to delete file: %w         File deleted and expunged: %w      >   Bx  ,2 B3B   +  S      W,,+  U x    V,,* >,~     F  I    Unable to undelete file: %w       File undeleted: %w       >   Bx  ,2 B3B   +  c      g,,+  e x    g,,* >,~     W  [    Unable to write file: %w     File written: %w         >   Bx  ,) B3B   +   ,3 3 B  ,         ,  B  ,        ,  B  ,        ,      ,>~+  x    ,>~+ *  h  kx x $x *     >   Bx  ,! B2B   +   x  ,) B3B   +   B   ,        , Bx    B  ,        , x  ,2+      +   B   ,        , Bx    B  ,        , x  >~,~   x $x      ,,,4+ 4    >   D, Bx   , B x  ,5 B ,5     ,53B   +  H x  ,6 B ,6     ,53B   +  H x  ,6 B ,6     ,53B   +  H x  , B ,     ,53B   +  H x  ,73B   +  >    +  >  *2B   +  H ,73B   +  B    +  C  *2B   +  H x  ,7 B ,7     >~+ 5>~,~        >  , Bx  ,7 B x  ,5 B x  ,6 B~ x  ,6 B~ x  , 
     ] ~  \ ~,8  \,8 ,8  [,8 ,8  [,8>}+ 9x 9x :x :x x ;  R,;  Q,;  Q,;  P,;  P,;  O,;  O,;  N,;     B     DZ&    N  M,<Z&    N  M,<Z&    N  L,<Z&    N  L,<Z&  %  N  K,<Z&  1  N  K,<Z&  7  N  J,<Z&  =  N  J,<Z&  J  N  I,<Z&  W  N  I,<Z&  c  N  H,<Z&  i  N  H,<Z&  n  N  G,<Z&  t  N  G,<Z&   N  F,<Z& *  N  F,<Z& 2  N  E,<Z& O  N  E,<Z& m  N  D,<Z& 	  N  D,<Z&   N  C,<Z& ,  N  C,<Z& ?  N  B,<Z& U  N  B,<Z& i  N  A,<Z&   N  A,<Z& "  N  @,<Z& 4  N  @,<Z& O  N  ?,<Z& p  N  ?,<Z& v  N  >,<Z&   N  >,<Z&   N  =,<Z& <  N  =,<Z& M  N  <,<Z& ^  N  <,<Z& n  N  ;,<Z&   N  ;,<Z& !  N  :,<Z& #  N  :,<Z& J  N  9+ <x x 1x /x ,x 
x x x x /x .x -x 	x x x x x x )x x x &x x !x x x x  x x x x x x x x 
x 	x x x x x  x <x x x %x -x x 3x x     / E0T+ E`  "@E
@ 

^ , +  
D` @"Q0 (@, 0 V 
X  D AA@" EA 
"D~ PT`P" " * PD E
D*P  
 "D A@
|    
 P@ Q| 
A0 ( (" A ( _| @A`
 @A`  PQ ^      

A  AP(@(_ P(< 
A A | 
A A |      @D
PPQ(DD/a    D, Q@A*D D(D" `   T*p     EQ(@   "TT`    T( A D(_|@"D( @<U D A"D"" DAE"QQ?}*UU*
EE"Q(TT*
EE"Q(TT*
EE"Q(TT*
EE"Q(TT*
EE"Q+x  