@Device(lpt)
@style(justification yes)
@style(linewidth 80, spacing 1,indent 5)
@use(Bibliography "<griss.docs>mtlisp.bib")
@make(article)
@modify(enumerate,numbered=<@a. @,@i. >, spread 1)
@modify(appendix,numbered=<APPENDIX @A: >)
@modify(itemize,spread 1)
@modify(description,leftmargin +2.0 inch,indent -2.0 inch)
@define(up,use text,capitalized on,  break off)
@define(mac,use text, underline off,  break off)
@define(LISPmac,use text, underline alphanumerics,  break off)
@pageheading(Left  "Utah Symbolic Computation Group",
             Right "September 1981", 
             Line "Operating Note 59"
            )
@set(page=1)
@newpage()
@begin(titlepage)
@begin(titlebox)
@b(PictureRLISP)

@center[A LISP-Based Graphics Language System
with Flexible Syntax
and Hierarchical Data Structure

by

Fuh-Meei Chen, Paul R. Stay and  Martin L. Griss
Computer Science Department
University of Utah
Salt Lake City, Utah  84112

Last Revision: @value(date)]
@end(titlebox)
@begin(abstract)
This report is a description and a users manual for PictureRLISP, a
LISP based interactive graphics language.  PictureRLISP has an
ALGOL-like syntax, with primitives to create, manipulate and apply 3D
transformations to hierachical data structures called "Models".
PictureRLISP is entirely written in RLISP which is a high-level
interface to Standard LISP.
@end(Abstract)
@begin(Researchcredit)
Work supported in part by the National Science Foundation
under Grant No. MCS80-07034.
@end(Researchcredit)
@end(titlepage)
@pageheading(Left "PictureRLISP",Center "@value(date)",
             Right "@value(Page)"
            )
@set(page=1)
@newpage
@section<Introduction>
PictureRLISP is a graphic specification language in an interactive
RLISP environment.  PictureRLISP usage typically consists of creating,
modifying, and requesting the display of graphical objects, called
"Models".  A model is a three dimensional representation of the
spatial, topological and graphical features of an object.  Models can
contain any number of primitives, which can generally be in any order.

The hierarchical structure and implementation of the PictureRLISP
system are designed to support both the beginning and the expert user
as well.  The sophisticated PictureRLISP user can utilize low level
primitive operations to support customized modeling, syntax or device
environments; yet the beginner need not know how to use these
features.

PictureRLISP is a re-implementation of an earlier system,
PICTUREBALM@cite[Goates80], with a number of additions. The major
improvement is that the entire system is now written in RLISP, including
the low-level clipping and transformation routines. RLISP is an ALGOL-like
interface to LISP, found more convenient to use by many people. The
extensible, table-driven RLISP parser itself is written in LISP, permitting
rapid syntactice customization.  The version of RLISP used for PictureRLISP
is built upon PSL@cite[Griss81,Griss82b], an efficient, portable and
interactive LISP system. PSL provides rich data structures, dynamic storage
management, and an efficient LISP to machine code compiler@cite[Griss79b],
which makes PSL-based PictureRLISP much more efficient than the previous
PictureBALM system. A complete PSL currently runs on DECSystem-20,
VAX-11/750 under UNIX.  A preliminary PSL now runs on an Apollo DOMAIN (a
Motorola MC68000-based personal machine with high-resolution graphics).

PictureRLISP is capable of driving a number of different graphic output
devices, and is fairly easy to extend to others. The current devices that
built-in PictureRLISP drivers support include: Tektronix 4010 (and 'clones,
such as ADM3a with retrographics board, Apollo Tektronix emulator,etc.);
Hewlett-Packard HP2648a; Evans and Sutherland MPS-1; AED-512 color
terminal; and "checkout" graphics on low-resolution devices such as 60 x 80
Ann-Arbor Ambassador, or 24 x 80 Teleray-1061 or VT100.  

PictureRLISP has also been extended to run under EMODE@cite[Galway82], an
interactive LISP-based, full-screen editor which is similar to EMACS. EMODE
runs within the PSL environment, and permits the editing of PictureRLISP
commands and procedures, and then immediate execution from within the
editing window.  One can also define graphics windows to display the models
presented.

@section(Basic concepts)
@subsection(Models)
PictureRLISP usage typically consists of creating, modifying, and
requesting the display of graphical objects, called "Models".  A Model
is a three dimensional representation of the spatial, topological and
graphical features of an object. Models can contain any number of
primitives, which can generally be in any order.  PictureRLISP Model
primitives include: Point Sets, which might be interpreted as
polygons, connected line segments, curve control points, etc.;
transformations of objects or coordinate systems in three dimensional
space; color or appearance attributes; Repeat Specifications, which
cause sub-sections of the Model to be replicated; named references to
other Models to be displayed as if they were part of the current
Model; and procedure calls. 


	Allowing Models to contain references to other Models
facilitates dynamic displays and allows the user to structure his data
in Clusters in a meaningful manner.  Sub-Models may be shared among a
number of Models.  Allowing procedure calls to be imbedded within
Models provides the user with a mechanism which can easily effect
arbitrary displays, transformations, parameterized models or other
functions that may be required by a specific application; in some
cases, it is essential to represent objects by algorithms or
procedural models.
@subsection<Coordinate systems, Viewport>

 [ *** This section needs more work ****]

Currently, each device supported by has its own "screen" coordinates,
and the user has to think of his model sizes in a device specific
fashion. This is a defect, and we are planning to change the basic system
so that each device driver will normalize coordiates so that a square
of side N world-coordinates (or M inches?) will map onto the physical
screen, with a square aspect ratio. Clipping of objects outside this square
(cube) and exact placement of the square will be controlled by default
settings of the View Port and a Global transformation matrix.
Since both view port and global transformation (for perspective and scaling)
are adjustable, the idea will be to provide a more natural default.
Perhaps two or three sets of defualts are desirable, selectable by the user: 
A device independant WORLD view, a semi-device independant PHYSICAL size
and a very device specific SCREEN view.

@subsection<Example of PictureRLISP>
As a small example of the flavor of PictureRLISP, the following
commands will display a set of BOX's of different sizes, after suitable
device initialization:
@begin(verbatim)
BOX := {0,0}_{0,10}_{10,10}_{10,0}_{0,0}; 
	% Assigns to BOX a set of connected points for 10*10 box
SHOW BOX & BOX | ZROT(45) & BOX | SCALE(2);
        % Display 3 boxes, the original, a rotated box, and
        % a 20 * 20 box. The & collects a set of unconnected models
        % and | attaches a transformation (matrix)
@end(verbatim)

@section(Specification of the PictureRLISP Language)
PictureRLISP supports the creation and manipulation of Models both by
means of built-in procedures for the various primitives (points,
pointsets, and groups) and by means of syntactic extensions, i.e.
operators which construct Models out of primitives. PictureRLISP
contains five operators designed to make graphics programs easy to
read and write. They are denoted by the following special characters:
{, }, _, & and |, and map to an appropriate set of Lisp procedures.

The following is the set of legal Model primitives: 
@begin(enumerate)

@u(Point.)  Points are constructed by using curly brackets, or by the
function POINT(x,y,z,w), e.g.  {x,y} [denotes the point (x, y, 0) in three
dimensional space]. Points can be described by any one of four ways. A
single value on the x axis, a two dimensional point, a three
dimensional point or in homogeneous coordinate space.

@u(Pointset.)  The function POINTSET(p,q,..s) or the infix "_" operator is
used to make Point Sets; e.g. it can be used to make polygons out of
Points.  For example, the usual graphical interpretation of the
sequence A@ _@ B@ _@ C, where A, B, and C are Points, moves the
display beam to the point represented by A, draws to B, and then draws
to C.

@u(Group) A Group is a set of Point Sets or Points and is formed by
the infix operator & or the function GROUP(ps1,ps2,...psN). Thus models may be
grouped together and formed into larger models for reference.

@u(Point Set Modifiers.)  Point Set Modifiers alter the interpretation
of any Point Sets within their scope.  The curved Point Set Modifier
BEZIER() causes the points to be interpreted as the specification
points for a BEZIER curve. The BEZIER curve has as its end points the
endpoints of the control polygon. BSPLINE() does the same for a closed
Bspline curve.  If a control polygon is not closed then then algorithm
will create a closed polygon by assuming there is a line segment
between the endpoints. In order to get these curves a pointset acting
as control points need to be given. Even though the control points may
not be closed for a BSPLINE curve the system will close the polygon to
form a closed BSPLINE curve. Another modifier is that of COLOR() where
on color drawing systems different color values can be given to the
model.

@u(Transforms.)
Transforms are the Model primitives which correspond to
transformations of objects or coordinate systems in three dimensional
space. PictureRLISP supports rotation, translation, scaling,  perspective
transformation and clipping. The Transform primitives are: 
@begin<enumerate>
Translation:  Move the specified amount along the 
              specified axis.
@*XMOVE (deltaX) ; YMOVE (deltaY) ; ZMOVE (deltaZ)
@*MOVE (deltaX, deltaY, deltaZ)
@blankspace(1 line)
These Transforms are implemented as procedures which return a transformation
matrix as their value.

Scale : Scale the Model SCALE (factor)
@*XSCALE (factor) ; YSCALE (factor) ; ZSCALE (factor)
@*SCALE1 (x.scale.factor, y.scale.factor, z.scale.factor)
@*SCALE <Scale factor>.  Scale along all axes.
@blankspace(1 line)
These Transforms are implemented as a transformation matrix which will scale 
Models by the specified factors, either uniformly or along only one dimension.

Rotation: Rotate the Model
@*ROT (degrees) ; ROT (degrees, point.specifying.axis)
@*XROT (degrees) ; YROT (degrees) ; ZROT (degrees)
@blankspace(1 line)
These procedures return a matrix which will rotate Models about the axis
specified. Currently rotation are limited to being about the three 
coordinate axes, though one would like to be able to specify an arbitrary
rotation axis.

WINDOW (z.eye,z.screen): The WINDOW primitive assumes that the viewer
is located along the z axis looking in the positive z direction, and
that the viewing window is to be centered on both the x and y axis.
The window function is used to show perspective for models and the
default window at initialization of the device is set with the eye at
-300 and with the screen at 60.  If one wish to use a right handed
coordinate system then the eye is in the positive direction.

VWPORT(leftclip,rightclip,topclip,bottomclip): The VWPORT, which specifies
the region of the screen which is used for display. This is set to a
convenient default at the time a device is initialized by the device
drivers.
@end<enumerate>

@u(Repeat Specifications.)
This primitive provides the user with a means of replicating a
section of a Model any number of times as modified by an arbitrary
Transform, e.g. in different positions.
The primitive is called REPEATED (number.of.times, my.transform),
where number.of.times is an integer.
The section of the Model which is contained within the scope of the Repeat
Specification is replicated.
Note that REPEATED is intended to duplicate a sub-image in several different
places on the screen; it was not designed for animation.

@u(Identifiers of other Models.)
When an identifier is encountered, the Model referenced is displayed
as if it were part of the current Model.  Allowing Models to contain
identifiers of other Models greatly facilitates dynamic displays.

@u(Calls to PictureRLISP Procedures.)
This Model primitive allows procedure calls to be imbedded within
Models.  When the Model interpreter reaches the procedure identifier
it calls it, passing it the portion of the Model below the procedure
as an argument.  The current transformation matrix and the current pen
position are available to such procedures as the values of the global
identifiers GLOBAL!.TRANSFORM and HEREPOINT.  This primitive provides
the user with a mechanism which can be used to easily effect arbitrary
displays, transformations, functions or models required by a specific
application.  The value of the procedure upon its return is assumed to
be a legal Model and is SHOW'n; PictureRLISP uses syntax to
distinguish between calling a procedure at Model-building time and
imbedding the procedure in the Model to be called at SHOW time; if
normal procedure call syntax, i.e. proc.name@ (parameters), is used
then the procedure is called at Model-building time, but if only the
procedure's identifier is used then the procedure is imbedded in the
Model.

@u(Global Variables) There are a number of important global variables
in PictureRLISP whose meaning should be aware of, and which should be
avoided by the user, unless understood:

@begin<description>

@u<Globals>@\@u<Meaning>

HEREPOINT@\Current cursor position as a 4-vector.

HERE@\Current cursor position as a '(POINT x y z)

ORIGIN@\The vector  [0,0,0,1].

GLOBAL!.TRANSFORM@\A global transform specified by the user,
which is applied to everything as the "last" transformation.
A default is set in the Device initializtion, but can be changed by
user as convenient.

MAT!*1@\Unit 4 x 4 transformation matrix.

MAT!*0@\Zero 4 x 4 transformation matrix.

DEV!.@\Name of the current device, for device dependent code.

CURRENT!.TRANSFORM@\The current (cumulative) transformation matrix.
All points  are transformed by this before a move
or draw.  Initialized to GLOBAL!.TRANSFORM before each Display.

CURRENT!.LINE@\The current Pointset modifier, can be 'BEZIER,
'BSPLINE or the default straight line modifier 'LINE.

!*EMODE@\Tells the system and or user if PictureRlisp is
in EMODE status.
@end(description)
@end(enumerate)
@newpage
The following is a BNF-like description of the set of legal Models.
The meta-symbols used are ::= for "is a" and | for "or".
Capitalized tokens are non-terminal symbols of the grammar of Models,
a usage that is adhered to in the text of this report.
Upper case tokens are PictureRLISP reserved words, which have been defined
as RLISP procedures, operators and/or macros.
Lower case tokens can  be either numbers or identifiers, but not
quoted number identifiers,
except for "string" which denotes either a RLISP item of type string
or a string identifier.
@begin(verbatim)
<Model>                  ::=      NIL
                              |   <Simple Model>
                              |   <Model>  &  <Model>

<Simple Model>                |   <Model Object>
                              |   ( <Model> )
                              |   <Model> | <Model Modifier>
                              |   <Model Identifier>
                              |  '<Model Identifier>


<Model Object>           ::=      NIL
                              |   <Point Set>
                              |   <Model Object Identifier>
                              |  '<Model Object Identifier>

<Model Modifier>         ::=      NIL
                             |   <Transform>
                             |   <Point Set Modifier>
                            
                            
<Transform>              ::= XROT (degrees)
                            |   YROT (degrees) | ZROT (degrees)
                            |   XMOVE (deltaX) | YMOVE (deltaY)
                            |   ZMOVE (deltaZ)
                            |   MOVE (xdelta, ydelta, zdelta)
                            |   SCALE (factor) | XSCALE (factor)
                            |   YSCALE (factor)| ZSCALE(factor)
                            |   SCALE (x.factor, y.factor, z.factor)
                            |   WINDOW (z.eye,z.screen)
                            |   <Transform Identifier>
                            | ' <Transform Identifier>


Repeat Specification   ::=    REPEATED (number!.of!.times, Transform)

<Point Set Modifier>   ::=  |   BEZIER()
                            |   BSPLINE()
                            |   CIRCLE(r)
			    |   COLOR(value)
                            
<Point Set>            ::=      <Point>
                            |   <Point>  _  <Point Set>
                            |   <Point Set Identifier>
                            |  '<Point Set Identifier>

<Point>                ::=      {x} |  {x, y}   |   {x, y, z} 
			    |   {x,y,z,w}
                            |   Point Identifier
                            | ' Point Identifier

@end(verbatim)
@section<Basic PictureRLISP Procedures>
It should be emphasized that the typical user of the PictureRLISP
language need never use some of these primitives directly, nor need he
even know of their existence.  They are called by the procedures which
are written in RLISP which implement the standard PictureRLISP user
functions.  Nevertheless, they are available for the sophisticated
user who can utilize them to implement a customized language
environment.  Also, they might serve as an example of the primitives
that a PictureRLISP implementor would want to add to support other
devices.
@subsection(Common Functions)
@begin<description>
@b<ERASE()>@\Clears the screen and leaves the
cursor at the origin.


@b<SHOW (pict)>@\Takes a picture and display it on the screen

@b<ESHOW (pict)>@\Erases the whole screen and display "pict"

@b<HP!.INIT()>@\Initializes the operating system's (TOPS-20) view 
of the characteristics of HP2648A terminal.

@b<TEK!.INIT()>@\Initializes the operating system's (TOPS-20) view
of the characteristics of TEKTRONIX 4006-1 terminal and
also ADM-3A with Retrographics board.

@b<TEL!.INIT()>@\Initializes the operating system's (TOPS-20) view
of the graphics characteristics of the Teleray 1061 terminal.
This is rather crude graphics, on a 24*80 grid, using the character X.
Nevertheless, it provides a reasonable preview.

@b<MPS!.INIT()>@\Initializes the operating system's (UNIX) on the vax
 to handle the MPS commands. (currently on the VAX).

@b<ST!.INIT()>@\Initializes the operating system's view of the
characteristics of the Apollo workstation (a 68000 based system hooked
up to the DEC 20 or Vax), emulating a TekTronix 4006 and VT-52
simultaneously in multiple windows.

@b<AED!.INIT()>@\Initializes the operating system's view of the
graphics color device AED-512 a 4006 tektronix color system.

@end(Description)

@subsection(Low Level Driver Functions)
Most of these are "generic" names for the device specific procedures
to do basic drawing, moving, erasing etc. The initialization routine for device XX,
called XX!.INIT() above, copies the routines, usually called XX!.YYYY into
the generic names YYYYY.
@begin(description)

@b<ERASES()>@\Erase the Graphics Screen

@B<GRAPHON()>@\Called by SHOW, ESHOW and ERASE() to put the device into
graphics mode. May have to turn off normal terminal ECHO, using ECHOOFF(),
unless running under EMODE.

@b<GRAPHOFF()>@\Called by SHOW, ESHOW and ERASE() to put the device back
into text mode. May have to turn  normal terminal ECHO back on, using ECHOON(),
unless running under EMODE.


@b<MOVES (x, y)>@\Moves the graphics cursor to the point (x, y) where
x and y are specified in coordinates.  These coordinates will be
converted to absolute location on the screen allowing different
devices to display the same models whether they have the same
coordinate systems internaly or not.

@b<DRAWS (x, y)>@\Draws a line from the current cursor position to the
point specified in screen space.

@end(description)
@subsection(Low Level Matrix Operations)
@begin(description)
@b<MAT!*MAT (new!.transform, current!.transform)>@\This procedure is passed
two transformation matrices.  Each matrix is represented by a 16 element
vector of floating point or interger numbers. They are concatenated via
matrix multiplication and returned as the new value of current transform.

@b<PNT!*PNT(point!.1,point!.2)>@\This procedure is passed two 4-vector
matrices, a value is returned.

@b<PNT!*MAT(point,transformation)>@\This is passed 4-vector and a 4 by
4 matrix, and returns a new (transformed) point.
@end<description>
@section<Internal Representations of PictureRLISP Graphical Objects>
In the LISP-like internal form, Points and Transforms are
represented by 4 vectors (homogeneous coordinates, also assuming the model
has been placed on w=1.0 plane) and 16 element vectors respectively.
Other Model primitives are represented as operators in LISP S-expressions
of the form "(operator arg1 arg2... argN)".
Points and matrices can also be represented as S-expression operators, if
this is desirable for increased flexibility.

It will be helpful for the PictureRLISP user to know what the
meaning of the interpreted form is in terms of the PictureRLISP
parsed form. The operator is some meaningful token, such as POINT,
TRANSFORM, POINTSET or GROUP; e.g. GROUP is the representation of the user
level operator "&".  The operator is used as a software interpreter
label, which makes this implementation of a PictureRLISP interpreter
easy to extend.  Here is the table to show the external and corresponding 
internal forms for some basic PictureRLISP operators.

@begin <verbatim>
@u[Internal Form]             @u[External Form]       @u[Result on Draw]

(POINT x y z )               {x,y,z}            [x,y,z,w]

(POINTSET a b c d)           a_b_c_d          move to a, then 
                                              connect b, c, and d.

(GROUP (pointset a b       a_b_c_d & e        do each pointset in 
          c d) e)                             turn.

(TRANSFORM f g)              f | g            apply the transform
                                              g to the picture f.

(TRANSFORM point              point |         draws a circle with 
 (CIRCLE radius))          CIRCLE(radius)     radius specified about 
                                              the center "point".

(TRANSFORM pict                pict |         draws Bezier curve for
   (BEZIER)                   BEZIER()        "pict".

(TRANSFORM pict                pict |         same as (pict |BEZIER())
   (BSPLINE)                  BSPLINE()       but drawing Bspline curve.

(TRANSFORM pict         pict | REPEATED       the "pict" is replicated
  (REPEATED                 (count,trans)     "count" times as modified 
   count trans ))                             by the specified transform
                                              "trans".   

For example, the Model
@end<verbatim>
@begin(display)
(A _ B _ C  &  {1,2} _ B)  |  XROT (30)  |  'TRAN ;

maps to the LISP form:

        (TRANSFORM
            (TRANSFORM
                (GROUP (POINTSET A B C) (POINTSET (POINT 1 2) B))
             (XROT 30))
            (QUOTE TRAN))
@end(display)

These structures give a natural hierachical  structure as well as
scope rules to PictureRLISP.

@section<How to run PictureRLISP>
Models can be built using any number of primitives and transformations
and assigned to model ID's.  Once a model is defined and the device
has been choosen then the object can be drawn on the graphics device
by using the commands Show and Eshow, both of which will display the
model or object on the graphics device and the difference being that
Eshow will first erase the screen. To erase the screen one can issue
the command Erase() and all models and object will be erased from the
screen. Unfortunately one cannot erase individual objects from the
display device. The following section will give an idea on other
aspects of running PictureRLISP by example. 

@section<Examples of PictureRLISP Commands>
In the following examples, anything following a % on the same line is
a comment.  Rlisp expressions (or commands) are terminated with a
semicolon. It is suggested that you execute these examples while
executing PictureRLISP at one of the terminals to see the correct
response one would get. Most of these are located in the file
<stay.pict>exp.red on the DecSystem 20 at Utah and is supplied with the
release of PictureRLISP.

@begin(verbatim)
%
% PictureRLISP Commands to SHOW lots of Cubes 
% 
% Outline is a Point Set defining the 20 by 20 
%   square which will be part of the Cubeface
%
Outline := { 10, 10} _ {-10, 10} _
          {-10,-10} _ { 10,-10} _ {10, 10};

% Cubeface will also have an Arrow on it
%
Arrow := {0,-1} _ {0,2}  &  {-1,1} _ {0,2} _ {1,1};

% We are ready for the Cubeface

Cubeface   :=   (Outline & Arrow)  |  'Tranz;

% Note the use of static clustering to keep objects
%  meaningful as well as the quoted Cluster
%  to the as yet undefined transformation Tranz,
%  which will result in its evaluation being
%  deferred until SHOW time

% and now define the Cube

Cube   :=   Cubeface   
        &  Cubeface | XROT (180)  % 180 degrees
        &  Cubeface | YROT ( 90)
        &  Cubeface | YROT (-90)
        &  Cubeface | XROT ( 90)
        &  Cubeface | XROT (-90);
% In order to have a more pleasant look at 
% the picture shown on the screen we magnify
% cube by 5 times.
BigCube := Cube | SCALE 5;

% Set up initial Z Transform for each cube face
%
Tranz   :=   ZMOVE (10);  % 10 units out

% Now draw cube
%

SHOW  BigCube;
@blankspace(4 inches)
% Draw it again rotated and moved left
%
SHOW  (BigCube | XROT 20 | YROT 30 | ZROT 10);
@blankspace(4 inches)
% Dynamically expand the faces out 
%
Tranz   :=   ZMOVE 12;
%
SHOW  (BigCube | YROT 30 | ZROT 10);
@blankspace(4inches)
% Now show 5 cubes, each moved further right by 80
%
Tranz   :=    ZMOVE 10;
%
SHOW (Cube | SCALE 2.5 | XMOVE (-240) | REPEATED(5, XMOVE 80));
@blankspace(4 inches)
%
% Now try pointset modifier.
% Given a pointset (polygon) as control points either a BEZIER or a
% BSPLINE curve can be drawn.
%
Cpts := {0,0} _ {70,-60} _ {189,-69} _ {206,33} _ {145,130} _ {48,130}
       _ {0,84} $
%
% Now draw Bezier curve
% Show the polygon and the Bezier curve
%
SHOW (Cpts & Cpts | BEZIER());
@blankspace(4 inches)
% Now draw Bspline curve
% Show the polygon and the Bspline curve
%
SHOW (Cpts & Cpts | BSPLINE());
@blankspace(4inches)
% Now work on the Circle
% Given a center position and a radius a circle will be drawn
%
SHOW ( {10,10} | CIRCLE(50));
@blankspace(3inches)

% Define a procedure which returns a model of
% a Cube when passed the face to be used
%
Symbolic Procedure Buildcube;
 List 'Buildcube;

% put the name onto the property list

Put('buildcube, 'pbintrp, 'Dobuildcube); 
Symbolic Procedure Dobuildcube Face$
       Face  &  Face | XROT(180)
             &  Face | YROT(90)
             &  Face | YROT(-90)
             &  Face | XROT(90)
             &  Face | XROT(-90) ;
% just return the value of the one statement


% Use this procedure to display 2 cubes, with and
%  without the Arrow - first do it by calling
%  Buildcube at time the Model is built
%

P := Cubeface | Buildcube() | XMOVE(-15) &
     (Outline | 'Tranz) | Buildcube() | XMOVE 15;
%

SHOW (P | SCALE 5);
@blankspace(4inches)
% Now define a procedure which returns a Model of
%   a cube when passed the half size parameter

Symbolic Procedure CubeModel;
 List 'CubeModel;

%put the name onto the property list

Put('CubeModel,'Pbintrp, 'DoCubeModel);
Symbolic Procedure DoCubeModel  HSize;
 << if idp HSize then HSize := eval HSize$
    { HSize,  HSize,  HSize}  _
    {-HSize,  HSize,  HSize}  _
    {-HSize, -HSize,  HSize}  _  
    { HSize, -HSize,  HSize}  _
    { HSize,  HSize,  HSize}  _  
    { HSize,  HSize, -HSize}  _
    {-HSize,  HSize, -HSize}  _  
    {-HSize, -HSize, -HSize}  _
    { HSize, -HSize, -HSize}  _  
    { HSize,  HSize, -HSize}  &
    {-HSize,  HSize, -HSize}  _  
    {-HSize,  HSize,  HSize}  &
    {-HSize, -HSize, -HSize}  _  
    {-HSize, -HSize,  HSize}  &
    { HSize, -HSize, -HSize}  _  
    { HSize, -HSize,  HSize} >>;


% Imbed the parameterized cube in some Models
%
His!.cube :=  'His!.size | CubeModel();
Her!.cube :=  'Her!.size | CubeModel();
R  :=  His!.cube | XMOVE (60)  &
      Her!.cube | XMOVE (-60) ;

% Set up some sizes and SHOW them

His!.size := 50;
Her!.size := 30;
%
SHOW   R ;
@blankspace(4inches)
%
% Set up some different sizes and SHOW them again
%
His!.size := 35;
Her!.size := 60;
%
SHOW R;
@blankspace(4inches)
@end<verbatim>

@section<How to run PictureRLISP on the various devices>
The current version of PictureRLISP runs on a number of devices at the
University of Utah. PictureRLISP source is in PU:PRLISP.RED,
and the device driver library is in the file
PU:PRLISP-DRIVERS.RED. These files, compiled into the binary LOAD form
are  PRLISP-1.B and PRLISP-2.B. Both are automatically loaded if
the user invokes LOAD PRLISP; from PSL:RLISP
(see PSL documentation for implementation and usage of the loader). The
following contains information concerning the generic form of a device
driver, and the execution of PictureRLISP under PSL. PictureRLISP is such
that device drivers can be written for what ever device you are using for a
graphics display device.  

@subsection<Generic Device Driver>

The following is an example of an xxx device driver and its associated
routines. The main routines of the driver may be divided into three
areas: low level I/O, basic graphics primitives (eg. move, draw,
viewport etc.), and the setup routine. 
@begin(verbatim)
		%***************************
		%  setup functions for     *
		%  terminal devices        *
		%***************************

% FNCOPY(NewName,OldName) is used to copy equivalent  a
% device specific function (e.g. xxx-Draws) into the generic
% procedure name

      % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      %          xxx specific Procedures            %
      % %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% device low level routines to drive the escape sequences for
% a graphics device. These output procedures will send the various
% codes to the device to perform the desired generic function

Procedure xxx!.OutChar x;	%. RawTerminal I/o
  Pbout x;

Procedure xxx!.EraseS();           %. EraseS screen, Returns terminal 
  <<xxx!.OutChar Char ESC;         %. to Alpha mode and places cursor.
    xxx!.OutChar Char FF>>;
% The following procedures are used to simulate the tektronix
% interface for picturerlisp and are considered the graphics
% primitives to emulate the system.


Procedure xxx!.4BYTES (XDEST, YDEST)$    %. Convert graphic plot 
<< xxx!.OutChar HIGHERY NormY YDEST$     %. information to the
   xxx!.OutChar LOWERY NormY YDEST$      %. terminal in a 4 byte 
   xxx!.OutChar HIGHERX NormX XDEST$     %. sequences containing the 
   xxx!.OutChar LOWERX NormX XDEST >>$   %. High and Low order Y 
                                         %. informationand High and
                                         %. Low order X information.

Procedure HIGHERY YDEST$            %. convert Y to higher order Y.
FIX(YDEST) / 32 + 32$

Procedure LOWERY YDEST$             %. convert Y to lower order Y.  
  REMAINDER (FIX YDEST,32) + 96$


Procedure HIGHERX XDEST$            %. convert X to higher order X.
  FIX(XDEST) / 32 + 32$

Procedure LOWERX XDEST$             %. convert X to lower order X.  
  REMAINDER (FIX XDEST,32) + 64$


Procedure xxx!.MoveS(XDEST,YDEST)$ 
  <<xxx!.OutChar 29 $                     %. GS: sets terminal to Graphic mode.
    xxx!.4BYTES (XDEST,YDEST)$
    xxx!.OutChar 31>> $                   %. US: sets terminal to Alpha mode.

Procedure xxx!.DrawS (XDEST,YDEST)$    %. Same as xxx!.MoveS but 
<< xxx!.OutChar 29$                                %. draw the line.
   xxx!.4BYTES (CAR2 HERE, CAR3 HERE)$
   xxx!.4BYTES (XDEST, YDEST)$
   xxx!.OutChar 31>> $

Procedure xxx!.NormX DESTX$               %. absolute location along
 DESTX + 512$                                      %. X axis.

Procedure xxx!.NormY DESTY$               %. absolute location along 
 DESTY + 390$                                      %. Y axis.

Procedure xxx!.VWPORT(X1,X2,Y1,Y2)$       %. set the viewport for
 <<  X1CLIP := MAX2 (-512,X1)$            %. the display device
     X2CLIP := MIN2 (512,X2)$
     Y1CLIP := MAX2 (-390,Y1)$
     Y2CLIP := MIN2 (390,Y2) >>$

Procedure xxx!.Delay();			  %. some devices may need a
 NIL;					  %. delay to flush the buffer output

Procedure xxx!.GRAPHON();          %. set the device in graph mode
If not !*emode then echooff();

Procedure xxx!.GRAPHOFF();	   %. Take the device out of graphics mode
If not !*emode then echoon();

Procedure xxx!.INIT$                %. Initialization of  device specIfic 
Begin                                        %. Procedures equivalent.
     PRINT "XXX IS DEVICE"$
     DEV!. := ' XXX;
     FNCOPY( 'EraseS, 'xxx!.EraseS)$         % should be called as for 
     FNCOPY( 'NormX, 'xxx!.NormX)$           % initialization when using 
     FNCOPY( 'NormY, 'xxx!.NormY)$           % xxx as the device
     FNCOPY( 'MoveS, 'xxx!.MoveS)$
     FNCOPY( 'DrawS, 'xxx!.DrawS)$
     FNCOPY( 'VWPORT, 'xxx!.VWPORT)$
     FNCOPY( 'Delay, 'xxx!.Delay)$
     FNCOPY( 'GraphOn, 'xxx!.GraphOn)$
     FNCOPY( 'GraphOff, 'xxx!.GraphOff)$
     Erase()$                     
     VWPORT(-800,800,-800,800)$
     GLOBAL!.TRANSFORM := WINdoW(-300,60)
end$
@end(verbatim)

The following is a sample session of PSL:Rlisp initializing the device xxx.
@begin(verbatim)
@@psl:rlisp
*PSL 3.0 Rlisp, 9-May-1982
*[1] load prlisp;  % The system types the [1] prompt
*[2] xxx.init();
@end(verbatim)
The system is now ready for pictureRlisp use, and one could then load
in any other routines for their application. 

It should be noted that a number of devices can be loaded into the
system but presently only one is the current display device at any
given time.

The following are specifics on each of the devices currently being
used in PictureRlisp. The coordinate systems mentioned are device
coordianates and should be transparent to the user. 

@subsection<Hp terminal 2648A>

The screen of the HP terminal is 720 units long in the X direction,
and 360 units high in the Y direction.  The coordinate system used in
HP terminal places the origin in approximately the center of the
screen, and uses a domain of -360 to 360 and a range of -180 to 180.
The procedure HP!.INIT() will load in the functions used for the HP
terminal. 

@subsection<Tektronix terminal>
Similarly, the screen of the TEKTRONIX 4006 and 4010 terminala are 1024
units long in the X direction, and 780 units high in the Y direction.
The same origin is used but the domain is -512 to 512 in the X
direction and the range is -390 to 390 in the Y direction. TEK!.INIT()
will initialize the tektronix device for displayable graphics.

@subsection<Apollo work station>
Currently the APOLLO DOMAIN can work station is being used as a terminal to
the Decsystem 20, using the ST program on the Apollo. The screen is
split into 2 windows, on of 24*80 lines, emulating a Teleray 1061,
and the other a 400 * 700 tektronix likes graphics terminal.
ST!.INIT() is used for initializing the commands for the apollo.

@subsection<Teleray Terminal>
The teleray terminal can only display characters on the screen. It
can be used as a "rapid-checkout" device, by
drawing  all lines as a
sequence of x's. To initialize the teleray the command TEL!.INIT()
will setup the graphics device to be the teleray terminal.
This gives a 24 * 80 resolution.

@subsection<Ann Arbaor Ambassador Terminal>
The teleray terminal can only display characters on the screen. It
can be used as a "rapid-checkout" device, by
drawing  all lines as a
sequence of x's. To initialize the teleray the command TEL!.INIT()
will setup the graphics device to be the teleray terminal.
This gives a 60 * 80 resolution.

@subsection<Evans and Sutherland Multi Picture System>
Currently, the MPS can be driven on the gr-vax at the University of
Utah and is an example of a high level graphics device being driven by
PictureRLISP. Thus it may be interesting to look at the device driver
for the mps to get the feel for how PictureRLISP drives high level
graphics devices. The initialization is done by calling the procedure
MPS!.INIT(). 

[???? add the other devices such as the AED, ADM3a+Retro ???]


@section<Future Work>

PictureRLISP currently uses a large number of vectors, regenerating points
at the very lowest level.  Since all Clipping and transformation is
done in LISP, using vectors. This results in very frequent garbage collection,
a time-consuming and expensive process. On the DEC-20, a grabage takes about 2.5 secs. On the VAX, GC is only 1 second, and happens much less frequently.
It is planned to optimize this lower level.

Perhaps  this could be fixed by using a number of fluid point vectors
as the only points which exist as vectors.


Since all devices currently defined in PRLISP-DRIVERS.RED use a standard
tektronix interface it becomes impossible under the current version to use
some features that the devices have defined in hardware. For instance the
MPS system has bult in clipping, viewport and windowing functions all
defined in hardeware as well as 3-d display. At this point it is impossible
for one to use the full features offered by the mps and it seems that it
would be nice if one could use some of these features.

@section(References)
@bibliography()
