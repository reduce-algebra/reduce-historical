%  GLTEST.SL.2   18 February 1983

% GLISP TEST FUNCTIONS, PSL VERSION.

% Object descriptions for a Company database.
(GLISPOBJECTS

(EMPLOYEE                             % Name of the object type

   (LIST (NAME STRING)                % Actual storage structure
	 (DATE-HIRED (A DATE))
	 (SALARY REAL)
         (JOBTITLE ATOM)
	 (TRAINEE BOOLEAN))

   PROP   ((SENIORITY ((THE YEAR OF (CURRENTDATE))   % Computed properties
		       -
		       (THE YEAR OF DATE-HIRED)))
	   (MONTHLY-SALARY (SALARY * 174)))

   ADJ    ((HIGH-PAID (MONTHLY-SALARY > 2000)))      % Computed adjectives

   ISA    ((TRAINEE (TRAINEE))
	   (GREENHORN (TRAINEE AND SENIORITY < 2)))

   MSG    ((YOURE-FIRED (SALARY _ 0)))  )            % Message definitions


(Date
   (List (MONTH INTEGER)
	 (DAY INTEGER)
	 (YEAR INTEGER))
   PROP   ((MONTHNAME ((NTH  '(JANUARY FEBRUARY MARCH APRIL MAY JUNE JULY
                               AUGUST SEPTEMBER OCTOBER NOVEMBER DECEMBER)
		             MONTH)))
	   (PRETTYFORM ((LIST DAY MONTHNAME YEAR)))
	   (SHORTYEAR (YEAR - 1900)))  )


(COMPANY
   (ATOM (PROPLIST (PRESIDENT (AN EMPLOYEE))
		   (EMPLOYEES (LISTOF EMPLOYEE)  )))
   PROP  ((ELECTRICIANS ((THOSE EMPLOYEES WITH JOBTITLE='ELECTRICIAN)))) )

)


% Some test data for the above functions.
(setq company1 (a company with
   President = (An Employee with Name = "Oscar the Grouch"
                                 Salary = 88.0
                                 Jobtitle = 'President
                                 Date-Hired = (A Date with Month = 3
                                                  Day = 15 Year = 1907))
   Employees = (list
               (An Employee with Name = "Cookie Monster"
                                 Salary = 12.50
                                 Jobtitle = 'Electrician
                                 Date-Hired = (A Date with Month = 7
                                                  Day = 21 Year = 1947))
               (An Employee with Name = "Betty Lou"
                                 Salary = 9.00
                                 Jobtitle = 'Electrician
                                 Date-Hired = (A Date with Month = 5
                                                  Day = 15 Year = 1980))
               (An Employee with Name = "Grover"
                                 Salary = 3.00
                                 Jobtitle = 'Electrician
                                 Trainee = T
                                 Date-Hired = (A Date with Month = 6
                                                  Day = 13 Year = 1978))
)))

% Program to give raises to the electricians.
(DG GIVE-RAISE
   (:COMPANY)
	   (FOR EACH ELECTRICIAN WHO IS NOT A TRAINEE
	      DO (SALARY _+(IF SENIORITY > 1
			       THEN 2.5
			       ELSE 1.5))
		 (PRINT (THE NAME OF THE ELECTRICIAN))
                 (PRINT (THE PRETTYFORM OF DATE-HIRED))
                 (PRINT MONTHLY-SALARY) ))

(DG CURRENTDATE ()    (Result DATE)
	   (A DATE WITH YEAR = 1981   MONTH = 11   DAY = 30))







% The following object descriptions are used in a graphics object test
% program (derived from one written by D.G. Bobrow as a LOOPS example).
% The test program MGO-TEST runs on a Xerox D-machine, but won't run on
% other machines.

(GLISPOBJECTS

% The actual stored structure for a Vector is simple, but it is overloaded
% with many properties.

(VECTOR

   (LIST (X INTEGER)
	 (Y INTEGER))

   PROP   ((MAGNITUDE ((SQRT X^2 + Y^2)))
           (DIRECTION ((IF X IS ZERO THEN (IF Y IS NEGATIVE THEN -90.0
                                                            ELSE 90.0)
                                     ELSE (ATAN2D Y X))) RESULT DEGREES)
                   )

   ADJ    ((ZERO (X IS ZERO AND Y IS ZERO))
	   (NORMALIZED (MAGNITUDE = 1.0)))

   MSG    ((+ VECTORPLUS OPEN T)   % Defining operators as messages
                                   % causes the compiler to automatically
                                   % overload the operators.
	   (- VECTORDIFF OPEN T)
	   (* VECTORTIMESSCALAR ARGTYPES (NUMBER) OPEN T)
	   (* VECTORDOTPRODUCT ARGTYPES (VECTOR) OPEN T)
	   (/ VECTORQUOTIENTSCALAR OPEN T)
	   (_+ VECTORMOVE OPEN T)
	   (PRIN1 ((PRIN1 "(")
		   (PRIN1 X)
		   (PRIN1 ",")
		   (PRIN1 Y)
		   (PRIN1 ")")))
	   (PRINT ((SEND SELF PRIN1)  % PRINT is defined in terms of the
		   (TERPRI)))  ) )    % PRIN1 message of this object.


(DEGREES REAL                         % Stored value is just a real number.
   PROP ((RADIANS (self*(3.1415926 / 180.0)) RESULT RADIANS)))

(RADIANS REAL
   PROP ((DEGREES (self*(180.0 / 3.1415926)) RESULT DEGREES)))

% A FVECTOR is a very different kind of VECTOR: it has a different
% storage structure and different element types.  However, it can
% still inherit some vector properties, e.g., addition.
(FVECTOR (CONS (Y STRING) (X BOOLEAN))
  SUPERS (VECTOR))
 
% The definition of GraphicsObject builds on that of Vector.
(GRAPHICSOBJECT

   (LIST (SHAPE ATOM)
	 (START VECTOR)
	 (SIZE VECTOR))

   PROP   ((LEFT (START:X))           % A property defined in terms of a
                                      % property of a substructure
	   (BOTTOM (START:Y))
	   (RIGHT (LEFT+WIDTH))
	   (TOP (BOTTOM+HEIGHT))
	   (WIDTH (SIZE:X))
	   (HEIGHT (SIZE:Y))
	   (CENTER (START+SIZE/2))    % Vector arithmetic
	   (AREA (WIDTH*HEIGHT)))

   MSG    ((DRAW ((APPLY (GET SHAPE 'DRAWFN)   % A way to get runtime message
			 (List SELF            % behavior without using the
			  (QUOTE PAINT)))))    % message mechanism.
	   (ERASE ((APPLY (GET SHAPE 'DRAWFN)
			  (LIST  SELF
			   (QUOTE ERASE)))))
	   (MOVE GRAPHICSOBJECTMOVE OPEN T))  )

(MOVINGGRAPHICSOBJECT

   (LIST (TRANSPARENT GRAPHICSOBJECT)          % Includes properties of a
	 (VELOCITY VECTOR))                    % GraphicsObject due to the
                                               % TRANSPARENT declaration.
   Msg    ((ACCELERATE MGO-ACCELERATE OPEN T)
	   (STEP ((SEND SELF MOVE VELOCITY))))  )
)


% The following functions define arithmetic operations on Vectors.
% These functions are generally called OPEN (macro-expanded) rather
% than being called directly.
(DG VECTORPLUS
   (V1:vector V2:VECTOR)
	   (A (TYPEOF V1) WITH X = V1:X + V2:X   Y = V1:Y + V2:Y))

(DG VECTORDIFF
   (V1:vector V2:VECTOR)
	   (A (TYPEOF V1) WITH X = V1:X - V2:X   Y = V1:Y - V2:Y))

(DG VECTORTIMESSCALAR
   (V:VECTOR N:NUMBER)
	   (A (TYPEOF V) WITH X = X*N   Y = Y*N))

(DG VECTORDOTPRODUCT
   (V1:vector V2:VECTOR)
	   (A (TYPEOF V1) WITH X = V1:X * V2:X   Y = V1:Y * V2:Y))

(DG VECTORQUOTIENTSCALAR
   (V:VECTOR N:NUMBER)
	   (A (TYPEOF V) WITH X = X/N   Y = Y/N))

% VectorMove, which defines the _+ operator for vectors, does a destructive
% addition to the vector which is its first argument.  Thus, the expression
% U_+V will destructively change U, while U_U+V will make a new vector with
% the value U+V and assign its value to U.
(DG VECTORMOVE
   (V:vector DELTA:VECTOR)
	   (V:X _+ DELTA:X)
	   (V:Y _+ DELTA:Y)
           V)

% An object is moved by erasing it, changing its starting point, and
% then redrawing it.
(DG GRAPHICSOBJECTMOVE
   (SELF:GRAPHICSOBJECT DELTA:VECTOR)
	   (SEND SELF ERASE)     % Erase the object
	   (START _+ DELTA)      % Destructively move start point by delta
	   (SEND SELF DRAW))     % Redraw the object in new location

(DG MGO-ACCELERATE
   (SELF: MOVINGGRAPHICSOBJECT ACCELERATION: VECTOR)
	   VELOCITY _+ ACCELERATION)


% Now we define some test functions which use the above definitions.
% First there are some simple functions which test vector operations.
(DG TVPLUS (U:VECTOR V:VECTOR) U+V)
(DG TVMOVE (U:VECTOR V:VECTOR) U_+V)
(DG TVTIMESV (U:VECTOR V:VECTOR) U*V)
(DG TVTIMESN (U:VECTOR V:NUMBER) U*V)
(DG TFVPLUS (U:FVECTOR V:FVECTOR) U+V)


% This test function creates a MovingGraphicsObject and then moves it
% across the screen by sending it MOVE messages.  Everything in this
% example is compiled open; the STEP message involves a great deal of
% message inheritance.
(DG MGO-TEST ()
   (PROG (MGO N)
         (MGO _(A MOVINGGRAPHICSOBJECT WITH
                    SHAPE =    (QUOTE RECTANGLE)
		    SIZE =     (A VECTOR WITH X = 4   Y = 3)
		    VELOCITY = (A VECTOR WITH X = 3   Y = 4)))
         (N _ 0)
         (WHILE (N_+1)<100 (SEND MGO STEP))
         (SEND (THE START OF MGO) PRINT)))


% This function tests the properties of a GraphicsObject.
(DG TESTFN2 (:GRAPHICSOBJECT)
   (LIST SHAPE START SIZE LEFT BOTTOM RIGHT TOP   
		 WIDTH HEIGHT CENTER AREA))

% Function to draw a rectangle.  Computed properties of the rectangle are
% used within calls to the graphics functions, making the code easy to
% write and understand.
(DG DRAWRECT (SELF:GRAPHICSOBJECT DSPOP:ATOM)
   (PROG (OLDDS)
         (OLDDS _(CURRENTDISPLAYSTREAM DSPS))
         (DSPOPERATION DSPOP)
         (MOVETO LEFT BOTTOM)
         (DRAWTO LEFT TOP)
         (DRAWTO RIGHT TOP)
         (DRAWTO RIGHT BOTTOM)
         (DRAWTO LEFT BOTTOM)
         (CURRENTDISPLAYSTREAM OLDDS) ))





% The LispTree and PreorderSearchRecord objects illustrate how generators
% can be written.
(GLISPOBJECTS

% In defining a LispTree, which can actually be of multiple types (atom or
% dotted pair), we define it as the more complex dotted-pair type and take
% care of the simpler case in the PROPerty definitions.
(LISPTREE
   (CONS (CAR LISPTREE)      % Defines a LispTree structure as the CONS
	 (CDR LISPTREE))     % of two fields named CAR and CDR.

   PROP   ((LEFTSON ((IF SELF IS ATOMIC THEN NIL ELSE CAR)))
	   (RIGHTSON ((IF SELF IS ATOMIC THEN NIL ELSE CDR))))

   ADJ    ((EMPTY (~SELF)))  )

% PreorderSearchRecord is defined to be a generator.  Its data structure holds
% the current node and a stack of previous nodes, and its NEXT message is
% defined as code to step through the preorder search.
(PREORDERSEARCHRECORD

   (CONS (NODE LISPTREE)
	 (PREVIOUSNODES (LISTOF LISPTREE)))

   MSG    ((NEXT ((PROG (TMP)
                   (IF TMP_NODE:LEFTSON
                     THEN (IF NODE:RIGHTSON THEN PREVIOUSNODES+_NODE)
                          NODE_TMP
                     ELSE TMP-_PREVIOUSNODES
                          NODE_TMP:RIGHTSON)))))  )
)


% PRINTLEAVES prints the leaves of the tree, using a PreorderSearchRecord
% as the generator for searching the tree.
(DG PRINTLEAVES (:LISPTREE)
   (PROG (PSR)
         (PSR _(A PREORDERSEARCHRECORD WITH NODE =(THE LISPTREE)))
         (WHILE NODE (IF NODE IS ATOMIC (PRINT NODE))
		     (SEND PSR NEXT))))



% The Circle objects illustrate the definition of a number of mathematical
% properties of an object in terms of stored data and other properties.
(Glispobjects

(CIRCLE (LIST (START VECTOR) (RADIUS REAL))
    PROP ((PI            (3.1415926))       % A PROPerty can be a constant.
          (DIAMETER      (RADIUS*2))
          (CIRCUMFERENCE (PI*DIAMETER))     % Defined in terms of other prop.
          (AREA          (PI*RADIUS^2)) )
    ADJ  ((BIG           (AREA>120))        % BIG defined in terms of AREA
          (MEDIUM        (AREA >= 60 AND AREA <= 120))
          (SMALL         (AREA<60)))
    MSG  ((STANDARD      (AREA_100))        % "Storing into" computed property
          (GROW          (AREA_+100))
          (SHRINK        (AREA_AREA/2)) )
     )


%   A DCIRCLE is implemented differently from a circle.
%   The data structure is different, and DIAMETER is stored instead of RADIUS.
%   By defining RADIUS as a PROPerty, all of the CIRCLE properties defined
%   in terms of radius can be inherited.

(DCIRCLE (LISTOBJECT (START VECTOR) (DIAMETER REAL))
    PROP ((RADIUS       (DIAMETER/2)))
   SUPERS (CIRCLE) )
)

%   Make a DCIRCLE for testing
(setq dc (a dcircle with diameter = 10.0))

%   Since DCIRCLE is an Object type, it can be used with interpreted messages,
%   e.g.,  (send dc area)     to get the area property,
%          (send dc standard) to set the area to the standard value,
%          (send dc diameter) to get the stored diameter value.



% EXAMPLE OF ASSIGNMENT TO COMPUTED PROPERTY
(DG GROWCIRCLE (C:CIRCLE)
   (C:AREA_+100)
   C )

(SETQ MYCIRCLE (A CIRCLE))

% Since SQRT is not defined in the bare-PSL system, we redefine it here.
(DG SQRT (X)
  (PROG (S)
    (S_X)
    (IF X < 0 THEN (ERROR)
        ELSE (WHILE (ABS S*S - X) > 0.000001 DO (S _ (S+X/S) * 0.5)))
    (RETURN S)))

% Function SQUASH illustrates elimination of compile-time constants.
% Of course, nobody would write such a function directly.  However, such forms
% can arise when inherited properties are compiled.  Conditional compilation
% occurs automatically when appropriate variables are defined to the GLISP
% compiler as compile-time constants because the post-optimization phase of
% the compiler makes the unwanted code disappear.

(DG SQUASH ()
  (IF 1>3 THEN 'AMAZING
      ELSEIF (SQRT 7.2) < 2 THEN 'INCREDIBLE
      ELSEIF 2 + 2 = 4 THEN 'OKAY
      ELSE 'JEEZ))


% The following object definitions describe a student records database.
(glispobjects

(student (atom (proplist (name string)
			 (sex atom)
			 (major atom)
			 (grades (listof integer))))
   prop ((average student-average)
	 (grade-average student-grade-average))
   adj  ((male (sex='male))
	 (female (sex='female))
	 (winning (average>=95))
	 (losing (average<60)))
   isa  ((winner (self is winning))))

(student-group (listof student)
   prop ((n-students length)       % This property is implemented by
                                   % the Lisp function LENGTH. 
	 (Average Student-group-average)))

(class (atom (proplist (department atom)
		       (number integer)
		       (instructor string)
		       (students student-group)))
   prop ((n-students (students:n-students))
	 (men ((those students who are male)))
	 (women ((those students who are female)))
	 (winners ((those students who are winning)))
	 (losers ((those students who are losing)))
	 (class-average (students:average))))

)


(dg student-average (s:student)
  (prog ((sum 0.0)(n 0.0))
    (for g in grades do  n _+ 1.0    sum_+g)
    (return sum/n) ))

(dg student-grade-average (s:student)
  (prog ((av s:average))
    (return (if av >= 90.0 then 'a
		elseif av >= 80.0 then 'b
		elseif av >= 70.0 then 'c
		elseif av >= 60.0 then 'd
		else 'f))))


(dg student-group-average (sg:student-group)
  (prog ((sum 0.0))
    (for s in sg do sum_+s:average)
    (return sum/sg:n-students) ))

% Print name and grade average for each student
(dg test1 (c:class)
  (for s in c:students (prin1 s:name)
                       (prin2 '! )
		       (print s:grade-average)))

% Another version of the above function
(dg test1b (:class)
  (for each student (prin1 name)
                    (prin2 '! )
                    (print grade-average)))

% Print name and average of the winners in the class
(dg test2 (c:class)
  (for s in c:winners (prin1 s:name)
                      (prin2 '! )
		      (print s:average)))

% The average of all the male students' grades
(dg test3 (c:class)
  c:men:average)

% The name and average of the winning women
(dg test4 (c:class)
  (for s in c:women when s is winning
                       (prin1 s:name)
                       (prin2 '! )
		       (print s:average)))

% Another version of the above function.  The * operator in this case
% denotes the intersection of the sets of women and winners.  The
% GLISP compiler optimizes the code so that these intermediate sets are
% not actually constructed.
(dg test4b (c:class)
  (for s in c:women*c:winners
                       (prin1 s:name)
                       (prin2 '! )
		       (print s:average)))

% Make a list of the easy professors.
(dg easy-profs (classes:(listof class))
  (for each class with class-average > 90.0 collect (the instructor)))

% A more Pascal-like version of easy-profs:
(dg easy-profs-b (classes:(listof class))
  (for c in classes when c:class-average > 90.0 collect c:instructor))


% Some test data for testing the above functions.
(setq class1 (a class with instructor = "A. Prof" department = 'cs
     number = 102 students =
 (list
   (a student with name = "John Doe" sex = 'male major = 'cs
       grades = '(99 98 97 93))
   (a student with name = "Fred Failure" sex = 'male major = 'cs
       grades = '(52 54 43 27))
   (a student with name = "Mary Star" sex = 'female major = 'cs
       grades = '(100 100 99 98))
   (a student with name = "Doris Dummy" sex = 'female major = 'cs
       grades = '(73 52 46 28))
   (a student with name = "Jane Average" sex = 'female major = 'cs
       grades = '(75 82 87 78))
   (a student with name = "Lois Lane" sex = 'female major = 'cs
       grades = '(98 95 97 96)) )))



% The following object definitions illustrate inheritance of properties
% from multiple parent classes.  The three "bottom" classes Planet, Brick,
% and Bowling-Ball all inherit the same definition of the property Density,
% although they are represented in very different ways.
(glispobjects

(physical-object anything
  prop ((density (mass/volume))))

(ordinary-object anything
  prop ((mass (weight / 9.88)))    % Compute mass as weight/gravity
  supers (physical-object))

(sphere anything
  prop ((volume ((4.0 / 3.0) * 3.1415926 * radius ^ 3))))

(parallelepiped anything
  prop ((volume (length*width*height))))

(planet (listobject (mass real)(radius real))
  supers (physical-object sphere))    % A planet is a physical-object
                                      % and a sphere.

(brick (object (length real)(width real)(height real)(weight real))
  supers (ordinary-object parallelepiped))

(bowling-ball (atomobject (type atom)(weight real))
  prop ((radius ((if type='adult then 0.1 else 0.07))))
  supers (ordinary-object sphere))

)

% Three test functions to demonstrate inheritance of the Density property.
(dg dplanet (p:planet) density)

(dg dbrick (b:brick) density)

(dg dbb (b:bowling-ball) density)

% Some objects to test the functions on.
(setq earth (a planet with mass = 5.98e24 radius = 6.37e6))

(setq brick1 (a brick with weight = 20.0 width = 0.10 height = 0.05
                length = 0.20))

(setq bb1 (a bowling-ball with type = 'adult weight = 60.0))


% Since the object types Planet, Brick, and Bowling-Ball are defined as
% Object types (i.e., they contain the Class name as part of their stored
% data), messages can be sent to them directly from the keyboard for
% interactive examination of the objects.  For example, the following
% messages could be used:
%     (send earth density)
%     (send brick1 weight: 25.0)
%     (send brick1 mass: 2.0)
%     (send bb1 radius)
%     (send bb1 type: 'child)
