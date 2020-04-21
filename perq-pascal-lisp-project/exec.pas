procedure execute;
begin
if (code < 1) or (code > 201) then
    writeln('Illegal operation code: ', code)
else
case code of
    (* Case for DSKIN *)
1:    PAS350;
    (* Case for RDEVPR *)
2:    PAS349;
    (* Case for READCH *)
3:    PAS348;
    (* Case for LITER *)
4:    PAS347;
    (* Case for DIGIT *)
5:    PAS346;
    (* Case for UPBV *)
6:    PAS345;
    (* Case for PUTV *)
7:    PAS344;
    (* Case for MKVECT *)
8:    PAS343;
    (* Case for GETV *)
9:    PAS342;
    (* Case for UNFLUID *)
10:    PAS341;
    (* Case for GLOBALP *)
11:    PAS340;
    (* Case for GLOBAL *)
12:    PAS339;
    (* Case for REMOB *)
13:    PAS338;
    (* Case for INTERN *)
14:    PAS337;
    (* Case for FLUIDP *)
15:    PAS336;
    (* Case for VECTORP *)
16:    PAS335;
    (* Case for STRINGP *)
17:    PAS334;
    (* Case for FLOATP *)
18:    PAS333;
    (* Case for PRIN2TL *)
19:    PAS332;
    (* Case for FLUID *)
20:    PAS331;
    (* Case for PUTC *)
21:    PAS330;
    (* Case for MAPLIST *)
22:    PAS329;
    (* Case for MAPCON *)
23:    PAS328;
    (* Case for MAPCAR *)
24:    PAS327;
    (* Case for NCONC *)
25:    PAS326;
    (* Case for MAPCAN *)
26:    PAS325;
    (* Case for MAPC *)
27:    PAS324;
    (* Case for OR *)
28:    PAS323;
    (* Case for COND *)
29:    PAS322;
    (* Case for AND *)
30:    PAS321;
    (* Case for WHILE *)
31:    PAS320;
    (* Case for PROG *)
32:    PAS319;
    (* Case for MAP *)
33:    PAS318;
    (* Case for PROGG0131 *)
34:    PAS317;
    (* Case for GO *)
35:    PAS316;
    (* Case for RETURN *)
36:    PAS315;
    (* Case for PROG2 *)
37:    PAS314;
    (* Case for PROGN *)
38:    PAS313;
    (* Case for APPLY1 *)
39:    PAS312;
    (* Case for EVLIS *)
40:    PAS311;
    (* Case for DM *)
41:    PAS310;
    (* Case for DN *)
42:    PAS39;
    (* Case for DF *)
43:    PAS38;
    (* Case for DE *)
44:    PAS37;
    (* Case for PUTL *)
45:    PAS36;
    (* Case for REMD *)
46:    PAS35;
    (* Case for PUTD *)
47:    PAS34;
    (* Case for GETD *)
48:    PAS33;
    (* Case for TTHROW *)
49:    PAS32;
    (* Case for TCATCH *)
50:    PAS31;
    (* Case for RLIST *)
51:    PAS133;
    (* Case for EOFP *)
52:    PAS132;
    (* Case for REV *)
53:    PAS131;
    (* Case for REVX *)
54:    PAS130;
    (* Case for PRIN2 *)
55:    PAS129;
    (* Case for CDDDR *)
56:    PAS128;
    (* Case for CDDAR *)
57:    PAS127;
    (* Case for CDADR *)
58:    PAS126;
    (* Case for CDAAR *)
59:    PAS125;
    (* Case for CADDR *)
60:    PAS124;
    (* Case for CADAR *)
61:    PAS123;
    (* Case for CAADR *)
62:    PAS122;
    (* Case for CAAAR *)
63:    PAS121;
    (* Case for CDDDDR *)
64:    PAS120;
    (* Case for CDDDAR *)
65:    PAS119;
    (* Case for CDDADR *)
66:    PAS118;
    (* Case for CDDAAR *)
67:    PAS117;
    (* Case for CDADDR *)
68:    PAS116;
    (* Case for CDADAR *)
69:    PAS115;
    (* Case for CDAADR *)
70:    PAS114;
    (* Case for CDAAAR *)
71:    PAS113;
    (* Case for CADDDR *)
72:    PAS112;
    (* Case for CADDAR *)
73:    PAS111;
    (* Case for CADADR *)
74:    PAS110;
    (* Case for CADAAR *)
75:    PAS19;
    (* Case for CAADDR *)
76:    PAS18;
    (* Case for CAADAR *)
77:    PAS17;
    (* Case for CAAADR *)
78:    PAS16;
    (* Case for CAAAAR *)
79:    PAS15;
    (* Case for CDDR *)
80:    PAS14;
    (* Case for CDAR *)
81:    PAS13;
    (* Case for CADR *)
82:    PAS12;
    (* Case for CAAR *)
83:    PAS11;
    (* Case for *FIRST-PROCEDURE *)
84:    FIRSTP;
    (* Case for ADD1 *)
85:    XADD1;
    (* Case for XAPPLY *)
86:    XXAPPLY;
    (* Case for CAR *)
87:    XCAR;
    (* Case for CATCH *)
88:    XCATCH;
    (* Case for CDR *)
89:    XCDR;
    (* Case for CODEP *)
90:    XCODEP;
    (* Case for COMPRESS *)
91:    COMPRESS;
    (* Case for CONS *)
92:    XCONS;
    (* Case for CLOSE *)
93:    XCLOSE;
    (* Case for DIFFERENCE *)
94:    XDIFFERENCE;
    (* Case for DIVIDE *)
95:    XDIVIDE;
    (* Case for EVAL *)
96:    XEVAL;
    (* Case for EXPLODE *)
97:    EXPLODE;
    (* Case for FASTSTAT *)
98:    XFASTSTAT;
    (* Case for GENSYM *)
99:    GENSYM;
    (* Case for GREATERP *)
100:    XGREATERP;
    (* Case for LESSP *)
101:    XLESSP;
    (* Case for MINUS *)
102:    XMINUS;
    (* Case for NCONS *)
103:    XNCONS;
    (* Case for OPEN *)
104:    XOPEN;
    (* Case for PLUS2 *)
105:    XPLUS2;
    (* Case for PRINT *)
106:    XPRINT;
    (* Case for QUOTIENT *)
107:    XQUOTIENT;
    (* Case for RDTOK *)
108:    XRDTOK;
    (* Case for RDS *)
109:    XRDS;
    (* Case for READ *)
110:    XREAD;
    (* Case for RECLAIM *)
111:    XGCOLLECT;
    (* Case for REMAINDER *)
112:    XREMAINDER;
    (* Case for RPLACA *)
113:    XRPLACA;
    (* Case for RPLACD *)
114:    XRPLACD;
    (* Case for TERPRI *)
115:    XTERPRI;
    (* Case for TIMES2 *)
116:    XTIMES2;
    (* Case for THROW *)
117:    XTHROW;
    (* Case for UNBINDTO *)
118:    XUNBINDTO;
    (* Case for WRTOK *)
119:    XWRTOK;
    (* Case for WRS *)
120:    XWRS;
    (* Case for XCONS *)
121:    XXCONS;
    (* Case for PAIRP *)
122:    PAS21;
    (* Case for EQ *)
123:    PAS22;
    (* Case for NOT *)
124:    PAS23;
    (* Case for IDP *)
125:    PAS24;
    (* Case for NULL *)
126:    PAS25;
    (* Case for CONSTANTP *)
127:    PAS26;
    (* Case for EQN *)
128:    PAS27;
    (* Case for LIST2 *)
129:    PAS28;
    (* Case for LIST3 *)
130:    PAS29;
    (* Case for LIST4 *)
131:    PAS210;
    (* Case for LIST5 *)
132:    PAS211;
    (* Case for REVERSE *)
133:    PAS212;
    (* Case for APPEND *)
134:    PAS213;
    (* Case for MEMBER *)
135:    PAS214;
    (* Case for ERROR *)
136:    PAS215;
    (* Case for PAIR *)
137:    PAS216;
    (* Case for APPLY *)
138:    PAS217;
    (* Case for SASSOC *)
139:    PAS218;
    (* Case for ASSOC *)
140:    PAS219;
    (* Case for SUBLIS *)
141:    PAS220;
    (* Case for SUBST *)
142:    PAS221;
    (* Case for MEMQ *)
143:    PAS222;
    (* Case for ATSOC *)
144:    PAS223;
    (* Case for PUT *)
145:    PAS224;
    (* Case for DEFLIST *)
146:    PAS225;
    (* Case for EQUAL *)
147:    PAS226;
    (* Case for DELETE *)
148:    PAS227;
    (* Case for DELQ *)
149:    PAS228;
    (* Case for DELATQ *)
150:    PAS229;
    (* Case for GET *)
151:    PAS230;
    (* Case for REMPROP *)
152:    PAS231;
    (* Case for LENGTH *)
153:    PAS232;
    (* Case for ERRPRT *)
154:    PAS233;
    (* Case for MSGPRT *)
155:    PAS234;
    (* Case for FLAGP *)
156:    PAS235;
    (* Case for FLAG1 *)
157:    PAS236;
    (* Case for FLAG *)
158:    PAS237;
    (* Case for REMFLAG1 *)
159:    PAS238;
    (* Case for REMFLAG *)
160:    PAS239;
    (* Case for EQCAR *)
161:    PAS240;
    (* Case for PLIST *)
162:    PAS241;
    (* Case for VALUE *)
163:    PAS242;
    (* Case for FUNCELL *)
164:    PAS243;
    (* Case for SETPLIST *)
165:    PAS244;
    (* Case for SETVALUE *)
166:    PAS245;
    (* Case for SETFUNCELL *)
167:    PAS246;
    (* Case for ORDERP *)
168:    PAS247;
    (* Case for TOKEN *)
169:    PAS248;
    (* Case for ERRORSET *)
170:    PAS249;
    (* Case for NUMBERP *)
171:    PAS250;
    (* Case for FIXP *)
172:    PAS251;
    (* Case for MINUSP *)
173:    PAS252;
    (* Case for ABS *)
174:    PAS253;
    (* Case for SUB1 *)
175:    PAS254;
    (* Case for ZEROP *)
176:    PAS255;
    (* Case for ONEP *)
177:    PAS256;
    (* Case for EXPT *)
178:    PAS257;
    (* Case for FIX *)
179:    PAS258;
    (* Case for FLOAT *)
180:    PAS259;
    (* Case for EXPAND *)
181:    PAS260;
    (* Case for MAX *)
182:    PAS261;
    (* Case for MIN *)
183:    PAS262;
    (* Case for PLUS *)
184:    PAS263;
    (* Case for TIMES *)
185:    PAS264;
    (* Case for MAX2 *)
186:    PAS265;
    (* Case for MIN2 *)
187:    PAS266;
    (* Case for FUNCTION *)
188:    PAS267;
    (* Case for ATOM *)
189:    PAS268;
    (* Case for SET *)
190:    PAS269;
    (* Case for PRINC *)
191:    PAS270;
    (* Case for PRIN1 *)
192:    PAS271;
    (* Case for PRIN2T *)
193:    PAS272;
    (* Case for LBIND1 *)
194:    PAS273;
    (* Case for PBIND1 *)
195:    PAS274;
    (* Case for UNBIND1 *)
196:    PAS275;
    (* Case for UNBINDN *)
197:    PAS276;
    (* Case for LBINDN *)
198:    PAS277;
    (* Case for P.N *)
199:    PAS278;
    (* Case for EVLAM *)
200:    PAS279;
    (* Case for PBINDN *)
201:    PAS280;
end;  (* of case *)
end;
   