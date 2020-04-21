%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "SysLisp" part of the HEAP-STATS package.
%%%
%%% Author: Cris Perdue
%%% December 1982
%%% Documented January 1983
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

on SysLisp;

compiletime <<
put('igetv,'assign!-op,'iputv);
>>;

%%% Magic constants defining the layout of a "heap-stats" object.
compiletime <<
Internal WConst TemplateX = 2,
       StringTabX = 3,
       StringSpaceX = 4,
       VectTabX = 5,
       VectSpaceX = 6,
       WordTabX = 7,
       WordSpaceX = 8,
       Pairs = 9,
       Strings = 10,
       HalfWords = 11,
       WordVecs = 12,
       Vectors = 13;
>>;

%%% This procedure sweeps the heap and collects statistics into
%%% its argument, which is a heap-stats object.  This routine may
%%% be called as part of a garbage collection, so it may not do
%%% any allocation whatsoever from the heap.  Moderate size
%%% integers are assumed to have in effect no tag.
syslsp procedure HeapStats(Results);
begin
   scalar CurrentItem,
   ObjLen,
   Last,
   HistoSize,
   StdTemplate,
   StringHTab,
   StringSpaceTab,
   VectHTab,
   VectSpaceTab,
   WordHTab,
   WordSpaceTab,
   Len;

   %% Check that the argument looks reasonable.
   if neq(isizev(Results), 13) then
      return nil;

   StdTemplate := igetv(Results,TemplateX);

   StringHTab := igetv(Results,StringTabX);
   StringSpaceTab := igetv(Results,StringSpaceX);
   VectHTab := igetv(Results,VectTabX);
   VectSpaceTab := igetv(Results,VectSpaceX);
   WordHTab := igetv(Results,WordTabX);
   WordSpaceTab := igetv(Results,WordSpaceX);

   %% Check the various subobjects of the argument to see that
   %% they look reasonable.  The returns are all errors effectively.
   HistoSize := isizev(StdTemplate) + 1;
   if neq(isizev(StringHTab),HistoSize) then return 1;
   if neq(isizev(StringSpaceTab),HistoSize) then return 2;
   if neq(isizev(VectHTab),HistoSize) then return 3;
   if neq(isizev(VectSpaceTab),HistoSize) then return 4;
   if neq(isizev(WordHTab),HistoSize) then return 5;
   if neq(isizev(WordSpaceTab),HistoSize) then return 6;

   igetv(Results,Pairs) := 0;
   igetv(Results,Strings) := 0;
   igetv(Results,HalfWords) := 0;
   igetv(Results,WordVecs) := 0;
   igetv(Results,Vectors) := 0;

   FillVector(StringHTab,0);
   FillVector(StringSpaceTab,0);
   FillVector(VectHTab,0);
   FillVector(VectSpaceTab,0);
   FillVector(WordHTab,0);
   FillVector(WordSpaceTab,0);

   Last := HeapLast();
   CurrentItem := HeapLowerBound();
   while CurrentItem < Last do
      begin
	 case Tag @CurrentItem of
	 BTRTAG, CODE, NEGINT, POSINT, ID, UNBOUND,
	 STR, BYTES, FIXN, FLTN, BIGN, WRDS, Halfwords, PAIR, VECT, EVECT:
	 << ObjLen := 2;			% must be first of pair
	    igetv(Results,Pairs) := igetv(Results,Pairs) + 1;
	    >>;
	 HBYTES:
	 << Len := StrLen CurrentItem;
	    ObjLen := 1 + StrPack Len;
	    igetv(Results,Strings) := igetv(Results,Strings) + 1;
	    Histo(StdTemplate,StringHTab,Len+1,StringSpaceTab,ObjLen);
	    >>;
	 HHalfwords:
	 << ObjLen := 1 + HalfWordPack HalfWordLen CurrentItem;
	    igetv(Results,HalfWords) := igetv(Results,HalfWords) + 1;
	    >>;
	 HWRDS:
	 << Len := WrdLen CurrentItem;
	    ObjLen := 1 + WrdPack Len;
	    igetv(Results,WordVecs) := igetv(Results,WordVecs) + 1;
	    Histo(StdTemplate,WordHTab,Len+1,WordSpaceTab,ObjLen);
	    >>;
	 HVECT:
	 << Len := VecLen CurrentItem;
	    ObjLen := 1 + VectPack Len;
	    igetv(Results,Vectors) := igetv(Results,Vectors) + 1;
	    Histo(StdTemplate,VectHTab,Len+1,VectSpaceTab,ObjLen);
	    >>;
	 default:
	    Error(0,"Illegal item in heap at %o", CurrentItem);
         end;			% case
      CurrentItem := CurrentItem + ObjLen;
      end;

   Results;
   end;

%%% Internal utility routine used by heapstats to accumulate
%%% values into the statistics tables.  The template is a
%%% histogram template.  The table is a histogram table.  The
%%% "value" is tallied into the appropriate bucket of the table
%%% based on the template.  Spacetab is similar to "table", but
%%% the value of "space" will be added rather than tallied into
%%% spacetab.
Syslsp procedure Histo(Template,Table,Value,SpaceTab,Space);
begin
   for i := 0 step 1 until isizev(Template) do
      if igetv(Template,i) >= Value then
	 << igetv(Table,i) := igetv(Table,i) + 1;
	    igetv(SpaceTab,i) := igetv(SpaceTab,i) + Space;
	    return;
	 >>;
   if Value > igetv(Template,isizev(Template)) then
      << igetv(Table,isizev(Template)+1)
	    := igetv(Table,isizev(Template)+1) + 1;
	 igetv(SpaceTab,isizev(Template)+1)
	    := igetv(SpaceTab,isizev(Template)+1) + Space;
      >>;
end;

SysLsp procedure FillVector(v,k);
   for i := 0 step 1 until isizev(v) do
      igetv(v,i) := k;
