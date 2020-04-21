Current FASL file format:

Word: Magic number (currently 99).
Word: Number of local IDs.
Block: Local ID names, in order, in regular Lisp format (string size followed
		by block of chars).
Word: Size of code segment in words.
Word: Offset in addressing units of initialization procedure.
Block: Code segment.
Word: Size of bit table in words (redundant, could be eliminated).
Block: Bit table.


Bit table format:

Block of 2 bit items, one for each \addressing unit/ in the code block.
0: Don't relocate at this offset.
1: Relocate the word at this offset in the code segment.
2: Relocate the (halfword on VAX, right half on 20) at this offset.
3: Relocate the info field of the Lisp item at this offset.

The data referred to by relocation entries in the bit table are split into
tag and info fields.  The tag field specifies the type of relocation to be
done:
0: Add the code base to the info part.
1: Replace the local ID number in the info part by its global ID number.
2: Replace the local ID number in the info part by the location of
	its value cell.
3: Replace the local ID number in the info part by the location of
	its function cell.

Local ID numbers begin at 2048, to allow for statically allocated ID numbers
(those which will be the same at compile time and load time).
