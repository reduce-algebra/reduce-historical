% Create a small virtual screen, 10 by 10 characters, starting at
% row 8 column 10.  (Remember the upper left hand corner is Row 0, Column 0.)

s1 := CreateVirtualScreen(10, 10,  8, 10);

% Fill the small screen with the letter A.
for i := 0:9 do for j := 0:9 do WriteToScreen(s1, char A, i, j);


% In normal "two window mode" there are 4 active screens, so the length of
% the list will be 4.

length activescreenlist;


% Selecting s1 gives us 5 active screens, and displays s1.
% However, the "main" screen will partly cover s1.
SelectScreen(s1);

% Deselecting s1 gives us 4 active screens.
DeSelectScreen(s1);

% Execute this FOR loop to see how stuff on the bottom window scrolls
% beneath s1.
for i := 1:30 do write i, "  ",i^2, "  ", i^3;

