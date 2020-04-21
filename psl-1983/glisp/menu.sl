%  MENU.SL.1
%  Abstract datatype for Menu operations.
%  G. Novak     31 Jan. 83


(glispobjects

(menu (listobject (items (listof atom)))
  msg ((create menu-create)
       (select menu-select)))

)

% Initialize a menu which has been newly created.
(dg menu-create (m:menu))

% Ask the user for a selection from a menu.
(dg menu-select (m:menu)
)
