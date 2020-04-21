% TLG.SL.3     31 Jan. 83     G. Novak
% Program to test speed of line graphics by filling a square with lines.
(de TLG (WINDOW)
    (PROG (XMIN XMAX DELTA XA XB)
          (SETQ XMIN 100)
          (SETQ XMAX 500)
          (SETQ XA XMIN)
          (SETQ XB XMAX)
          (SETQ DELTA 4)
      LP  (COND
	    ((IGREATERP XA XMAX)
	      (RETURN)))
          (DRAWLINE XA XMIN XB XMAX 1 (QUOTE PAINT)
		    WINDOW)
          (DRAWLINE XMIN XA XMAX XB 1 (QUOTE PAINT)
		    WINDOW)
          (SETQ XA (IPLUS XA DELTA))
          (SETQ XB (IDIFFERENCE XB DELTA))
          (GO LP)))
