;;;
;;; CHARS.LSP - Common Lisp operations on characters
;;; 
;;; Author:      Eric Benson
;;;		 Symbolic Computation Group
;;;              Computer Science Dept.
;;;              University of Utah
;;; Date:        7 April 1982
;;; Copyright (c) 1982 University of Utah
;;;

; <PSL.UTIL>CHARS.LSP.4,  2-Sep-82 14:22:45, Edit by BENSON
; Fixed bug in CHAR-UPCASE and CHAR-DOWNCASE

(defvar char-code-limit 128 "Upper bound of character code values")

(defvar char-font-limit 1 "Upper bound on supported fonts")

(defvar char-bits-limit 1 "Upper bound on values produces by char-bits")

;;;; STANDARD-CHARP - ASCII definition
(defun standard-charp (c)
  (and (characterp c)
       (or (not (or (char< c #\Space) (char> c #\Rubout)))
	   (eq c #\Eol)
	   (eq c #\Tab)
	   (eq c #\FF))))

;;;; GRAPHICP - printable character
(defun graphicp (c)
  (and (characterp c)
    (not (char< c #\Space))
    (char< c #\Rubout)))

;;;; STRING-CHARP - a character that can be an element of a string
(defun string-charp (c)
  (and (characterp c)
       (>= (char-int c) 0)
       (<= (char-int c) #\Rubout)))

;;;; ALPHAP - an alphabetic character
(defun alphap (c)
  (or (uppercasep c)
      (lowercasep c)))

;;;; UPPERCASEP - an uppercase letter
(defun uppercasep (c)
  (and (characterp c)
       (not (char< c #\A))
       (not (char> c #\Z))))

;;;; LOWERCASEP - a lowercase letter
(defun lowercasep (c)
  (and (characterp c)
       (not (char< c #\\a))
       (not (char> c #\\z))))

;;;; BOTHCASEP - same as ALPHAP
(fset 'bothcasep (fsymeval 'alphap))

;;;; DIGITP - a digit character (optional radix not supported)
(defun digitp (c)
  (when (and (characterp c)
	     (not (char< c #\0))
	     (not (char> c #\9)))
        (- (char-int c) (char-int #\0))))

;;;; ALPHANUMERICP - a digit or an alphabetic
(defun alphanumericp (c)
  (or (alphap c) (digitp c)))

;;;; CHAR= - strict character comparison
(defun char= (c1 c2)
  (eql (char-int c1) (char-int c2)))

;;;; CHAR-EQUAL - similar character objects
(defun char-equal (c1 c2)
  (or (char= c1 c2)
      (and (string-charp c1)
	   (string-charp c2)
	   (or (char< c1 #\Space) (char> c1 #\?))
	   (or (char< c2 #\Space) (char> c2 #\?))
	   (eql (logand (char-int c1) (char-int #\))
		(logand (char-int c2) (char-int #\))))))

;;;; CHAR< - strict character comparison
(defun char< (c1 c2)
  (< (char-int c1) (char-int c2)))

;;;; CHAR> - strict character comparison
(defun char> (c1 c2)
  (> (char-int c1) (char-int c2)))

;;;; CHAR-LESSP - ignore case and bits for CHAR<
(defun char-lessp (c1 c2)
  (or (char< c1 c2)
      (and (string-charp c1)
	   (string-charp c2)
	   (or (char< c1 #\Space) (char> c1 #\?))
	   (or (char< c2 #\Space) (char> c2 #\?))
	   (< (logand (char-int c1) (char-int #\))
	      (logand (char-int c2) (char-int #\))))))

;;;; CHAR-GREATERP - ignore case and bits for CHAR>
(defun char-greaterp (c1 c2)
  (or (char> c1 c2)
      (and (string-charp c1)
	   (string-charp c2)
	   (or (char< c1 #\Space) (char> c1 #\?))
	   (or (char< c2 #\Space) (char> c2 #\?))
	   (> (logand (char-int c1) (char-int #\))
	      (logand (char-int c2) (char-int #\))))))

;;;; CHAR-CODE - character to integer conversion
(defmacro char-code (c)
  c)

;;;; CHAR-BITS - bits attribute of a character
(defmacro char-bits (c)
  0)

;;;; CHAR-FONT - font attribute of a character
(defmacro char-font (c)
  0)

;;;; CODE-CHAR - integer to character conversion, optional bits, font ignored
(defmacro code-char (c)
  c)

;;;; CHARACTER - character plus bits and font, which are ignored
(defun character (c)
  (cond ((characterp c) c)
        ((stringp c) (char c 0))
        ((symbolp c) (char (get-pname c) 0))
	(t (stderror (bldmsg "%r cannot be coerced to a character" c)))))

;;;; CHAR-UPCASE - raise a character
(defun char-upcase (c)
  (if (not (or (char< c #\\a)
	       (char> c #\\z)))
      (int-char (+ (char-int #\A)
		   (- (char-int c)
		      (char-int #\\a))))
      c))

;;;; CHAR-DOWNCASE - lower a character
(defun char-downcase (c)
  (if (not (or (char< c #\A)
	       (char> c #\Z)))
      (int-char (+ (char-int #\\a)
		   (- (char-int c)
		      (char-int #\A))))
      c))

;;;; DIGIT-CHAR - convert character to digit (optional radix, bits, font NYI)
(defun digit-char (i)
  (when (and (>= i 0) (<= i 10))
        (int-char (+ (char-int #\0) i))))

;;;; CHAR-INT - convert character to integer
(defmacro char-int (c)
  ;; Identity operation in PSL
  c)

;;;; INT-CHAR - convert integer to character
(defmacro int-char (c)
  ;; Identity operation in PSL
  c)
