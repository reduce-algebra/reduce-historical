%
% KERNEL.SL - Generate scripts for building PSL kernel
% 
% Author:      Eric Benson
%              Symbolic Computation Group
%              Computer Science Dept.
%              University of Utah
% Date:        26 May 1982
% Copyright (c) 1982 University of Utah
%

% <PSL.UTIL>KERNEL.SL.2, 20-Dec-82 11:21:03, Edit by BENSON
% Added kernel-header and kernel-trailer
% <PSL.UTIL>KERNEL.SL.9,  7-Jun-82 12:22:48, Edit by BENSON
% Changed kernel-file to all-kernel-script-name* and all-kernel-script-format*
% <PSL.UTIL>KERNEL.SL.8,  6-Jun-82 05:23:40, Edit by GRISS
% Added kernel-file

(compiletime (load useful))

(compiletime (flag '(build-link-script build-kernel-file
		     build-init-file build-file-aux
		     insert-file-names insert-file-names-aux)
	           'InternalFunction))

(fluid '(kernel-name-list*
	 command-file-name*
	 command-file-format*
	 init-file-name*
	 init-file-format*
         all-kernel-script-name*
	 all-kernel-script-header*
	 all-kernel-script-format*
	 all-kernel-script-trailer*
	 code-object-file-name*
	 data-object-file-name*
	 link-script-name*
	 link-script-format*
	 script-file-name-separator*))

(de kernel (kernel-name-list*)
  (let ((*lower t))			% For the benefit of Unix
       (build-command-files kernel-name-list*)
% MAIN is not included in all-kernel-script
       (build-kernel-file (delete 'main kernel-name-list*))
       (build-link-script)
       (build-init-file)))

(de build-command-files (k-list)
  (unless (null k-list)
    (let ((name-stem (first k-list)))
	 (let ((f (wrs (open (bldmsg command-file-name* name-stem)
			     'output))))
	      (printf command-file-format* name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem
					   name-stem)
	      (close (wrs f))))
	  (build-command-files (rest k-list))))

(de build-link-script ()
  (let ((f (wrs (open link-script-name* 'output))))
       (linelength 1000)
       (printf link-script-format* '(insert-link-file-names)
	 			   '(insert-link-file-names)
	 			   '(insert-link-file-names)
	 			   '(insert-link-file-names)
	 			   '(insert-link-file-names)
				   '(insert-link-file-names))
       (close (wrs f))))

(de build-kernel-file (n-list)
  (let ((f (wrs (open all-kernel-script-name* 'output))))
       (linelength 1000)
       (unless (null all-kernel-script-header*)
	       (prin2 all-kernel-script-header*))
       (build-file-aux n-list all-kernel-script-format*)
       (unless (null all-kernel-script-trailer*)
	       (prin2 all-kernel-script-trailer*))
       (close (wrs f))))

(de insert-link-file-names ()
  (insert-file-names kernel-name-list* code-object-file-name*)
  (prin2 script-file-name-separator*)
  (insert-file-names kernel-name-list* data-object-file-name*))

(de insert-file-names (n-list format)
  (printf format (first n-list))
  (insert-file-names-aux (rest n-list) format))

(de insert-file-names-aux (n-list format)
  (unless (null n-list)
          (prin2 script-file-name-separator*)
	  (printf format (first n-list))
	  (insert-file-names-aux (rest n-list) format)))

(de build-init-file ()
  (let ((f (wrs (open init-file-name* 'output))))
       (build-file-aux kernel-name-list* init-file-format*)
       (close (wrs f))))

(de build-file-aux (n-list format)
  (unless (null n-list)
	  (printf format (first n-list))
	  (build-file-aux (rest n-list) format)))
