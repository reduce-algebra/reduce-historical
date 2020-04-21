%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Ordinary LISP part of the heap statistics gathering package, HEAP-STATS.
%%% Load this file to get the package.
%%% The top-level function is collect-stats.  See its description.
%%% 
%%% Author: Cris Perdue
%%% December 1982
%%% Documented and cleaned up a litte, January 1983
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

(compiletime (load if))

(load h-stats-1 get-heap-bounds)

%%% An object that holds a complete set of statistics for the heap
%%% at some moment in time.  When one of these is created, the
%%% instance variable "template" must be initialized, and the
%%% template must be a "histogram template" as discussed below.

%%% Maintainer note: the code that actually gathers statistics assumes
%%% that the heap-stats object is a vector (or evector) with a header,
%%% 2 items of data allocated by the objects package, then the data shown
%%% here, in order.
(defflavor heap-stats
  (template
   string-count
   string-space
   vector-count
   vector-space
   wordvec-count
   wordvec-space
   (pairs 0)
   (strings 0)
   (halfwords 0)
   (wordvecs 0)
   (vectors 0))
  ()
  (initable-instance-variables template)
  gettable-instance-variables)

(defmethod (heap-stats init) (init-plist)
  (if (not (vectorp template)) then
      (error 0 "The TEMPLATE of a HEAP-STATS object must be initialized."))
  (let ((s (+ (size template) 1)))
    (setf string-count (make-vector s 0))
    (setf string-space (make-vector s 0))
    (setf vector-count (make-vector s 0))
    (setf vector-space (make-vector s 0))
    (setf wordvec-count (make-vector s 0))
    (setf wordvec-space (make-vector s 0))))

(global '(old-!%reclaim stats-channel))

%%% This method prints statistics on a particular snapshot of the heap
%%% onto the given channel.
(defmethod (heap-stats print-stats) (channel)
  (channelprintf
   channel
   "%w pairs, %w strings, %w vectors, %w wordvecs, %w halfwordvecs%n%n"
   pairs strings vectors wordvecs halfwords)
  (for (in table (list string-count vector-count))
       (in spacetable (list string-space vector-space))
       (in title '("STRINGS" "VECTORS"))
       (do
	(channelprintf channel "%w%n%n" title)
	(print-histo template table spacetable channel)
	(channelterpri channel)
	(channelterpri channel))))

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% Prints a single histogram onto the given channel.  Arguments
%%% are the template from which the histogram was generated, a
%%% corresponding table with a count of the number of objects of
%%% each size range, and another corresponding table with the
%%% total space occupied by the objects within each size range.
(defun print-histo (template table spacetable channel)
  (channelprintf channel
		 "Size <= n%tHow many%tStorage items used%n" 12 24)
  (channelprintf channel
		 "------------------------------------------%n")
  (for (from i 0 (size template))
       (do (channelprintf channel
			  "%w%t%w%t%w%n" (indx template i) 12
			  (indx table i) 24 (indx spacetable i))))
  (channelprintf channel
		 "> %w%t%w%t%w%n"
		 (indx template (size template)) 12
		 (indx table (+ (size template) 1)) 24
		 (indx spacetable (+ (size template) 1))))

(fluid '(before-stats after-stats print-stats? stdtemplate))

%%% This function initializes the collecting of statistics and
%%% printing them to a file.  The name of the file is the
%%% argument to collect-stats.  NIL rather than a string for the file
%%% name turns statistics collection off.  In statistics collection mode
%%% statistics are gathered just before and after each garbage collection.
(defun collect-stats (file)
  (if (and file (not old-!%reclaim)) then
      (if (not (and (eq (object-type before-stats) 'heap-stats)
		    (eq (object-type after-stats) 'heap-stats))) then
	  (printf "Caution: before- and after-stats are not both bound.%n"))
      (setq old-!%reclaim (cdr (getd '!%reclaim)))
      (setq stats-channel (open file 'output))
      (putd '!%reclaim
	    'expr
	    '(lambda ()
	       (heapstats before-stats)
	       (apply old-!%reclaim nil)
	       (heapstats after-stats)
	       (channelprintf stats-channel "BEFORE RECLAIMING%n%n")
	       (=> before-stats print-stats stats-channel)
	       (channelterpri stats-channel)
	       (channelprintf stats-channel "AFTER RECLAIMING%n%n")
	       (=> after-stats print-stats stats-channel)))
      elseif (and (not file) old-!%reclaim) then
      (close stats-channel)
      (putd '!%reclaim 'expr old-!%reclaim)
      (setq old-!%reclaim nil)
      elseif old-!%reclaim then
      (printf "Statistics collecting is apparently already turned on.%n")
      else
      (printf "Statistics collecting is apparently already off.%n")
      (printf "Trying to close the channel anyway.%n")
      (close stats-channel)))

%%% This is initialized here to be a reasonable histogram template for
%%% statistics on heap usage.  A histogram template is a vector of
%%% integers that define the buckets to be used in collecting the
%%% histogram data.  All values less than or equal to template[0]
%%% go into data[0].  Of those values that do not go into data[0],
%%% all less than or equal to template[1] go into data[1], etc..
%%% The vector of data must have at least one more element that
%%% the template does.  All values greater than the last value in
%%% the template go into the following element of the data vector.
(setq StdTemplate
      (make-vector 27 0))

(for (from i 0 16)
     (do (setf (indx StdTemplate i) i)))

(for (from i 17 27)
     (for k 32 (* k 2))
     (do (setf (indx StdTemplate i) k)))

(setq before-stats (make-instance 'heap-stats 'template StdTemplate))

(setq after-stats (make-instance 'heap-stats 'template StdTemplate))
