	SUBROUTINE PC_METH  ( index, ninpm, prmin, noutpm, prmout,
     +			      cmpflg, np, iret )
C************************************************************************
C* PC_METH								*
C*									*
C* This subroutine decides how a set of output parameters can be	*
C* computed from a set of input parameters.  Internal tables store	*
C* the information needed to perform the computations.			*
C*									*
C* PC_METH ( INDEX, NINPM, PRMIN, NOUTPM, PRMOUT, CMPFLG, NP, IRET )	*
C*									*
C* Input parameters:							*
C*	INDEX		INTEGER		Table index			*
C*					  1 = basic level parms		*
C*					  2 = intermed. level parms	*
C*					  3 = final level parms		*
C*					  4 = vertical coordinate parms	*
C*					  5 = basic stability parms	*
C*					  6 = intermed. stability parms	*
C*					  7 = final stability parms	*
C*	NINPM		INTEGER		Number of input parameters	*
C*	PRMIN  (NINPM)	CHAR*4		Input parameters		*
C*	NOUTPM		INTEGER		Number of output parameters	*
C*	PRMOUT (NOUTPM) CHAR*4		Output parameters		*
C*									*
C* Output parameters:							*
C*	CMPFLG (NOUTPM)	LOGICAL		Computable parameter flag	*
C*	NP		INTEGER		Number of computable parms	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = invalid # of in parms	*
C*					 -5 = invalid # of out parms	*
C*					-30 = invalid index		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/84						*
C* M. desJardins/GSFC	11/87	Changed character conversion method	*
C* M. desJardins/GSFC	 8/88	Cleaned up				*
C* G. Huffman/USRA	10/89	Documentation				*
C* S. Jacobs/EAI	 3/93	Changed call to TB_PCNV			*
C* D. Keiser/GSC	12/95	Changed PCVTBL to 'pcconv.tbl'		*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* J. Wu/GSC             7/00   Added checks for kans(j, index) before  *
C*                              proceeding to fnctbl                    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'pccmn.cmn'
C*
	CHARACTER*(*)	prmin  (*), prmout (*)
	LOGICAL		cmpflg (*)
C
C*	These variables are used temporarily in generating tables.
C*	They are referred to as the computable function table.
C*	They represent function names, corresponding parameter names,
C*	and pointers to the arguments of the functions.
C
	CHARACTER*8	fnctbl (MAXFNC)
	CHARACTER*4	prmtbl ( MAXFNC )
	INTEGER		jposno ( MAXPPF, MAXFNC )
C*
	LOGICAL		ok, more
	INTEGER		list ( MAXTMP )
C*
	CHARACTER*4	tmdw1 (4), tmdw2 (4)
	DATA		tmdw1  / 'TEMP', 'TMPC', 'DWPT', 'DWPC' /
	DATA		tmdw2  / 'TMPC', 'TEMP', 'DWPC', 'DWPT' /
C-------------------------------------------------------------------------
C	Check for input errors
C
	IF  ( (ninpm .le. 0) .or. (ninpm .gt. MAXPRM) )  THEN
	    iret = -1
	  ELSE IF  ( (noutpm .le. 0) .or. (noutpm .gt. MAXPRM) )  THEN
	    iret = -5
	  ELSE IF  ( (index .lt. 1)  .or. (index .gt. MAXTBL) )   THEN
	    iret = -30
	  ELSE
	    iret = 0
	END IF
	IF  ( iret .lt. 0 )  RETURN
C
C*	Initialize permanent and temporary tables.
C
	kinpm  (index) = ninpm
	koutpm (index) = noutpm
	kfunc  (index) = 0
	DO  i = 1, MAXFNC
	    kfuncn ( i, index ) = ' '
	    fnctbl ( i ) = ' '
	    prmtbl ( i ) = ' '
	    koutfn ( i, index ) = 0
	    DO  j = 1, MAXPPF
		kposno ( j, i, index ) = 0
		jposno ( j, i ) = 0
	    END DO
	END DO
C
	DO  i = 1, MAXPRM
	    kans (i, index) = 0
	END DO
C
C*	Initialize output flag array
C
	DO  i = 1, noutpm
	    cmpflg (i) = .false.
	END DO
C
C*	Initialize computable parameter variables with input parameters
C*	(no function name needed).
C
	DO  i = 1, ninpm
	    prmtbl (i) = prmin (i)
	    fnctbl (i) = ' '
	END DO
	ntable = ninpm
C
C*	If TEMP, TMPC, DWPT or DWPC are in dataset, add corresponding
C*	parameter to the computable list.  The argument of SAMEPARM is
C*	the parameter actually in the data set (with index IPOS).
C
	DO  i = 1, 4
	    CALL ST_FIND  ( tmdw1 (i), prmtbl, ninpm, ipos, ier )
	    IF  ( ipos .ne. 0 )  THEN
		ntable               = ntable + 1
		prmtbl ( ntable )    = tmdw2 (i)
		fnctbl ( ntable )    = 'SAMEPARM'
		jposno ( 1, ntable ) = ipos
	    END IF
	END DO
C
C*	Set local variables: ntable = #of comp. parms,
C*			     ntbold = #of comp. parms on the last pass;
C*			     nfound = #of parms found
C
	ntbold = 0
	nfound = 0
C
C*	This loop searches for the requested parameters in the
C*	computable parameter table until all parameters are found or
C*	no more computable parameters are being added to the table.
C
	more = .true.
	DO WHILE  ( ( nfound .lt. noutpm ) .and.
     +		    ( ntbold .lt. ntable ) .and. ( more ) )
C
	    num = ntable - ntbold
	    i   = 1
C
C*	    Check each output parameter against new parms in
C*	    computable table.
C
	    DO WHILE  ( ( nfound .lt. noutpm ) .and. ( i .le. noutpm ) )
C
		IF  ( .not. cmpflg (i) )  THEN
		    CALL ST_FIND  ( prmout(i), prmtbl(ntbold+1), num,
     +				    ipos, ier )
		    IF  ( ipos .gt. 0 )  THEN
			cmpflg (i)        = .true.
			kans ( i, index ) = ntbold + ipos
			nfound            = nfound + 1
		    END IF
		END IF
		i = i + 1
	    END DO
C
C*	    Read in the function table if all parameters are not found,
C*	    and it has not been read in previously.
C
	    IF  ( ( nfound .lt. noutpm ) .and. (.not. tablrd ) )  THEN
		CALL TB_PCNV  ( 'pcconv.tbl', MAXFNC, jtfunc, tparms,
     +				tfuncs, tplist, ier )
		tablrd = .true.
	    END IF
C
C*	    Check to see if any of the parameters not already found
C*	    can now be computed.
C
	    ntbold = ntable
	    IF  ( nfound .lt. noutpm )  THEN
		i    = 1
		more = .false.
		DO  WHILE ( (nfound .lt. noutpm) .and. (i .le. noutpm) )
		    IF  ( .not. cmpflg (i) )  THEN
			ipos = 1
			DO  WHILE ((ipos .ne. 0).and.(ipos .le. jtfunc))
			    jnum = jtfunc - ipos + 1
			    kpos = ipos
			    CALL ST_FIND  ( prmout (i), tparms (kpos),
     +					    jnum, ipos, ier )
C
C*			    Parameter found inn function table; test
C*			    whether all arguments are computable
C*			    (storing pointers along the way).
C
			    IF  ( ipos .ne. 0 )  THEN
				ipos = ipos + kpos - 1
				ok   = .true.
				ii   = 1
				DO  WHILE ( (ii .le. MAXPPF) .and.
     +					    (tplist (ii,ipos) .ne. ' '))
				    CALL ST_FIND  ( tplist (ii,ipos),
     +						    prmtbl, ntable,
     +						    jpos, ier )
				    jposno ( ii, ntable+1 ) = jpos
				    IF  ( jpos .eq. 0 )  ok = .false.
				    ii = ii + 1
				END DO
C
C*				Parameter and all arguments have been
C*				found.  Add to the table and set
C*				computable flag.
C
				IF  ( ok )  THEN
				    ntable          = ntable + 1
				    prmtbl (ntable) = tparms (ipos)
				    fnctbl (ntable) = tfuncs (ipos)
				    cmpflg (i)      = .true.
				    kans (i, index) = ntable
				    nfound          = nfound + 1
				    ipos            = 0
C
C*				  Some arguments uncomputable; reset
C*				  flag and pointer to continue search.
C
				  ELSE
				    more = .true.
				    ipos = ipos + 1
				END IF
			    END IF
			END DO
		    END IF
		    i = i + 1
		END DO
	    END IF
C
C*	    Check to see if any more parameters can now be computed.
C
	    IF  ( ( nfound .lt. noutpm ) .and. ( more ) )  THEN
		knt = 1
		DO  WHILE ( knt .le. jtfunc )
C
C*		    If a parameter in the function table is not in the
C*		    computable list check whether its arguments are now
C*		    computable (storing pointers along the way).
C
		    CALL ST_FIND ( tparms(knt), prmtbl, ntable, ipos,
     +								ier )
		    IF  ( ipos .eq. 0 )  THEN
			ok = .true.
			i  = 1
			DO  WHILE ( (i .le. MAXPPF) .and.
     +				    (tplist (i,knt) .ne. ' ') )
			    CALL ST_FIND  ( tplist (i,knt), prmtbl,
     +					    ntable, ipos, ier )
			    jposno ( i, ntable+1 ) = ipos
			    IF  ( ipos .eq. 0 )  ok = .false.
			    i = i + 1
			END DO
C
C*			All arguments computable -- a new parameter has
C*			been found and is added to the table.
C
			IF  ( ok )  THEN
			    ntable          = ntable + 1
			    prmtbl (ntable) = tparms (knt)
			    fnctbl (ntable) = tfuncs (knt)
			END IF
		    END IF
		    knt = knt + 1
		END DO
	    END IF
	END DO
C
C*	Fill in the function table in common which will be used to
C*	compute parameters.  Input parameters and special corresponding
C*	temp/dewpoint parameters were already loaded above.
C
C*	LIST flags those functions that need to be executed.  First, use
C*	a 2 to flag functions not an input parameter which are used
C*	as an answer.
C
	DO  i = 1, MAXTMP
	    list (i) = 0
	END DO
C
	max = 0
	DO  i = 1, noutpm
	    k = kans (i, index)
	    IF  ( k .gt. ninpm ) THEN
		list (k) = 2
		IF  ( k .gt. max ) max = k
	    END IF
	END DO
C
C*	Now flag answers not in the input data with 2, use 1 to flag
C*	arguments not in the input.
C
	DO  WHILE ( max .gt. 0 )
	    nmax = 0
	    DO  i = max, ninpm+1, -1
		IF  ( list (i) .eq. 2 )  THEN
		    list (i) = 1
		    DO  j = 1, maxppf
			kk = jposno (j,i)
			IF  ( (kk .gt. ninpm) .and. (list(kk) .ne. 1) )
     +								THEN
			    list (kk) = 2
			    IF  ( kk .gt. max ) nmax = kk
			END IF
		    END DO
		END IF
	    END DO
	    max = nmax
	END DO
C
C*	Fill computable function table with computable functions used.
C
	DO  k = ninpm+1, MAXTMP
	    IF  ( list (k) .ne. 0 ) THEN
		if                   = kfunc (index) + 1
		kfunc (index)        = if
		kfuncn ( if, index ) = fnctbl (k)
		DO  j = 1, MAXPPF
		    kposno (j, if, index) = jposno (j, k)
		END DO
		koutfn ( if, index ) = k
	    END IF
	END DO
C
C*	Fill in table with types for output parameters.
C
	CALL PC_CKPM  ( index, noutpm, prmout, ier )
C
C*	Find character parameters.
C
	IF  ( index .eq. 1 )  THEN
	    DO  j = 1, noutpm
		qchr   (j) = .false.
		chrfnc (j) = ' '
		IF  ( kans (j,index) .ne. 0 ) THEN  
                     IF ( fnctbl ( kans (j,index) ) (1:2) .eq. 'PT' ) 
     +								THEN
		          qchr   (j) = .true.
		          chrfnc (j) = fnctbl ( kans (j,index) )
		     END IF
		END IF
	    END DO
	END IF
C
C*	set final output variables.
C
	np               = nfound
	kfound ( index ) = nfound
	tabflg (index)   = .true.
C*
	RETURN
	END
