	SUBROUTINE LV_INPT  ( level, nexp, vcoord, nlev, rlevel, 
     +			      levtyp, vparm, ivert, mandat, iret )
C************************************************************************
C* LV_INPT								*
C*									*
C* This subroutine converts the user input for LEVELS and VCOORD	*
C* into a list of levels.  The input for LEVELS may be a list 		*
C* separated by semicolons.  The following items may be included	*
C* in the list:								*
C*									*
C*             a single level;						*
C*             MAN for the mandatory levels below 100 mb;		*
C*             VAS for the standard VAS levels;				*
C*             a range of levels with an increment.			*
C*									*
C* The following items are also valid, provided they are not part of	*
C* a list:								*
C*									*
C*             ALL for all levels;					*
C*             a range of levels without an increment.			*
C*									*
C* If a range without an increment is entered, the limits will be	*
C* returned in RLEVEL and LEVTYP will be set to 2.			*
C*									*
C* If MAN or VAS is input, the input vertical coordinate must be	*
C* PRES.  The names SFC and TOP may be used.  They will be translated	*
C* into 0 and -1, respectively.						*
C*									*
C* The flag /MAN may be used after any specification of LEVELS so that  *
C* any application will use only mandatory level data with no           *
C* interpolation.                                                       *
C*                                                                      *
C* LV_INPT  ( LEVEL, NEXP, VCOORD, NLEV, RLEVEL, LEVTYP, VPARM,		*
C*            IVERT, MANDAT, IRET )					*
C*									*
C* Input parameters:							*
C*	LEVEL		CHAR*		Input for LEVEL			*
C*	NEXP		INTEGER		Maximum number of levels 	*
C*	VCOORD		CHAR*		Input for VCOORD		*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NLEV)	REAL		Levels or range			*
C*	LEVTYP		INTEGER		Level type			*
C*					  0 = no levels input		*
C*					  1 = list of levels		*
C*					  2 = range of levels		*
C*	VPARM		CHAR*		Vertical coordinate		*
C*	IVERT		INTEGER		Numerical vertical coord	*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*					  4 = SGMA			*
C*					  5 = DPTH			*
C*	MANDAT		LOGICAL		Mandatory data only flag	*
C*	IRET		INTEGER		Return code			*
C*					  1 = too many levels		*
C*					  0 = normal return		*
C*					 -2 = MAN, VAS need PRES cord	*
C*					 -3 = invalid VCOORD		*
C*					 -4 = invalid input for LEVEL	*
C*					 -5 = range w inc can't have 0	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/86						*
C* M. desJardins/GSFC	 9/88	Rewritten for GEMPAK4			*
C* M. desJardins/GSFC	 1/89	Added LV_SORT for range without inc	*
C* G. Huffman/GSC	 4/89	Error -5				*
C* K. Brill/NMC		 4/91	Documented SGMA and DPTH		*
C* K. Brill/NMC		05/93	CALL ST_LCUC before LV_CORD		*
C* S. Jacobs/NMC	 3/95	Added check for mand data only flag	*
C* S. Jacobs/NMC	 9/95	Fixed a bug by declaring mandat		*
C* D. Kidwell/NCEP	 5/99	Added /MAN info to prologue             *
C************************************************************************
	CHARACTER*(*)	level, vcoord, vparm
	REAL		rlevel (*)
	LOGICAL		mandat
C*
	CHARACTER	clevel*72, start*8, stop*8, inc*8, 
     +			clist (50)*20, ccc*20
C------------------------------------------------------------------------
	iret   = 0
	nlev   = 0
	levtyp = 0
	nummax = MIN  ( nexp, 50 )
C
C*	Check that nexp is positive.
C
	IF  ( nexp .le. 0 )  THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Get the vertical coordinate type.
C
	CALL ST_LCUC  ( vcoord, vparm, ier )
	CALL LV_CORD  ( vparm, vparm, ivert, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Convert the string to upper case.
C
	CALL ST_LCUC  ( level, clevel, ierr )
C
C*	Check for use of mandatory data only. Do not merge with
C*	significant level data.
C
	CALL ST_RMST  ( clevel, '/MAN', ipos, clevel, ier )
	IF  ( ipos .eq. 0 )  THEN
	    mandat = .false.
	ELSE
	    mandat = .true.
	END IF
C
C*	Check for "ALL".
C
	IF  ( clevel .eq. 'ALL' )  THEN
	    IF  ( nexp .gt. 2 )  THEN
		levtyp = 2
		nlev   = 2
		rlevel (1) = 0.
		rlevel (2) = -1.
	      ELSE
		iret = -4
	    END IF
	    RETURN
	END IF
C
C*	Check if the input was a range without increment.
C
	CALL ST_RANG  ( clevel, start, stop, inc, itype, ierr )
	IF  ( itype .eq. 1 )  THEN
	    CALL LV_DECD  ( start, rlev1, ier1 )
	    CALL LV_DECD  ( stop,  rlev2, ier2 )
	    IF  ( ( nexp .lt. 2 ) .or. ( ier1 .ne. 0 ) .or.
     +		  ( ier2 .ne. 0 ) )  THEN
		iret = -4
	      ELSE
		levtyp = 2
		nlev   = 2
		rlevel (1) = rlev1
		rlevel (2) = rlev2
		CALL LV_SORT  ( ivert, nlev, rlevel, ier )
		IF  ( nlev .eq. 1 )  levtyp = 1
	    END IF
	    RETURN
	END IF
C
C*	Break input for LEVEL into list.  Eliminate blank parts.
C*	Check that list is not empty.
C
	CALL ST_LSTR  ( clevel, lenc, ier )
	CALL ST_CLST  ( clevel, ';', ' ', 50, clist, nlist, ier )
	knt = 0
	DO  i = 1, nlist
	    IF  ( clist (i) .ne. ' ' )  THEN
		knt = knt + 1
		clist (knt) = clist (i)
	    END IF
	END DO
	nlist = knt
	IF  ( nlist .eq. 0 )  THEN
	    iret = -4
	    RETURN
	END IF
C
C*	Check each part of list and add to levels.
C
	DO  i = 1, nlist
	    ccc = clist (i)
C
C*	    Check for MAN.
C
	    IF  ( ccc .eq. 'MAN' )  THEN
		IF  ( ivert .ne. 1 )  THEN
		    iret = -2
		    RETURN
		END IF
		nleft = nexp - nlev
		CALL LV_MANL  ( nleft, nl, rlevel (nlev+1), iret )
		nlev = nlev + nl
C
C*	    Check for "VAS".
C
	      ELSE IF  ( ccc .eq. 'VAS' )  THEN
		IF  ( ivert .ne. 1 )  THEN
		    iret = -2
		    RETURN
		END IF
		nleft = nexp - nlev
		CALL LV_VASL  ( nleft, nl, rlevel (nlev+1), iret )
		nlev = nlev + nl
C
C*		Check for range with increment or single item.
C
	      ELSE
C
C*		Check if the input was a range.
C
		CALL ST_RANG  ( ccc, start, stop, inc, itype, ier )
C
C*		Range without increment is an error.
C
		IF  ( itype .eq. 1 )  THEN
		    iret = -4
		    RETURN
C
C*		    Get levels for range with an increment.
C
		  ELSE IF  ( itype .eq. 2 )  THEN
		    nleft = nexp - nlev
		    CALL LV_GRNG  ( start, stop, inc, nleft,
     +				    rlevel (nlev+1), nl, iret )
		    nlev = nlev + nl
		    IF  ( iret .lt. 0 )  RETURN
C
C*		    Get individual levels.
C
		  ELSE
C
C*		    Decode this item.
C
		    CALL LV_DECD  ( ccc, rnum, ier )
		    IF  ( ier .ne. 0 )  THEN
			iret = -4
		      ELSE IF  ( nlev .lt. nexp )  THEN
			nlev = nlev + 1
			rlevel (nlev) = rnum
		      ELSE
			iret = 3
		    END IF
		END IF
	    END IF
	END DO
C
C*	Sort levels from surface to top of atmosphere eliminating
C*	duplicate levels.
C
	IF  ( nlev .eq. 0 )  THEN
	    iret = -4
	  ELSE
	    CALL LV_SORT  ( ivert, nlev, rlevel, iret )
	    levtyp = 1
	END IF
C*
	RETURN
	END
