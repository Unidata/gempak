	SUBROUTINE GDU_GLEV  ( level, nexp, vcoord, nlev, rlevel, 
     +			      levtyp, vparm, ivert, iret )
C************************************************************************
C* GDU_GLEV								* 
C*									*
C* This subroutine converts the user input for LEVELS and VCOORD	*
C* into a list of levels.  The input for LEVELS may be a list 		*
C* separated by semicolons.  The following items may be included	*
C* in the list:								*
C*									*
C*             a single level;						*
C*             MAN for the mandatory levels below 100 mb;		*
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
C* If MAN is input, the input vertical coordinate must be PRES.		*
C* The names SFC and TOP may be used.  They will be translated		*
C* into 0 and -1, respectively.						*
C*									*
C* GDU_GLEV  ( LEVEL, NEXP, VCOORD, NLEV, RLEVEL, LEVTYP, VPARM,	*
C*            IVERT, IRET )  						*
C*									*
C* Input parameters:							*
C*	LEVEL		CHAR*		Input for LEVEL			*
C*	NEXP		INTEGER		Maximum number of levels 	*
C*	VCOORD		CHAR*		Input for VCOORD		*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	RLEVEL (NEXP,2) REAL		Levels or range			*
C*	LEVTYP		INTEGER		Level type			*
C*					  0 = no levels input		*
C*					  1 = list of levels		*
C*					  2 = range of levels		*
C*					  3 = all levels		*
C*	VPARM		CHAR*		Vertical coordinate		*
C*	IVERT		INTEGER		Numerical vertical coord	*
C*					 -2 = ALL			*
C*					  0 = NONE			*
C*					  1 = PRES			*
C*					  2 = THTA			*
C*					  3 = HGHT			*
C*					  4 = SGMA			*
C*					  5 = DPTH			*
C*	IRET		INTEGER		Return code			*
C*					  1 = too many levels		*
C*					  0 = normal return		*
C*					 -1 = too many levels	        *
C*					 -2 = for input = MAN, need     *
C*					      PRES cord.                *
C*					 -3 = input = range w/inc       *
C*					 -4 = input = range w/out inc   *
C*					 -5 = list for LEVEL is empty	*
C*					 -6 = cannot decode indv. level	*
C**									*
C* Log:									*
C*									*
C* S. Maxwell 		9/96		Copied from LV_INPT		*
C************************************************************************
	CHARACTER*(*)	level, vcoord, vparm
	REAL		rlevel ( NEXP, 2 )
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
	    iret = -1
	    RETURN
	END IF
C
	DO i = 1, nexp
	   rlevel ( i, 2 ) = -1
	END DO
C
C*	Get the vertical coordinate type.
C
	CALL ST_LCUC  ( vcoord, vparm, ier )
C
	IF ( vparm .eq. 'ALL' ) THEN
	     ivert = -2
	ELSE
	     CALL LV_CORD  ( vparm, vparm, ivert, iret )
	ENDIF
C
	IF  ( iret .ne. 0 )  RETURN
C
C*	Convert the string to upper case.
C
	CALL ST_LCUC  ( level, clevel, ierr )
C
	IF ( clevel .eq. 'ALL' ) THEN
	     levtyp = 3
	     nlev   = 1
	     RETURN
C
	END IF 
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
C*	Check if the input was a range without increment.
C
	CALL ST_RANG  ( clevel, start, stop, inc, itype, ierr )
	IF  ( itype .eq. 1 )  THEN
	    CALL LV_DECD  ( start, rlev1, ier1 )
	    CALL LV_DECD  ( stop,  rlev2, ier2 )
	    IF  ( ( nexp .lt. 2 ) .or. ( ier1 .ne. 0 ) .or.
     +		  ( ier2 .ne. 0 ) )  THEN
		iret = -3
	      ELSE
		levtyp = 2
		nlev   = 2
		rlevel (1,1) = rlev1
		rlevel (2,1) = rlev2
		CALL LV_SORT  ( ivert, nlev, rlevel(1,1), ier )
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
	    iret = -5
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
		CALL LV_MANL  ( nleft, nl, rlevel (nlev+1,1), iret )
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
     +				    rlevel (nlev+1,1), nl, iret )
		    nlev = nlev + nl
		    IF  ( iret .lt. 0 )  RETURN
C
C*		    Get individual levels.
C
		  ELSE
		    icpos = INDEX ( ccc, ':' )
C
C*		    Decode this item.
		    IF ( icpos .eq. 0 ) THEN
C
		    CALL LV_DECD  ( ccc, rnum, ier )
		       IF  ( ier .ne. 0 )  THEN
			   iret = -6
		         ELSE IF  ( nlev .lt. nexp )  THEN
			   nlev = nlev + 1
			   rlevel (nlev,1) = rnum
		         ELSE
		           iret = 1
		       END IF
		    ELSE
C
C		      Check if the input was a layer.
C
		      IF ( nlev .lt. nexp ) then
		         nlev = nlev + 1 
		         CALL ST_CRNM ( ccc ( : icpos-1 ), 
     +		                        rlevel ( nlev, 1 ), ier )
			 CALL ST_CRNM ( ccc ( icpos+1 : ), 
     +					rlevel ( nlev, 2 ), ier )
		      END IF
		    END IF
		END IF
	    END IF
	END DO
C
	RETURN
	END
