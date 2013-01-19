	SUBROUTINE DC_FCYL ( filnam, iflsrc, stnfil, iadstn, maxtim, 
     +			     iflno, nparm, parms, iret )
C************************************************************************
C* DC_FCYL								*
C*									*
C* This routine checks a list of open files for the requested file,	*
C* then either sets the unit number to one from the list or opens the 	*
C* new file. If the list contains open files, the oldest is closed	*
C* in order to open the new file, and the order is cycled for the	*
C* remaining files.							*
C*									*
C* DC_FCYL ( FILNAM, IFLSRC, STNFIL, IADSTN, MAXTIM, IFLNO, NPARM, 	*
C*	     PARMS, IRET )						*
C*									*
C* Input parameters:							*
C*	FILNAM		CHAR*		Requested file name		*
C*	IFLSRC		INTEGER		File source			*
C*	STNFIL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Max num of additional stations	*
C*					  for land data			*
C*					Ignored for ship data		*
C*	MAXTIM		INTEGER		Max num of times for land data	*
C*					Max num of reports for ship data*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		Unit number for requested file	*
C*	NPARM		INTEGER		Number of parameters		*
C*	PARMS (NPARM)	CHAR*4		Parameter list			*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/NMC	 8/95						*
C* D. Keiser/GSC	 4/96	Changed iftype = 5, added call to	*
C*				DC_OSFR, added iflsrc			*
C* S. Jacobs/NCEP	 5/96	Fixed all routine calls to add iflsrc	*
C* S. Jacobs/NCEP	 7/96	Updated documentation			*
C* S. Danz/AWC		 3/98	Added check for file not opened		*
C* A. Hardy/GSC		 5/99   Added type 6 for ASCII files            *
C* S. Chiswell/Unidata	11/02	Added path creation if necessary	*
C************************************************************************
	INCLUDE		'dccmn.cmn'
C*
	CHARACTER*(*)	filnam, stnfil, parms (*)
	CHARACTER*256	tmpfil
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for an existing match for the file name.
C
	DO  i = 1, maxfil
	    IF  ( filnam .eq. filopn (i) )  THEN
		iflno = lunopn (i)
		RETURN
	    END IF
	END DO
C
C*      Ensure full directory path exists and is accessible
C
	CALL ST_NULL ( filnam, tmpfil, lens, ier)
        CALL DC_DIRA ( tmpfil, ier )
C
C*	Open the file and assign to an unused item in the list.
C
	DO  i = 1, maxfil
	    IF  ( numopn (i) .eq. 0 )  THEN
		IF  ( iftype .eq. 1 )  THEN
		    CALL DC_OSFC  ( filnam, iflsrc, stnfil, iadstn, 
     +				    maxtim, iflno, nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 2 )  THEN
		    CALL DC_OSHP  ( filnam, iflsrc, maxtim, iflno, 
     +				    nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 3 )  THEN
		    CALL DC_OSND  ( filnam, iflsrc, stnfil, iadstn, 
     +				    maxtim, iflno, iret )
		  ELSE IF  ( iftype .eq. 4 )  THEN
		    CALL DC_OSNM  ( filnam, iflsrc, stnfil, iadstn, 
     +				    maxtim, iflno, nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 5 )  THEN
		    CALL DC_OSFR  ( filnam, iflsrc, stnfil, iadstn, 
     +				    maxtim, iflno, nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 6 )  THEN
		    CALL FL_SWOP ( filnam, iflno, iret )
		    CALL FL_APND ( iflno, iret )
		END IF
		IF  ( iret .eq. 0 )  THEN
		    filopn (i) = filnam
		    lunopn (i) = iflno
		    DO  j = 1, maxfil
			numopn (i) = MAX ( numopn (i), numopn (j) )
		    END DO
		    numopn (i) = numopn (i) + 1
		END IF
		RETURN
	    END IF
	END DO
C
C*	Close the oldest file and open the new file it its place.
C*	Cycle the order of the other open files.
C
	DO  i = 1, maxfil
	    IF  ( numopn (i) .eq. 1 )  THEN
		IF  ( iftype .eq. 1 )  THEN
		    CALL SF_CLOS  ( lunopn (i), ier )
		    CALL DC_OSFC  ( filnam, iflsrc, stnfil, iadstn,
     +				    maxtim, iflno, nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 2 )  THEN
		    CALL SF_CLOS  ( lunopn (i), ier )
		    CALL DC_OSHP  ( filnam, iflsrc, maxtim, iflno,
     +				    nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 3 )  THEN
		    CALL SN_CLOS  ( lunopn (i), ier )
		    CALL DC_OSND  ( filnam, iflsrc, stnfil, iadstn,
     +				    maxtim, iflno, iret )
		  ELSE IF  ( iftype .eq. 4 )  THEN
		    CALL SN_CLOS  ( lunopn (i), ier )
		    CALL DC_OSNM  ( filnam, iflsrc, stnfil, iadstn,
     +				    maxtim, iflno, nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 5 )  THEN
		    CALL SF_CLOS  ( lunopn (i), ier )
		    CALL DC_OSFR  ( filnam, iflsrc, stnfil, iadstn,
     +				    maxtim, iflno, nparm, parms, iret )
		  ELSE IF  ( iftype .eq. 6 )  THEN
		    CALL FL_CLOS ( lunopn (i), iret )
		    CALL FL_SWOP ( filnam, iflno, iret )
		    CALL FL_APND ( iflno, iret )
		END IF
		IF  ( iret .eq. 0 )  THEN
		    filopn (i) = filnam
		    lunopn (i) = iflno
		    numopn (i) = maxfil
		  ELSE
		    filopn (i) = ' '
		    lunopn (i) = 0
		    numopn (i) = 0
		END IF
	    ELSE
		numopn (i) = numopn (i) - 1
	    END IF
	END DO
C*
	RETURN
	END
