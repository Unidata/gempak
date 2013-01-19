	SUBROUTINE JB_READ  ( ifn, parms, maxdta, dta, nprm, nlvl, iret )
C************************************************************************
C* JB_READ								*
C*									* 
C* This subroutine reads a Jack Woollen BUFR data file.			*
C*									*
C* To use this routine, first open a BUFR file using JB_OPEN.  Then,	*
C* call JB_NEXT to locate the next BUFR report.  Next, call JB_READ.	*
C* Refer to the BUFR Table associated with the file to specify a list	*
C* of parameters.  This list is stored in the string PARMS with the	*
C* individual names separated by spaces.  The names must be uppercase.	*
C* PARMS must be no more than 80 characters long.			*
C*									*
C* This subroutine returns the data values in the REAL*8 array DTA.	*
C* The values in DTA are arranged by parameter, in the order specified	*
C* in PARMS.  If NLVL > 1, then the parameter sequence repeats itself	*
C* NLVL times, giving a profile of data for all of the parameters.	*
C* Therefore, it is not allowed to mix profile and non-profile		*
C* parameter names in the string PARMS.  In this subroutine, DTA is	*
C* treated as a single-dimension array of length MAXDTA, specified by	*
C* the calling program.  The returned array may be treated as a two	*
C* dimensional array of dimensions (NPRM, NLVL) in any subsequent	*
C* subroutine calls.							*
C*									*
C* If the requested value is missing, the value in DTA is 1.0E+11.	*
C*									*
C* JB_READ  ( IFN, PARMS, MAXDTA, DTA, NPRM, NLVL, IRET )		*
C*									*
C* Input parameters:							*
C*	IFN		INTEGER		File unit number		*
C*	PARMS		CHAR*		String of parameter names	*
C*	MAXDTA		INTEGER		Number of elements in DTA	*
C*									*
C* Output parameters:							*
C*	DTA		REAL*8		Output data array		*
C*	NPRM		INTEGER		Number of parameters		*
C*	NLVL		INTEGER		Number of levels		*
C*	IRET		INTEGER		Return code			*
C*				 	 0 = normal return 		*
C*					-1 = no data read		*
C*					-2 = no parms requested		*
C**									*
C* Log:									*
C* K. Brill/EMC		10/96						*
C************************************************************************
	REAL*8		dta (*)
	CHARACTER*(*)	parms
C*
	CHARACTER	ctst*1
	LOGICAL		count
C*-----------------------------------------------------------------------
	iret = 0
C
C*	Count the number of parameters in the input string.
C
	nprm = 0
	count = .true.
	istop = LEN ( parms )
	DO i = 1, istop
	    ctst = parms ( i:i )
	    IF ( ctst .ne. ' ' .and. count ) THEN
		nprm = nprm + 1
		count = .false.
	    ELSE IF ( ctst .eq. ' ' .and. .not. count ) THEN
		count = .true.
	    END IF
	END DO
	IF ( nprm .le. 0 ) then
	    iret = -2
	    RETURN
	END IF
C
C*	Compute maximum second dimenstion of DTA.
C
	ny = maxdta / nprm
C*
	CALL UFBINT ( ifn, dta, nprm, ny, nlvl, parms )
	IF ( nlvl .le. 0 ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
