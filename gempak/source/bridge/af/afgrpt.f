	SUBROUTINE AF_GRPT  ( bullx, lenbx, ibxptr, report, lenr, iret )
C************************************************************************
C* AF_GRPT								*
C*									*
C* This subroutine locates and returns the next report from within an	*
C* aircraft bulletin.  Upon entry, IBXPTR points to the character in	*
C* the bulletin with which to begin the search for the next report.	*
C*									*
C* AF_GRPT  ( BULLX, LENBX, IBXPTR, REPORT, LENR, IRET )		*
C*									*
C* Input parameters:							*
C*	BULLX		CHAR*		Text portion of bulletin 	*
C*	LENBX		INTEGER		Length of BULLX 		*
C*									*
C* Input and output parameters:						*
C*	IBXPTR		INTEGER		Pointer within BULLX 		*
C*									*
C* Output parameters:							*
C*	REPORT		CHAR*		Report 				*
C*	LENR		INTEGER		Length of report 		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return		*
C*					 -1 = no more reports in	*
C*					      bulletin			*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		11/96	Treat intermediate section of RECCO as	*
C*				separate report 			*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		08/99	Set iend = lenbx if can't find '=' sign	*
C* J. Ator/NCEP		 4/03	Skip leading blanks at start of report	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report, bullx
C*
	LOGICAL		start
C------------------------------------------------------------------------
	iret = 0
C
C*	Look for the next non-blank character in the bulletin
C*	as the start of the next report.
C
	start = .false. 
	DO WHILE ( .not. start )
	    IF  ( ibxptr .gt. lenbx )  THEN
		iret = -1
		RETURN
	    ELSE IF  ( bullx ( ibxptr : ibxptr ) .ne. ' ' )  THEN
		start = .true.
		istart = ibxptr
	    ELSE
		ibxptr = ibxptr + 1
	    END IF
	END DO 
C
C*	Look for the end of the next report.  If no end-of-report
C*	indicator is found, then set the last byte of the bulletin
C*	as the end of the next report.
C
	ipt1 = INDEX ( bullx ( istart : lenbx ), '=' )
	IF  ( ipt1 .ne. 0 )  THEN
	    iend = istart + ipt1 - 2
	ELSE
	    iend = lenbx
	END IF
	ibxptr = iend + 2
C
	IF  ( bultyp .eq. RECCO )  THEN
C
C*	    Check if the report contains an intermediate level section,
C*	    and, if so, then treat this section as a separate report.
C
	    IF  ( ( istart + 5 ) .lt. iend )  THEN
		ipt2 = INDEX ( bullx ( ( istart + 5 ) : iend ), '95559')
		IF  ( ipt2 .ne. 0 )  THEN
		    iend = istart + ipt2 + 3
		    ibxptr = iend + 1
		END IF
	    END IF
	END IF
C
C*	Set the output values.
C
	report = bullx ( istart : iend )
	lenr = iend - istart + 1
C*
	RETURN
	END
