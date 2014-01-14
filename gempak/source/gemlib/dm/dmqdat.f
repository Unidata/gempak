	SUBROUTINE DM_QDAT  ( iflno, irow, icol, part, datflg, iret )
C************************************************************************
C* DM_QDAT								*
C*									*
C* This subroutine sets a flag indicating whether data for a given	*
C* row, column and part is stored in a DM file.				*
C*									*
C* DM_QDAT  ( IFLNO, IROW, ICOL, PART, DATFLG, IRET )			*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		File number			*
C*	IROW		INTEGER		Row number			*
C*	ICOL		INTEGER		Column number			*
C*	PART		CHAR*4		Part name			*
C*									*
C* Output parameters:							*
C*	DATFLG		LOGICAL		Data present flag		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C*					 -6 = write error		*
C*					 -7 = read error		*
C*					 -9 = invalid location		*
C*					-10 = invalid part name		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/87						*
C* S. Jacobs/NCEP	 8/13	Added checks for non-gempak files	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'dmcmn.cmn'
C
	CHARACTER*(*)	part
	LOGICAL		datflg
C------------------------------------------------------------------------
	datflg = .false.
C
C*	Check that file is open.
C
	CALL DM_CHKF ( iflno, iret )
	IF  ( iret .ne. 0 ) RETURN
C
C*	Check for a standard file.
C
	IF  ( .not. stdgem(iflno) )  THEN
	    iret = -36
	    RETURN
	END IF
C
C*	Check for valid row and column positions.
C
	IF  ( ( irow .lt. 1 ) .or. ( irow .gt. krow (iflno) ) .or.
     +	      ( icol .lt. 1 ) .or. ( icol .gt. kcol (iflno) ) ) THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Get part number.
C
	iprt = 0
	DO  i = 1, kprt ( iflno )
	    IF  ( kprtnm ( i, iflno ) .eq. part ) iprt = i
	END DO
	IF  ( iprt .eq. 0 )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Get pointer to data.
C
	ipoint = kpdata (iflno) + (irow-1) * kcol (iflno) * kprt (iflno)
     +		 + (icol-1) * kprt (iflno) + (iprt-1)
	CALL DM_RINT  ( iflno, ipoint, 1, istart, iret )
C
C*	Check to see if there is data.
C
	IF  ( istart .ne. 0 )  datflg = .true.
C*
	RETURN
	END
