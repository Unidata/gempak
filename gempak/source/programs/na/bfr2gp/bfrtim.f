	SUBROUTINE BFRTIM ( iymdh, toff, dattim, dtmact, ihhmm, iret )
C************************************************************************
C* BFRTIM								*
C*									*
C* This subroutine computes the valid time of the BUFR data.		*
C*									*
C* BFRTIM  ( IYMDH, TOFF, DATTIM, DTMACT, IHHMM, IRET )			*
C*									*
C* Input parameters:							*
C*	IYMDH		INTEGER		Base time (YYMMDDHH)		*
C*	TOFF		REAL		Offset interval in seconds	*
C*									*
C* Output parameters:							*
C*	DATTIM		CHAR*		GEMPAK base time		*
C*	DTMACT		CHAR*		GEMPAK actual time		*
C*	IHHMM		INTEGER		Hour minute obs time		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					-22 = Cannot add offset		*
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C* K. Brill/EMC		 2/97	Added DTMACT and IHHMM			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	dattim, dtmact
C*
	INTEGER		itm (5), itm2 (5)
C-----------------------------------------------------------------------
	iret = 0
C*
	iyr = iymdh / 1000000
	IF ( iyr .lt. 77 ) THEN
	    iyr = iyr + 2000
	ELSE IF ( iyr .lt. 100 ) THEN
	    iyr = iyr + 1900
	END IF
	itm (1) = iyr
	itm (2) = MOD ( ( iymdh / 10000 ), 100 )
	itm (3) = MOD ( ( iymdh / 100 ), 100 )
	itm (4) = MOD ( iymdh, 100 )
	itm (5) = 0
	DO i = 1, 5
	    itm2 (i) = itm (i)
	END DO
	madd = NINT ( toff / 60. )
	IF ( madd .gt. 0 ) THEN
	    CALL TI_ADDM ( itm, madd, itm2, iret )
	ELSE IF ( madd .lt. 0 ) THEN
	    madd = -madd
	    CALL TI_SUBM ( itm, madd, itm2, iret )
	END IF
	IF ( iret .ne. 0 ) THEN
	    iret = -22
	    RETURN
	END IF
C
C*	Convert to GEMPAK character time.
C
	CALL TI_ITOC ( itm, dattim, iret )
	IF ( iret .ne. 0 ) iret = -22
	CALL TI_ITOC ( itm2, dtmact, iret )
	IF ( iret .ne. 0 ) iret = -22
	ihhmm = itm2 (4) * 100 + itm2 (5)
C*
	RETURN
	END
