	SUBROUTINE MN_TITL ( bultin, ibpnt, date, irhour, irmin, iret ) 
C************************************************************************
C* MN_TITL								*
C*									*
C* This subroutine decodes the model date/time from the NGM MOS         *
C* bulletin title line.  On input, ibpnt points to the first character  *
C* in the title line; on output, to the first character of the next     *
C* line.                                                             	*
C*									*
C* MN_TITL ( BULTIN, IBPNT, DATE, IRHOUR, IRMIN, IRET )		        *
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		NGM MOS bulletin		*
C*									*
C* Input and output parameters:						*
C*	IBPNT		INTEGER		Pointer in bulletin             *
C*									*
C* Output parameters:							*
C*	DATE		CHAR*		Model date			*
C*	IRHOUR		INTEGER		Model hour			*
C*	IRMIN		INTEGER		Model minute			*
C*	IRET		INTEGER		Return code			*
C*					 -3 = date not found            *
C**									*
C* Log:									*
C* D. Keiser/GSC	 2/96						*
C* F. J. Yen/NCEP	11/98	Converted from AV_TITL.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, date
C
	CHARACTER	rhour*2, rmin*2, record*80, carr(8)*10, time*10
C------------------------------------------------------------------------
	iret = 0
	date = ' '
C
C*	Extract and parse title line of the bulletin.
C
	ilf = INDEX ( bultin ( ibpnt: ), CHLF )
	IF ( ilf .gt. 0 ) THEN
	    record = bultin ( ibpnt:ibpnt + ilf - 1 )
	    CALL ST_CLST ( record, ' ', ' ', 8, carr, num, ier )
	    IF ( num .le. 2 ) THEN
C
C*		Not regular date record; must be Alaskan data. Skip rec.
C
		ibpnt = ibpnt + ilf
		ilf = INDEX ( bultin ( ibpnt: ), CHLF )
		IF ( ilf .gt. 0 ) THEN
		    record = bultin ( ibpnt:ibpnt + ilf - 1 )
		    CALL ST_CLST (record, ' ',' ', 8, carr, num, ier )
		END IF
	    END IF
C
C*	    Get date and time.
C
	    DO i = 2, num
	        IF ( ( INDEX ( carr (i), '/' ) ) .ne. 0 ) THEN
      		       date = carr (i)
		       time = carr (i + 1)
		END IF
	    END DO
	END IF
	ibpnt = ibpnt + ilf
	IF ( date ( 1:1 ) .eq. ' ' ) iret = -3
C
C*	Set hour and minute.
C
	rhour = time ( 1:2 )
	rmin = time ( 3:4 )
C
C*	Convert hour and minute to integers
C
	CALL ST_NUMB ( rhour, irhour, ier )
	CALL ST_NUMB ( rmin, irmin, ier )
C*
	RETURN
	END
