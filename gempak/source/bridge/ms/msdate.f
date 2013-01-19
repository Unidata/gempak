	SUBROUTINE MS_DATE ( bultin, ibpnt, date, irhour, irmin, stid,
     +			     iret ) 
C************************************************************************
C* MS_DATE								*
C*									*
C* This subroutine decodes the model date/time from the MOS report      *
C* title line and also returns the station id.  On input, ibpnt points  *
C* to the first character in the title line; on output, to the first    *
C* character of the initial data line.                                  *
C*									*
C* MS_DATE ( BULTIN, IBPNT, DATE, IRHOUR, IRMIN, STID, IRET )	        *
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		GFS or GFSX MOS bulletin	*
C*									*
C* Input and output parameters:						*
C*	IBPNT		INTEGER		Pointer in bulletin             *
C*									*
C* Output parameters:							*
C*	DATE		CHAR*		Model date			*
C*	IRHOUR		INTEGER		Model hour			*
C*	IRMIN		INTEGER		Model minute			*
C*	STID		CHAR*		Station id                      *
C*	IRET		INTEGER		Return code			*
C*					 -1 = badly formatted report    *
C*					 -3 = date not found            *
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00	                                        *
C* F. J. Yen/NCEP	10/01	Added 0600 and 1800 UTC                 *
C* m.gamazaychikov/SAIC 11/03   Replaced references to AVN/MRF with     *
C*                              references to GFS/GFSX                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	bultin, date, stid
C
	CHARACTER	carr (8)*10, time*10
C------------------------------------------------------------------------
	iret   = 0
	date   = ' '
	irhour = 0
	irmin  = 0
	stid   = ' '
C
C*	Skip the AWIPS ID.
C
	ilf   = INDEX ( bultin ( ibpnt: ), CHLF )
	ibpnt = ibpnt + ilf
C
C*      Get the station id.
C
	CALL MS_STID ( bultin, ibpnt, stid, carr, num, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*      Get the date and time.
C
	DO i = 2, num
	    IF ( ( INDEX ( carr ( i ), '/' ) ) .ne. 0 ) THEN
      	        date = carr ( i )
	        time = carr ( i + 1 )
	    END IF
	END DO
C
        IF ( date ( 1:1 ) .eq. ' ' ) THEN
	    iret = -3
	  ELSE
C
C*          Reset hour if time is not 0000 UTC.
C
	    IF ( time ( 1:2 ) .eq. '06' ) THEN
		irhour =  6
	      ELSE IF ( time ( 1:2 ) .eq. '12' ) THEN
		irhour = 12 
	      ELSE IF ( time ( 1:2 ) .eq. '18' ) THEN
		irhour = 18 
	    END IF
	END IF
C*
	RETURN
	END
