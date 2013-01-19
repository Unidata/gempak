	SUBROUTINE MS_FCTM ( date, irhour, ifcstm, gemftm, iret )
C************************************************************************
C* MS_FCTM								*
C*									*
C* This subroutine determines the forecast times for the MOS report and *
C* converts them into GEMPAK format.				        *
C*									*
C* MS_FCTM ( DATE, IRHOUR, IFCSTM, GEMFTM, IRET )			*
C*									*
C* Input parameters:							*
C*	DATE		CHAR*		Model date			*
C*	IRHOUR		INTEGER		Model hour			*
C*	IFCSTM		INTEGER		Number of forecast times        *
C*									*
C* Output parameters:							*
C*	GEMFTM(*)	CHAR*		Forecast times in GEMPAK format *
C*	IRET		INTEGER		Return Code			*
C*					  -3 = date cannot be decoded	*
C*					  -8 = invalid no. of fcst times*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00		                         	*
C* m.gamazaychikov/SAIC 11/03   Replaced references to AVN/MRF with     *
C*                              references to GFS/GFSX                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	date, gemftm (*)
C
	INTEGER		idtarr (5), iymd (3)
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse the date.	
C
	CALL ST_ILST ( date, '/', IMISSD, 3, iymd, inum, ier )
	IF ( ( ier .ne. 0 ) .or. ( inum .ne. 3 ) ) THEN
            iret = -3
	    RETURN
	END IF
C
	idtarr ( 1 ) = iymd ( 3 )
	idtarr ( 2 ) = iymd ( 1 )
	idtarr ( 3 ) = iymd ( 2 )
	idtarr ( 4 ) = irhour
	idtarr ( 5 ) = 0
C
C*	Convert the forecast times into GEMPAK format.  For GFS MOS,
C*	forecast times are in 3 hour increments starting 6 hours after
C*	the report time and going to 60 hours after the report time,
C*	then in 6 hour increments to 72 hours after the report time.
C*	For GFSX MOS, forecast times are in 12 hour increments starting
C*	24 hours after the report time and going to 192 hours after the
C*	report time.
C
	IF ( ifcstm .eq. 21 ) THEN
C
C*	    This is GFS MOS.
C
	    CALL TI_ADDM ( idtarr, 360, idtarr, ier )
	    incr   = 180
	  ELSE IF ( ifcstm .eq. 15 ) THEN
C
C*	    This is GFSX MOS.
C
	    CALL TI_ADDD ( idtarr, idtarr, ier )
	    incr   = 720
	  ELSE
	    iret = -8
	    RETURN
	END IF
C
	DO ii = 1, ifcstm
	    CALL TI_ITOC ( idtarr, gemftm ( ii ), ier )
	    IF ( ii .eq. 19 ) incr = 360
	    CALL TI_ADDM ( idtarr, incr, idtarr, ier )
	END DO
C*
	RETURN
	END
