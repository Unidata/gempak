	SUBROUTINE MS_IDAT ( report, istart, incr, ifcstm, line, iret )
C************************************************************************
C* MS_IDAT                                                              *
C*									*
C* This subroutine decodes a line of the MOS report that has numerical	*
C* data.                                                                *
C*									*
C* MS_IDAT ( REPORT, ISTART, INCR, IFCSTM, LINE, IRET )			*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		MOS report			*
C*	ISTART		INTEGER		Index of line beginning 	*
C*	INCR		INTEGER		Field width                     *
C*	IFCSTM		INTEGER		Number of forecast times        *
C*									*
C* Output parameters:							*
C*	LINE (*)	INTEGER		Forecast data			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00						*
C* D. Kidwell/NCEP	 9/02	Fixed bug in decoding climatology	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	INTEGER		line (*)
C*
	CHARACTER	tstr*3
	LOGICAL		climo
C------------------------------------------------------------------------
	iret   = 0
	jncr   = incr
	jfcstm = IABS ( ifcstm )
	IF ( ifcstm .eq. jfcstm ) THEN
	    climo = .false.
	  ELSE
	    climo = .true.
	END IF
	DO ii = 1, jfcstm
	    line ( ii ) = IMISSD
	END DO
C
C*	Parse the specified line of the report.
C
	IF ( istart .ne. 0 ) THEN
	    ibeg = istart
	    iend = INDEX ( report ( istart: ), CHCR ) + istart
	    DO ii = 1, jfcstm 
		IF ( ibeg .lt. iend ) THEN
		    tstr = report ( ibeg:ibeg + 2 )
C
C*		    Convert each string to an integer.
C
		    CALL ST_NUMB ( tstr, line ( ii ), ier )
		    IF ( climo .and. ( ii .eq. ( jfcstm - 2 ) ) )
     +			 jncr = incr - 1
		    ibeg = ibeg + jncr
		END IF
	    END DO
	END IF
C*
	RETURN
	END
