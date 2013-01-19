	SUBROUTINE MS_IDT2 ( report, istart, incr, ifcstm, initft, 
     +			     line1, line2, iret )
C************************************************************************
C* MS_IDT2                                                              *
C*									*
C* This subroutine decodes a line of the MOS report that has numerical	*
C* data occurring in pairs, with the numbers in each pair separated by  *
C* a slash ('/').                                                       *
C*									*
C* MS_IDT2 ( REPORT, ISTART, INCR, IFCSTM, INITFT, LINE1, LINE2, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		MOS report			*
C*	ISTART		INTEGER		Index of line beginning 	*
C*	INCR		INTEGER		Field width                     *
C*	IFCSTM		INTEGER		Number of forecast times        *
C*	INITFT		INTEGER		Initial forecast time           *
C*									*
C* Output parameters:							*
C*	LINE1 (*)	INTEGER		Forecast data preceding slash   *
C*	LINE2 (*)	INTEGER		Forecast data following slash   *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00						*
C* D. Kidwell/NCEP	11/01	Fixed for 06 and 18Z                    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	INTEGER		line1 (*), line2 (*)
C
	CHARACTER	tstr*3, tstr2*2
C------------------------------------------------------------------------
	iret = 0
	DO ii = 1, ifcstm
	    line1 ( ii ) = IMISSD
	    line2 ( ii ) = IMISSD
	END DO
C
C*	Parse the specified line of the report.
C
	IF ( istart .ne. 0 ) THEN
	    iskp = incr / 3
	    inc  = incr
	    ibeg = istart
	    iend = INDEX ( report ( istart: ), CHCR ) + istart
	    ii   = initft
	    last = initft + 12
	    DO WHILE ( ii .le. ifcstm )
		IF ( ibeg .lt. iend ) THEN
		    tstr = report ( ibeg:ibeg + 2 )
C
C*		    Convert each string to an integer.
C
		    CALL ST_NUMB ( tstr, line1 ( ii ), ier )
		    tstr2 = report ( ibeg + 4:ibeg + 5 )
		    CALL ST_NUMB ( tstr2, line2 ( ii ), ier )
		    IF ( ( inc .eq. 12 ) .and. ( ii .eq. last ) ) THEN
			IF ( initft .eq. 5 ) THEN
			    inc  = 9
			    iskp = 3
			  ELSE IF ( initft .eq. 7 ) THEN
			    inc  = 6
			    iskp = 2
			END IF
		    END IF
		    ibeg = ibeg + inc
		END IF
		ii = ii + iskp
	    END DO
	END IF
C*
	RETURN
	END
