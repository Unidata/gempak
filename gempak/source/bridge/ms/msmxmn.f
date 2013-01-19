	SUBROUTINE MS_MXMN ( report, istart, incr, ifcstm, initft, 
     +			     line1, line2, iret )
C************************************************************************
C* MS_MXMN                                                              *
C*									*
C* This subroutine decodes a line of the MOS report that has numerical	*
C* data for maximum and minimum (day and night) temperatures.           *
C*									*
C* MS_MXMN ( REPORT, ISTART, INCR, IFCSTM, INITFT, LINE1, LINE2, IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		MOS report			*
C*	ISTART		INTEGER		Index of line beginning 	*
C*	INCR		INTEGER		Field width                     *
C*	IFCSTM		INTEGER		Number of forecast times        *
C*	INITFT		INTEGER		Initial forecast time           *
C*									*
C* Output parameters:							*
C*	LINE1 (*)	INTEGER		First set of forecast data      *
C*	LINE2 (*)	INTEGER		Second set of forecast data     *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00						*
C* D. Kidwell/NCEP	11/01	Fixed for 06 and 18Z                    *
C* D. Kidwell/NCEP	 9/02	Fixed bug in decoding climatology	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	INTEGER		line1 (*), line2 (*)
C*
	CHARACTER	tstr*3
	LOGICAL		first
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
	    iskp  = incr / 3
	    inc   = incr
	    ibeg  = istart
	    iend  = INDEX ( report ( istart: ), CHCR ) + istart
	    ii    = initft
	    first = .true.
	    IF ( initft .ne. 1 ) THEN
	        last = initft + 12
	      ELSE
		last = ifcstm - 2
	    END IF
	    DO WHILE ( ii .le. ifcstm )
		IF ( ibeg .lt. iend ) THEN
		    tstr = report ( ibeg:ibeg + 2 )
C
C*		    Convert each string to an integer.
C
		    CALL ST_NUMB ( tstr, ival, ier )
		    IF ( first ) THEN
			line1 ( ii ) = ival
		      ELSE
			line2 ( ii ) = ival
		    END IF
		    first = .not. first
		    IF ( ii .eq. last ) THEN
		        IF ( initft .eq. 7 ) THEN
			    inc  = 6
			    iskp = 2
		          ELSE IF ( initft .eq. 5 ) THEN
			    inc  = 9
			    iskp = 3
			  ELSE
			    inc = inc - 1
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
