	SUBROUTINE MS_CDAT ( report, istart, incr, ifcstm, cline, iret )
C************************************************************************
C* MS_CDAT                                                              *
C*									*
C* This subroutine decodes a line of the MOS report that has character	*
C* data.                                                                *
C*									*
C* MS_CDAT ( REPORT, ISTART, INCR, IFCSTM, CLINE, IRET )		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		MOS report			*
C*	ISTART		INTEGER		Index of line beginning 	*
C*	INCR		INTEGER		Field width                     *
C*	IFCSTM		INTEGER		Number of forecast times        *
C*									*
C* Output parameters:							*
C*	CLINE (*)	CHAR*		Forecast data			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, cline (*)
C------------------------------------------------------------------------
	iret = 0
	DO ii = 1, ifcstm
	    cline ( ii ) = ' XX'
	END DO
C
C*	Parse the specified line of the report.
C
	IF ( istart .ne. 0 ) THEN
	    ibeg = istart
	    iend = INDEX ( report ( istart: ), CHCR ) + istart
	    DO ii = 1, ifcstm 
		IF ( ibeg .lt. iend ) THEN
		    cline ( ii ) = report ( ibeg:ibeg + 2 )
		    ibeg = ibeg + incr
		END IF
	    END DO
	END IF
C*
	RETURN
	END
