	SUBROUTINE MS_GLIN ( report, lenr, numnam, cpnam, istart, iret ) 
C************************************************************************
C* MS_GLIN								*
C*									*
C* This subroutine determines the indices at which to begin decoding  	*
C* each line in a MOS report.		                                *
C*									*
C* MS_GLIN ( REPORT, LENR, NUMNAM, CPNAM, ISTART, IRET )		*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*	 	MOS report			*
C*	LENR		INTEGER		Length of report                *
C*	NUMNAM		INTEGER		Number of fcst variable names   *
C*	CPNAM(*)	CHAR*		Array of fcst variable names    *
C*									*
C* Output parameters:							*
C*	ISTART(*)	INTEGER		Indices of line beginnings	*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00	                               		*
C* D. Kidwell/NCEP	 9/02	Allowed WND or WSP for wind speed	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, cpnam (*)
	INTEGER		istart (*)
C*
	PARAMETER	( ILEN = 3 )
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through the report, one line at a time.
C
	DO k = 1, numnam
C
C*	    Find the index that identifies the start of the line.
C
	    istart ( k ) = INDEX ( report (:lenr), cpnam ( k ) (:ILEN) )
	    IF ( istart ( k ) .eq. 0 ) THEN
		IF ( cpnam ( k ) ( :ILEN ) .eq. './.' ) THEN
      	            istart (k) = MAX ( INDEX ( report (:lenr), 'X/N' ),
     +				       INDEX ( report (:lenr), 'N/X' ) )
		  ELSE IF ( cpnam ( k ) ( :ILEN ) .eq. 'WSP' ) THEN
      	            istart (k) = INDEX ( report (:lenr), 'WND' )
		END IF
	    END IF
	    IF ( istart ( k ) .gt. 0 ) istart ( k ) = istart ( k ) + 4
	END DO
C*
	RETURN
	END
