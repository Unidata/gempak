	SUBROUTINE IN_FILT  ( filter, filtfc, iret )
C************************************************************************
C* IN_FILT								*
C*									*
C* This subroutine decodes the input for FILTER.  Acceptable inputs	*
C* include a single number or a string beginning with a Y or N.		*
C* FILTFC will be set to zero if no filtering is requested.  If		*
C* FILTER = Y[ES], FILTFC is set to 1.  For all positive inputs, 	*
C* FILTFC is set to the number input.  Otherwise, FILTFC is set to	*
C* zero and the error flag is set.					*
C*									*
C* IN_FILT  ( FILTER, FILTFC, IRET )					*
C*									*
C* Input parameters:							*
C*	FILTER		CHAR*		Filter input			*
C*									*
C* Output parameters:							*
C*	FILTFC		REAL		Filter factor			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-11 = invalid input		*
C**									*
C* Log:									*
C* J. Nielsen/TAMU	11/91						*
C************************************************************************
	CHARACTER*(*)	filter
C------------------------------------------------------------------------
	iret  = 0
C
C*	Check for yes or no as input.
C
	IF ( ( filter (1:1) .eq. 'y' ) .or. 
     +	     ( filter (1:1) .eq. 'Y' ) )  THEN
	    filtfc = 1.
C
	ELSE IF ( ( filter (1:1) .eq. 'n' ) .or.
     +	  ( filter (1:1) .eq. 'N' ) ) THEN
	    filtfc = 0.
C
C*	Decode real number from input string.
C
	ELSE
	    CALL ST_CRNM  ( filter, filtfc, ier )
	    IF  ( ( ier .lt. 0 ) .or. ( filtfc .lt. 0. ) )  THEN
		iret = -11
		filtfc = 0.
	    END IF
	END IF
C*
	RETURN
	END
