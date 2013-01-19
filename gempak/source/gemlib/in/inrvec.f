	SUBROUTINE IN_RVEC  ( refvec, rmag, rx, ry, rtext, string,
     +			      iret )
C************************************************************************
C* IN_RVEC								*
C*									*
C* This subroutine decodes the reference arrow parameter in the form:	*
C*	Reference mag ; x- ; y- ; size / font / width / HW ; string  	*
C*									*
C* IN_RVEC  ( REFVEC, RMAG, RX, RY, RTEXT, STRING, IRET ) 		*
C*									*
C* Input parameters:							*
C*	REFVEC		CHAR*		Reference arrow input		*
C*									*
C* Output parameters:							*
C*	RMAG		REAL   		Reference arrow magnitude	*
C*	RX      	REAL    	Reference arrow x-position   	*
C*	RY		REAL   		Reference arrow y-position	*
C*	RTEXT		CHAR*		Reference arrow text attributes	*
C*	STRING		CHAR*		Reference arrow text		*
C*      IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* L. Sager/NMC	 	 7/93						*
C* S. Jacobs/EAI	10/93		Cleaned up; added rtext return	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	refvec, rtext, string
	CHARACTER	rrr*72, rrr2*72
	REAL		rarr (3)
C------------------------------------------------------------------------
	iret = 0
C
C*	Parse off the the user input string, if present.
C
	CALL ST_NOCC ( refvec, ';', 4, ipos, ier )
	IF  ( ( ipos .ne. 0 ) .and. ( ier .eq. 0 ) )  THEN
	    rrr2   = refvec ( :ipos-1 )
	    string = refvec ( ipos+1: )
	ELSE
	    rrr2   = refvec
	    string = ' '
	END IF
C
C*	Parse off the the text attributes, if present.
C
	CALL ST_NOCC ( refvec, ';', 3, ipos, ier )
	IF  ( ( ipos .ne. 0 ) .and. ( ier .eq. 0 ) )  THEN
	    rrr   = rrr2 ( :ipos-1 )
	    rtext = rrr2 ( ipos+1: )
	ELSE
	    rrr   = rrr2
	    rtext = ' '
	END IF
C
C*	Break the remaining input into elements.
C
	CALL ST_RLST  ( rrr, ';', RMISSD, 3, rarr, num, ier )
C
C* 	Set the magnitude and location.
C
	IF  ( rarr (1) .eq. RMISSD )  THEN
	    rmag = 0.
	ELSE
	    rmag = rarr (1)
	END IF
C*
	IF  ( rarr (2) .eq. RMISSD )  THEN
	    rx = .05
	ELSE
	    rx = rarr (2)
	END IF
C*
	IF  ( rarr (3) .eq. RMISSD )  THEN
	    ry = .1
	ELSE
	    ry = rarr (3)
	END IF
C*
	RETURN
	END
