	SUBROUTINE RS_HHDE  ( temp, digits, nout, iret )
C************************************************************************
C* RS_HHDE								*
C*									*
C* This subroutine decodes the one or two digit cloud height code and	*
C* returns the cloud height in meters.					*
C*									*
C* RS_HHDE ( TEMP, DIGITS, NOUT, IRET )					*
C*									*
C* Input parameters:							*
C*	TEMP		INTEGER		Undecoded digits		*
C*	DIGITS		INTEGER		Number of digits		*
C*									*
C* Output parameters:							*
C*	NOUT		INTEGER		Decoded cloud height		*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal return		*
C*					 -1 = Too many digits		*
C**									*
C* Log:									*
C* J. Nielsen/MIT	 4/89						*
C* J. Nielsen/TAMU	 2/92	Gempacized				*
C* S. Jacobs/NCEP	 2/01	Fixed typo code -> temp			*
C************************************************************************
	INTEGER		temp, digits
C
	INTEGER		nout, iret
C------------------------------------------------------------------------
	iret = 0
	IF  ( ( digits .ne. 1 ) .and. ( digits .ne. 2 ) )  THEN
	    iret = -1
	    RETURN
	ENDIF
C
C*	Decode two digits
C
	IF  ( digits .eq. 2 )  THEN
	    IF  ( temp .le. 50 )  THEN
		nout = 30 * temp
		RETURN
	    ELSE IF  ( temp .le. 80 )  THEN
		nout = ( temp - 50 ) * 300
		RETURN
	    ELSE IF  ( temp .le. 90 )  THEN
		nout = ( temp - 74 ) * 1500
		RETURN
	    ELSE
C
C*		Convert to one-digit code
C
		temp = temp - 90
	    ENDIF
	ENDIF
C
C*	Decode one digit
C
	If  ( temp .eq. 0 )  nout = 0
	IF  ( temp .eq. 1 )  nout = 50
	IF  ( temp .eq. 2 )  nout = 100
	IF  ( temp .eq. 3 )  nout = 200
	IF  ( temp .eq. 4 )  nout = 300
	IF  ( temp .eq. 5 )  nout = 600
	IF  ( temp .eq. 6 )  nout = 1000
	IF  ( temp .eq. 7 )  nout = 1500
	IF  ( temp .eq. 8 )  nout = 2000
C
C*	The final possibility includes cloud height of > 2500 m, 
C*	or no clouds at all
C
	IF  ( temp .eq. 9 )  nout = 2500
	RETURN
	END

