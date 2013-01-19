	SUBROUTINE RS_VVDE  ( temp, nout, iret )
C************************************************************************
C* RS_VVDE								*
C*									*
C* This subroutine converts the VV code to visibility in tens of meters	*
C*									*
C* RS_VVDE  ( TEMP, NOUT, IRET )					*
C*									*
C* Input parameters:							*
C*	TEMP		INTEGER		VV code				*
C*									*
C* Output parameters:							*
C*	NOUT		INTEGER		Decoded visibility		*
C*	IRET		INTEGER		Return code			*
C*					  0 = Normal completion		*
C*					 -1 = Unexpected code		*
C**									*
C* Log:									*
C* J. Nielsen/MIT	10/86						*
C* J. Nielsen/TAMU	 2/92	Gempacized				*
C************************************************************************
	INTEGER		temp
C
	INTEGER		nout, iret
C------------------------------------------------------------------------
	iret = 0
	IF  ( ( temp .lt. 0 ) .or. ( temp .ge. 100 ) )  THEN
	    iret = -1
	    RETURN
	ENDIF
	IF  ( temp .le. 55 )  THEN
C
C*	    Visibility less than 5 km
C
	    nout = temp * 10
	ELSE IF  ( temp .le. 80 )  THEN
C
C*	    Visibility up to 30 km
C
	    nout = ( temp - 50 ) * 100
	ELSE IF  ( temp .lt. 90 )  THEN
C
C*	    Visibility up to 70 km; encode >70 as if 70
C
	    nout = ( temp - 74 ) * 500
	ELSE
C
C*	    Less exact visibility codes
C
	    IF  ( temp .eq. 90 )  nout = 0
	    IF  ( temp .eq. 91 )  nout = 5
	    IF  ( temp .eq. 92 )  nout = 20
	    IF  ( temp .eq. 93 )  nout = 50
	    IF  ( temp .eq. 94 )  nout = 100
	    IF  ( temp .eq. 95 )  nout = 200
	    IF  ( temp .eq. 96 )  nout = 400
	    IF  ( temp .eq. 97 )  nout = 1000
	    IF  ( temp .eq. 98 )  nout = 2000
	    IF  ( temp .eq. 99 )  nout = 5000
	END IF
	RETURN
	END

