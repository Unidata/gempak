	SUBROUTINE LV_DECD   ( clevel, rlev, iret )
C************************************************************************
C* LV_DECD								*
C*									*
C* This subroutine decodes a single level.  CLEVEL must be a number,	*
C* SFC, or TOP.  SFC and TOP will be transformed into 0 and -1,		*
C* respectively.							*
C*									*
C* LV_DECD  ( CLEVEL, RLEV, IRET )					*
C*									*
C* Input parameters:							*
C*	CLEVEL		CHAR*		Input character level		*
C*									*
C* Output parameters:							*
C*	RLEV		REAL		Level value			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = decode error		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/86						*
C* M. desJardins/GSFC	 9/88	Documentation				*
C************************************************************************
	CHARACTER*(*)	clevel
C*
	CHARACTER	clev*20
C------------------------------------------------------------------------
	CALL ST_LCUC  ( clevel, clev, ier )
C
C*	Check for 'SFC'.
C
	IF  ( clev .eq. 'SFC' )  THEN
	    rlev = 0.0
	    iret = 0
C
C*	    Check for 'TOP'.
C
	  ELSE IF  ( clev .eq. 'TOP' )  THEN
	    rlev = -1.0
	    iret = 0
C
C*	    Decode a number.
C
	  ELSE
	    CALL ST_CRNM  ( clev, rlev, iret )
	    IF  ( iret .ne. 0 )  iret = -1
	END IF
C*
	RETURN
	END
