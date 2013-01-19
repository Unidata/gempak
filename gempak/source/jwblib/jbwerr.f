	SUBROUTINE JB_WERR ( i, usrmsg, iret )
C************************************************************************
C* JB_WERR								*
C*									*
C* This subroutine writes a JB library error message.			*
C*									*
C* The USRMSG is not printed if it is blank.				*
C*									*
C* JB_WERR  ( I, USRMSG, IRET )						*
C*									*
C* Input parameters:							*
C*	I		INTEGER		Value of a return code		*
C*	USRMSG		CHAR*		User reference message		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				 	  0 = normal return 		*
C**									*
C* Log:									*
C* K. Brill/EMC		 7/98						*
C************************************************************************
	CHARACTER*(*)	usrmsg
C------------------------------------------------------------------------
	iret = 0
	IF ( usrmsg .ne. ' ' ) THEN
	    CALL ST_LSTR ( usrmsg, lng, ier )
	    WRITE (6,*) usrmsg (1:lng)
	END IF
	IF ( i .eq. 0 ) WRITE (6,*)
     +		 '  0 = normal return'
	IF ( i .eq. -1 ) WRITE (6,*)
     +		  '-01 = cannot open BUFR file'
	IF ( i .eq. -2 ) WRITE (6,*)
     +		  '-02 = BUFR file name is blank'
	RETURN
	END
