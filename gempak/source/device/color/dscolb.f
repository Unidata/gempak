	SUBROUTINE DSCOLB  ( icbank, icolr, iret )
C************************************************************************
C* DSCOLB								*
C* 									*
C* This subroutine sets the color number.  				*
C*									*
C* DSCOLB ( ICBANK, ICOLR, IRET )					*
C*									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank			*
C*	ICOLR		INTEGER		Color number			*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 5/95	After dscolr()				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C------------------------------------------------------------------------
	iret  = NORMAL
C
C*	Set image colors directly
C
	IF ( icbank .gt. 0 ) THEN
	    CALL HSCOLB ( icbank, icolr, ier )
	    mcolr = 0
	    RETURN
	END IF
C
C*	Exclude negative and zero colors.
C
	IF  ( icolr .eq. 101 )  THEN
	    jcolr = icolr
	  ELSE IF  ( icolr .gt. 0 ) THEN
	    jcolr = MOD ( icolr, nncolr )
	    IF  ( jcolr .eq. 0 ) jcolr = nncolr
	  ELSE
	    RETURN
	END IF
C
C*	Check if this is the active color.
C
	
	IF  ( jcolr .ne. mcolr ) THEN
C
C*	    Set this color in the device.
C
	    CALL HSCOLR  ( jcolr, iret )
C
C*	    Store color in active buffer.
C
	    mcolr = jcolr
	END IF
C*
	RETURN
	END
