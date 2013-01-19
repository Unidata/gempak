	SUBROUTINE DSCOLR  ( icolr, jcolr, iret )
C************************************************************************
C* DSCOLR								*
C* 									*
C* This subroutine sets the color number.  Color numbers larger than	*
C* the number of valid colors will be converted, via modular 		*
C* arithmetic to a valid color number.  If the color number is 		*
C* negative or zero, no change will be made.				*
C*									*
C* DSCOLR  ( ICOLR, JCOLR, IRET )					*
C* 									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Color number			*
C* 									*
C* Output parameters:							*
C*	JCOLR		INTEGER		Color number set		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/NMC	 1/92	Pass 101 to device driver		*
C* I. Durham/GSC	 3/98	Added setting for mcolr2		*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C------------------------------------------------------------------------
	iret  = NORMAL
	jcolr = mcolr
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
	    mcolr  = jcolr
	    mcolr2 = jcolr
	END IF
C*
	RETURN
	END
