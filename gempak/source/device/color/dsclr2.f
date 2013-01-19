	SUBROUTINE DSCLR2  ( icolr, icolr2, jcolr, jcolr2, iret )
C************************************************************************
C* DSCLR2								*
C* 									*
C* This subroutine sets two color numbers.  Color numbers larger than	*
C* the number of valid colors will be converted, via modular 		*
C* arithmetic to a valid color number.  If the color number is 		*
C* negative or zero, no change will be made. 				*
C*									*
C* DSCLR2  ( ICOLR, ICOLR2, JCOLR, JCOLR2, IRET )			*
C* 									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Color number 1			*
C*	ICOLR2		INTEGER		Color number 2			*
C* 									*
C* Output parameters:							*
C*	JCOLR		INTEGER		Color number set 1		*
C*	JCOLR2		INTEGER		Color number set 2		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVACT.CMN'
C------------------------------------------------------------------------
	iret  = NORMAL
	jcolr  = mcolr
	jcolr2 = mcolr2
C
C*	Exclude negative and zero colors.
C
	IF  ( icolr .eq. 101 )  THEN
	    jcolr  = icolr
	  ELSE IF  ( icolr .gt. 0 ) THEN
	    jcolr  = MOD ( icolr, nncolr )
	    IF  ( jcolr .eq. 0 ) jcolr = nncolr
	  ELSE
	    RETURN
	END IF
C
C*	Exclude negative and zero colors.
C
	IF  ( icolr2 .eq. 101 )  THEN
	    jcolr2 = icolr2
	  ELSE IF  ( icolr2 .gt. 0 ) THEN
	    jcolr2 = MOD ( icolr2, nncolr )
	    IF  ( jcolr2 .eq. 0 ) jcolr2 = nncolr
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
C
	IF  ( jcolr2 .ne. mcolr2 ) THEN
C
C*	    Store color in active buffer.
C
	    mcolr2 = jcolr2
	END IF
C
	RETURN
	END
