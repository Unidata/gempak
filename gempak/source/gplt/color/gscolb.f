	SUBROUTINE GSCOLB ( icbank, icolr, iret )
C************************************************************************
C* GSCOLB								*
C* 									*
C* This subroutine sets the color number in a color bank. The current	*
C* color banks are:							*
C*			0 = graphics					*
C*			1 = satellite images				*
C*			2 = radar images				*
C*									*
C* Graphics color numbers larger than the number of valid colors will	*
C* be converted, via modular arithmetic, to a valid color number.  If	*
C* the color number is negative or zero, no change will be made.	*
C* This routine replaces gscolr.					*
C* 									*
C* GSCOLB  ( ICBANK, ICOLR, IRET )					*
C*									*
CC*									*
C* Input parameters:							*
C*	ICBANK		INTEGER		Color bank			*
C* 	ICOLR		INTEGER		Color number			*
C* 					  <= 0 = no change		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Cowie/COMET	 5/95	After GSCOLR()				*
C* M. Linda/GSC		 2/97	Removed GFLUSH				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	If this is a new color bank, save it in common.
C
	IF ( icbank .ne. kcbank ) kcbank = icbank
C
C*	For non-graphics colors, set the color directly
C
	IF ( icbank .gt. 0 ) THEN
C
C*	    Call color setting routine
C
	    CALL DSCOLB ( icbank, icolr, ier )
	    kcolr = 0
	    lcolr = 0
	    RETURN
C
	END IF
C
C*	First check if this is the current requested color.
C*	If so, do nothing.
C
	IF  ( icolr .eq. kcolr ) THEN
C
C*	    Check if this is the current set color.  If so, save
C*	    as requested color.
C
	  ELSE IF  ( icolr .eq. lcolr ) THEN
	    kcolr = icolr
C
C*	    If the color is negative or zero, ignore.
C
	  ELSE IF  ( icolr .le. 0 ) THEN
C*
	  ELSE
C
C*	    Save the color as the requested color.
C
	    kcolr  = icolr
C
C*	    Make this the active color only if the device has been set.
C
	    IF  ( ddev .ne. ' ' ) THEN
C
C*	        Compute the color number to be set.
C
	        ijcolr = MOD ( icolr, nncolr )
	        IF  ( ijcolr .eq. 0 )  ijcolr = nncolr
		IF  ( icolr .eq. 101 )  ijcolr = icolr
		lcolr = ijcolr
	    END IF
	END IF
C*
	RETURN
	END
