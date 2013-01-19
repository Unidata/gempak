	SUBROUTINE GSCLR2 ( icolr, icolr2, iret )
C************************************************************************
C* GSCLR2								*
C* 									*
C* This subroutine sets two color numbers.  Color numbers larger than	*
C* the number of valid colors will be converted, via modular 		*
C* arithmetic, to a valid color number.  If the color number is 	*
C* negative or zero, no change will be made. 				*
C* 									*
C* GSCLR2  ( ICOLR, ICOLR2, IRET )					*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number 1			*
C* 					  <= 0 = no change		*
C* 	ICOLR2		INTEGER		Color number 2			*
C* 					  <= 0 = no change		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	 3/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	    First check if this is the current requested color.
C*	    If so, do nothing.
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
C
C*	    First check if this is the current requested color (2).
C*	    If so, do nothing.
C
	IF  ( icolr2 .eq. kcolr2 ) THEN
C
C*	    Check if this is the current set color (2).  If so, save
C*	    as requested color.
C
	  ELSE IF  ( icolr2 .eq. lcolr2 ) THEN
	    kcolr2 = icolr2
C
C*	    If the color is negative or zero, ignore.
C
	  ELSE IF  ( icolr2 .le. 0 ) THEN
C*
	  ELSE
C
C*	    Save the color as the requested color.
C
	    kcolr2 = icolr2
C
C*	    Make this the active color only if the device has been set.
C
	    IF  ( ddev .ne. ' ' ) THEN
C
C*	        Compute the color number to be set.
C
	        ijcolr2 = MOD ( icolr2, nncolr )
	        IF  ( ijcolr2 .eq. 0 )   ijcolr2 = nncolr
		IF  ( icolr2 .eq. 101 )  ijcolr2 = icolr2
		lcolr2 = ijcolr2
	    END IF
	END IF
C
	RETURN
	END
