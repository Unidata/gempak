	SUBROUTINE GSCOLR ( icolr, iret )
C************************************************************************
C* GSCOLR								*
C* 									*
C* This subroutine sets the color number.  Color numbers larger than	*
C* the number of valid colors will be converted, via modular 		*
C* arithmetic, to a valid color number.  If the color number is 	*
C* negative or zero, no change will be made.				*
C* 									*
C* GSCOLR  ( ICOLR, IRET )						*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C* 					  <= 0 = no change		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/GSFC	12/91	Use color 101 for background color	*
C* I. Durham/GSC	 3/98	Added setting for icolr2		*
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
C
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
	kcolr2 = kcolr
	lcolr2 = lcolr
C
	RETURN
	END
