	SUBROUTINE GSDASH  ( szdsh, iret )
C************************************************************************
C* GSDASH								*
C* 									*
C* This subroutine sets the line dashing scale.				*
C* 									*
C* GSDASH  ( SZDSH, IRET )						*
C*                                                                    	*
C* Input parameters:							*
C*	SZDSH		REAL		Line dashing scale		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 3/98   					*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	First check if these are the current requested characteristics.
C*	If so, do nothing.
C
	IF  ( szdsh .eq. rszdsh ) THEN
	ELSE 
C
C*	    Set requested parameters
C
	    IF  ( szdsh .gt. 0 ) rszdsh = szdsh
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*		Only set if requested different than what is already set
C
		IF ( rszdsh .ne. sszdsh ) THEN
		    CALL DSDASH ( rszdsh, sszdsh, iret )
		END IF
	    END IF
	END IF
C*
	RETURN
	END
