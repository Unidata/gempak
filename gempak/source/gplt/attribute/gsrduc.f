	SUBROUTINE GSRDUC ( filter, iret )
C************************************************************************
C* GSRDUC								*
C*									*
C* This subroutine sets the filter factor for the point reduction	*
C* scheme.								*
C*									*
C* GSRDUC ( FILTER, IRET )						*
C*									*
C* Input parameters:							*
C*	FILTER		REAL		Filter factor for pnt reduction	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 5/99						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	First check if these are the current requested characteristics.
C
	IF  ( filter .eq. rrfilt )  THEN
C
C*	    Do nothing.
C
	  ELSE
C
C*	    Set the requested parameters.
C
	    IF  ( ( filter .ge. 0.0 ) .and.
     +		  ( filter .le. 1.0 ) )  rrfilt = filter
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*		Only set if requested values are different than what
C*		is already set.
C
		IF  ( rrfilt .ne. srfilt )  THEN
		    CALL DSRDUC ( rrfilt, srfilt, iret )
		END IF
	    END IF
	END IF
C*
	RETURN
	END
