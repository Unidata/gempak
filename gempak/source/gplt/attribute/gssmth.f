	SUBROUTINE GSSMTH ( ismtyp, dens, iret )
C************************************************************************
C* GSSMTH								*
C*									*
C* This subroutine sets the line smoothing attributes.			*
C*									*
C* GSSMTH ( ISMTYP, DENS, IRET )					*
C*									*
C* Input parameters:							*
C*	ISMTYP		INTEGER		Smoothing type			*
C*					  0 = none			*
C*					  1 = splines			*
C*					  2 = parametric		*
C*	DENS		REAL		Density of intermediate points	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* S. Jacobs/NCEP	 2/98						*
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
	IF  ( ( ismtyp .eq. ksmtyp ) .and.
     +	      ( dens   .eq. rdens  ) )  THEN
C
C*	    Do nothing.
C
	  ELSE
C
C*	    Set the requested parameters.
C
	    IF  ( ismtyp .ge. 0 )  ksmtyp = ismtyp
	    IF  ( dens   .ge. 0 )  rdens  = dens
C
C*	    Make changes only if the device has been set.
C
	    IF  ( ddev .ne. ' ' )  THEN
C
C*		Only set if requested values are different than what
C*		is already set.
C
		IF  ( ( ksmtyp .ne. lsmtyp ) .or.
     +		      ( rdens  .ne. sdens  ) )  THEN
		    CALL DSSMTH ( ksmtyp, rdens, ketype, rtensn,
     +				  lsmtyp, sdens, letype, stensn, iret )
		END IF
	    END IF
	END IF
C*
	RETURN
	END
