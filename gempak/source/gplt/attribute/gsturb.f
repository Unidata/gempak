	SUBROUTINE GSTURB ( szturb, ituwid, iret )
C************************************************************************
C* GSTURB								*
C*									*
C* This subroutine sets the turbulence symbol attributes.		*
C*									*
C* GSTURB ( SZTURB, ITUWID, IRET )					*
C*									*
C* Input parameters:							*
C*	SZTURB		REAL		Turbulence symbol size		*
C*	ITUWID		INTEGER		Turbulence symbol line width	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Linda/GSC		 8/96	Based on GSWTHR				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVSET.CMN'
C-----------------------------------------------------------------------
C*
	iret = NORMAL
C
C*	If these are the current requested characteristics, do nothing.
C
	IF  ( szturb .eq. rtursz .and. ituwid .eq. ktuwid ) RETURN
C
C*	Set requested parameters.
C
	IF  ( szturb .gt. 0 ) rtursz = szturb
	IF  ( ituwid .ge. 1 ) ktuwid = ituwid
C
C*	Make changes in device only if the device has been set.
C
	IF  ( ddev .ne. ' ' ) THEN
C
C*	    Set only if requested is different than what is already set.
C
	    IF  ( rtursz .ne. stursz .or. ktuwid .ne. ltuwid)
     +		CALL DSTURB ( rtursz, ktuwid, stursz, ltuwid, iret )
	END IF
C*
	RETURN
	END
