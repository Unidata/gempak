	SUBROUTINE GSICNG ( szicng, icewid, iret )
C************************************************************************
C* GSICNG								*
C*									*
C* This subroutine sets the icing symbol attributes.			*
C*									*
C* GSICNG ( SZICNG, ICEWID, IRET )					*
C*									*
C* Input parameters:							*
C*	SZICNG		REAL		Icing symbol size		*
C*	ICEWID		INTEGER		Icing symbol line width		*
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
	IF  ( szicng .eq. rcersz .and. icewid .eq. kcewid ) RETURN
C
C*	Set requested parameters.
C
	IF  ( szicng .gt. 0 ) rcersz = szicng
	IF  ( icewid .ge. 1 ) kcewid = icewid
C
C*	Make changes in device only if the device has been set.
C
	IF  ( ddev .ne. ' ' ) THEN
C
C*	    Set only if requested is different than what is already set.
C
	    IF  ( rcersz .ne. scersz .or. kcewid .ne. lcewid)
     +		CALL DSICNG ( rcersz, kcewid, scersz, lcewid, iret )
	END IF
C*
	RETURN
	END
