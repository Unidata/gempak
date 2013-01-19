	SUBROUTINE GSSPCL ( szspcl, ispwid, iret )
C************************************************************************
C* GSSPCL								*
C*									*
C* This subroutine sets the special symbol attributes.			*
C*									*
C* GSSPCL ( SZSPCL, ISPWID, IRET )					*
C*									*
C* Input parameters:							*
C*	SZSPCL		REAL		Special symbol size		*
C*	ISPWID		INTEGER		Special symbol line width	*
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
	IF  ( szspcl .eq. rsprsz .and. ispwid .eq. kspwid ) RETURN
C
C*	Set requested parameters.
C
	IF  ( szspcl .gt. 0 ) rsprsz = szspcl
	IF  ( ispwid .ge. 1 ) kspwid = ispwid
C
C*	Make changes in device only if the device has been set.
C
	IF  ( ddev .ne. ' ' ) THEN
C
C*	    Set only if requested is different than what is already set.
C
	    IF  ( rsprsz .ne. ssprsz .or. kspwid .ne. lspwid)
     +		CALL DSSPCL ( rsprsz, kspwid, ssprsz, lspwid, iret )
	END IF
C*
	RETURN
	END
