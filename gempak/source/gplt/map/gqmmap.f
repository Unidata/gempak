	SUBROUTINE GQMMAP ( proj, dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GQMMAP								*
C*									*
C* This subroutine returns the current simple map projection defined	*
C* by GSMMAP.								*
C*									*
C* GQMMAP  ( PROJ, DLATLL, DLONLL, DLATUR, DLONUR, IRET )		*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*	IRET 		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*)	proj
C------------------------------------------------------------------------
C*	Check for valid mode.
C*	Then check that a projection is set.
C*	Then check that a simple projection is set.
C
	IF  ( igmode .ne. 1 )  THEN
	    iret = NIMODE
	  ELSE IF  ( .not. mset )  THEN
	    iret = NIPROJ
	  ELSE IF  ( mtype .ne. 1 )  THEN
	    iret = NIPROJ
	  ELSE
C
C*	    Get projection name.  Change from GSMPRJ name to GSMMAP
C*	    name if necessary.
C
	    proj  = prjnam
	    IF  ( ( proj .eq. 'STR' ) .and. ( angle1 .eq. 90. ) )  THEN
		proj = 'NPS'
	      ELSE IF ((proj .eq. 'STR') .and. (angle1 .eq. -90.)) THEN
		proj = 'SPS'
	      ELSE IF ((proj .eq. 'ORT') .and. (angle1 .eq.  90.)) THEN
		proj = 'NOR'
	      ELSE IF ((proj .eq. 'ORT') .and. (angle1 .eq. -90.)) THEN
		proj = 'SOU'
	    END IF
	    dlatll = clats
	    dlonll = clonw
	    dlatur = clatn
	    dlonur = clone
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
