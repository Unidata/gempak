	SUBROUTINE GQMPRJ  (  proj,  angl1,  angl2,  angl3,
     +			     dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GQMPRJ								*
C*									*
C* This subroutine returns the current map projection and bounds, 	*
C* which were defined by GSMPRJ or GSMMAP.				*
C*									*
C* GQMPRJ  ( PROJ, ANGL1, ANGL2, ANGL3, DLATLL, DLONLL, DLATUR, 	*
C*							DLONUR, IRET )	*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	ANGL1		REAL		Reference angle 1		*
C*	ANGL2		REAL		Reference angle 2		*
C*	ANGL3		REAL		Reference angle 3		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/85	GEMPLT Version 3.1			*
C* S. Jacobs/NCEP	6/00	Removed check for mtype out of range	*
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
	  ELSE 
	    proj   = prjnam
	    angl1  = angle1
	    angl2  = angle2
	    angl3  = angle3
	    dlatll = clats
	    dlonll = clonw
	    dlatur = clatn
	    dlonur = clone
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
