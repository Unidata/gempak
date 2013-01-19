	SUBROUTINE GQGPRJ  ( proj,  dangl1, dangl2, dangl3, kx, ky,
     +			    dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GQGPRJ								*
C* 									*
C* This subroutine returns the current coordinate system definition 	*
C* for a grid evenly spaced on a general map projection.  The grid	*
C* can be defined by GSGPRJ or GSGMAP.					*
C* 									*
C* GQGPRJ  ( PROJ, DANGL1, DANGL2, DANGL3, KX, KY, DLATLL, DLONLL, 	*
C*           DLATUR, DLONUR, IRET )					*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	DANGL1		REAL		Reference angle 1		*
C*	DANGL2		REAL		Reference angle 2		*
C*	DANGL3		REAL		Reference angle 3		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/85		GEMPLT Version 3.1		*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*)	proj
C*
C------------------------------------------------------------------------
C*	Check for valid mode.
C*	Then check that a projection is set.
C*	Then check that a simple projection is set.
C
	IF  ( igmode .ne. 1 )  THEN
	    iret = NIMODE
	  ELSE IF  ( .not. mgset )  THEN
	    iret = NIPROJ
	  ELSE IF  ( ( mgtype .ne. 1 ) .and. ( mgtype .ne. 2 ) )  THEN
	    iret = NIPROJ
	  ELSE IF  ( ( gpxlm .ne. 1. ) .or. ( gpybm .ne. 1. ) )  THEN
	    iret = NIPROJ
	  ELSE
	    proj    = gpjnam
	    dangl1  = gangl1
	    dangl2  = gangl2
	    dangl3  = gangl3
	    kx      = gpxrm
	    ky      = gpytm
	    dlatll  = gclats
	    dlonll  = gclonw
	    dlatur  = gclatn
	    dlonur  = gclone
	    iret    = NORMAL
	END IF
C*
	RETURN
	END
