	SUBROUTINE GQGMAP  ( proj,  kx, ky, dlatll, dlonll, dlatur,
     +			    dlonur, iret )
C************************************************************************
C* GQGMAP								*
C*									*
C* This subroutine returns the current definition for a grid evenly	*
C* spaced in a simple map projection.  The grid can be defined by 	*
C* GSGMAP.								*
C*									*
C* GQGMAP  ( PROJ, KX, KY, DLATLL, DLONLL, DLATUR, DLONUR, IRET )	*
C*									*
C* Output parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	7/85	GEMPLT Version 3.1			*
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
	  ELSE IF  ( mgtype .ne. 1 )  THEN
	    iret = NIPROJ
	  ELSE 
	    proj  = gpjnam
	    IF  ( ( proj .eq. 'STR' ) .and. ( gangl1 .eq. 90. ) )  THEN
		proj = 'NPS'
	      ELSE IF ((proj .eq. 'STR') .and. (gangl1 .eq. -90.)) THEN
		proj = 'SPS'
	      ELSE IF ((proj .eq. 'ORT') .and. (gangl1 .eq.  90.)) THEN
		proj = 'NOR'
	      ELSE IF ((proj .eq. 'ORT') .and. (gangl1 .eq. -90.)) THEN
		proj = 'SOU'
	    END IF
	    kx     = gpxrm
	    ky     = gpytm
	    dlatll = gclats
	    dlonll = gclonw
	    dlatur = gclatn
	    dlonur = gclone
	    iret   = NORMAL
	END IF
C*
	RETURN
	END
