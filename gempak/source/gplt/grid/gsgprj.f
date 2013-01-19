	SUBROUTINE GSGPRJ  ( proj,  angl1,  angl2,  angl3, kx, ky,
     +			    dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GSGPRJ								*
C* 									*
C* This subroutine defines the coordinate system for a grid which 	*
C* is evenly spaced on a general map projection. Information about 	*
C* map projections is given in GSMPRJ.					*
C* 									*
C* GSGPRJ  (  PROJ, ANGL1, ANGL2, ANGL3, KX, KY, DLATLL, DLONLL,	*
C*           DLATUR, DLONUR, IRET )					*
C* 									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	ANGL1		REAL		Reference angle 1		*
C*	ANGL2		REAL		Reference angle 2		*
C*	ANGL3		REAL		Reference angle 3		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		 Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	11/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 7/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* K. Brill/HPC		 8/02	Set reference angles close to 0	equal 0	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	proj
C*
	CHARACTER	cprj*4
C------------------------------------------------------------------------
C*	Check that map mode has been set.
C
	IF  ( igmode .ne. 1 )  THEN
	    iret = NIMODE
	  ELSE IF  ( ( kx .le. 1 ) .or. ( ky .le. 1 ) )  THEN
	    iret = NIPBND
	  ELSE
	    iret = NORMAL
C
C*	    Save input values.
C
	    CALL ST_LCUC  ( proj, cprj, ier )
	    gpjnam = cprj
	    IF  ( cprj .eq. 'CED' )  THEN
		mgclas = 1
		mgproj = 1
	      ELSE IF  ( cprj .eq. 'MER' )  THEN
		mgclas = 1
		mgproj = 2
	      ELSE IF  ( cprj .eq. 'MCD' )  THEN
		mgclas = 1
		mgproj = 3
	      ELSE IF  ( cprj .eq. 'AED' )  THEN
		mgclas = 2
		mgproj = 1
	      ELSE IF  ( cprj .eq. 'STR' )  THEN
		mgclas = 2
		mgproj = 2
	      ELSE IF  ( cprj .eq. 'ORT' )  THEN
		mgclas = 2
		mgproj = 3
	      ELSE IF  ( cprj .eq. 'LEA' )  THEN
		mgclas = 2
		mgproj = 4
	      ELSE IF  ( cprj .eq. 'GNO' )  THEN
		mgclas = 2
		mgproj = 5
	      ELSE IF  ( cprj .eq. 'LCC' )  THEN
		mgclas = 3
		mgproj = 1
	      ELSE IF  ( cprj .eq. 'SCC' )  THEN
		mgclas = 3
		mgproj = 2
	    END IF
	    mgsppj  = 0
	    mgtype  = 2
	    gangl1 = angl1
	    gangl2 = angl2
	    gangl3 = angl3
	    IF ( ABS ( gangl1 ) .lt. .001 ) gangl1 = 0.0
	    IF ( ABS ( gangl2 ) .lt. .001 ) gangl2 = 0.0
	    IF ( ABS ( gangl3 ) .lt. .001 ) gangl3 = 0.0
	    gangr1 = gangl1 * dtr
	    gangr2 = gangl2 * dtr
	    gangr3 = gangl3 * dtr
C
C*	    Save grid bounds.
C
	    gpxlm  = 1.
	    gpybm  = 1.
	    gpxrm  = kx
	    gpytm  = ky
C
C*	    Save bounds.
C
	    gclats = dlatll
	    gclonw = dlonll
	    gclatn = dlatur
	    gclone = dlonur
	    CALL UPDGDM  ( iret )
	END IF
C
C*	Check for valid map projection set.
C
	IF  ( iret .ne. NORMAL )  THEN
	    mgset  = .false.
	    mgtype = 0
	END IF
C*
	RETURN
	END
