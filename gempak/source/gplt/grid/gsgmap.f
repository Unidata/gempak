	SUBROUTINE GSGMAP  (  proj, kx, ky, dlatll, dlonll, dlatur,
     +			    dlonur, iret )
C************************************************************************
C* GSGMAP								*
C* 									*
C* This subroutine defines the coordinate system for a grid which is	*
C* evenly spaced in a simplified map projection.  It is valid for 	*
C* the following map projection types:					*
C*									*
C*        CED       Cylindrical equidistant				*
C*        MCD       Modified cylindrical equidistant			*
C*        MER       Mercator						*
C*        NPS       North polar stereographic				*
C*        SPS       South polar stereographic				*
C*        LCC       Lambert conic conformal ( Northern hemisphere )	*
C*        SCC       Lambert conic conformal ( Southern hemisphere )	*
C*        NOR       North Polar Orthographic 				*
C*        SOR       South polar Orthographic 				*
C*        UTM       Universal Transverse Mercator		        *
C*									*
C* GSGMAP  ( PROJ, KX, KY, DLATLL, DLONLL, DLATUR, DLONUR, IRET )	*
C* 									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection name		*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C* 									*
C* Output parameters:							*
C* 	IRET 		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 9/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Changed calling sequence to have kx,ky	*
C* K. Brill/EMC		 3/96	CALL PRNLON				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*)	proj
C*
	CHARACTER	cprj*4
C------------------------------------------------------------------------
C*	Verify that map mode is set.
C
	IF  ( igmode .ne. 1 ) THEN
	    iret = NIMODE
	  ELSE IF  ( ( kx .le. 1 ) .or. ( ky .le. 1 ) )  THEN
	    iret = NIPBND
	  ELSE
	    iret = NORMAL
C
C*	    Convert projection type to upper case.
C
	    CALL ST_LCUC  ( proj, cprj, ier )
C
C*	    Compute average longitude for setting up projection.
C
	    ddlonw = dlonll
	    CALL PRNLON ( 1, ddlonw, ier )
	    ddlone = dlonur
	    CALL PRNLON ( 1, ddlone, ier )
C
	    IF  ( ddlonw .eq. ddlone )  THEN
		avlon = ddlonw + 180.
		CALL PRNLON ( 1, avlon, ier )
C
	      ELSE IF  ( ddlonw .gt. ddlone )  THEN
		avlon = ( 360. + ddlonw + ddlone ) / 2.
		CALL PRNLON ( 1, avlon, ier )
C
	      ELSE 
	    	avlon = ( ddlonw + ddlone ) / 2.0
		CALL PRNLON ( 1, avlon, ier )
	    END IF
C
C*	    Check for valid projection type and call GSGPRJ to set up.
C
	    IF  ( cprj .eq. 'CED' )  THEN
		CALL GSGPRJ  ( cprj, 0., avlon, 0., kx, ky, dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'MCD' )  THEN
		CALL GSGPRJ  ( cprj, 0., avlon, 0., kx, ky, dlatll, 
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'MER' )  THEN
		CALL GSGPRJ  ( cprj, 0., avlon, 0., kx, ky, dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'NPS' )  THEN
		CALL GSGPRJ  ( 'STR', 90., avlon, 0., kx, ky, dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'LCC' )  THEN
		CALL GSGPRJ  ( cprj, 30., avlon, 60., kx, ky, dlatll, 
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'SCC' )  THEN
		CALL GSGPRJ  ( 'SCC', -30., avlon, -60., kx, ky, 
     +			       dlatll, dlonll, dlatur, dlonur, iret )
C*
              ELSE IF  ( cprj .eq. 'SPS' )  THEN
		CALL GSGPRJ  ( 'STR', -90., avlon, 0., kx, ky, dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'NOR' )  THEN 
		CALL GSGPRJ  ( 'ORT', 90.0, avlon, 0.0, kx, ky, dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'SOR' )  THEN 
		CALL GSGPRJ ( 'ORT', -90.0, avlon, 0.0, kx, ky, dlatll, 
     +			      dlonll, dlatur, dlonur, iret )
C*
	      ELSE
		iret = NIPROJ
	    END IF
	END IF
C
C*	Save values specific to this projection type definition.
C
	IF  ( iret .eq. NORMAL )  mgtype  = 1
C*
	RETURN
	END
