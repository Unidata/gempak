	SUBROUTINE GSMMAP ( proj, dlatll, dlonll, dlatur, dlonur, iret )
C************************************************************************
C* GSMMAP								*
C*									*
C* This subroutine provides a simplified call for defining the map      *
C* projection and bounds used for plotting in map coordinates.  The  	*
C* angles necessary for defining these projections are based on the	*
C* specified bounds.  The following projections are valid:		*
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
C* If the projection name is DEF, the current map or satellite 		*
C* projection will be used.  In this case, the bounds specified will	*
C* not be used.								*
C*									*
C* GSMMAP  ( PROJ, DLATLL, DLONLL, DLATUR, DLONUR, IRET )		*
C*									*
C* Input parameters:							*
C*	PROJ		CHAR*		Map projection			*
C*	DLATLL		REAL		Lower left latitude		*
C*	DLONLL		REAL		Lower left longitude		*
C*	DLATUR		REAL		Upper right latitude		*
C*	DLONUR		REAL		Upper right longitude		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* J. Vilardo/RDS	 6/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* B. Doty/RDS		 5/87   Added SPS, UTS, and DEF as map types	*
C* B. Doty/RDS		 7/87	Added SCC, SOR, NOR, and fixed DEF	*
C* M. desJardins/GSFC	 6/88	Redone					*
C* K. Brill/EMC		 3/96	CALL PRNLON				*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	CHARACTER*(*) 	proj
C*
	CHARACTER	cprj*4
C------------------------------------------------------------------------
C*	Verify that map mode is set.
C
	IF  ( igmode .ne. 1 )  THEN
	    iret = NIMODE
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
C*	    Check for valid simple projection type and call GSMPRJ to set up.
C
	    IF  ( cprj .eq. 'CED' )  THEN
		CALL GSMPRJ  ( cprj, 0., avlon, 0., dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'MCD' )  THEN
		CALL GSMPRJ  ( cprj, 0., avlon, 0., dlatll, 
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'MER' )  THEN
		CALL GSMPRJ  ( cprj, 0., avlon, 0., dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'NPS' )  THEN
		CALL GSMPRJ  ( 'STR', 90., avlon, 0., dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'LCC' )  THEN
		CALL GSMPRJ  ( cprj, 30., avlon, 60., dlatll, 
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'SCC' )  THEN
		CALL GSMPRJ  ( 'SCC', -30., avlon, -60., dlatll, 
     +			       dlonll, dlatur, dlonur, iret )
C*
              ELSE IF  ( cprj .eq. 'SPS' )  THEN
		CALL GSMPRJ  ( 'STR', -90., avlon, 0., dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
              ELSE IF  ( cprj .eq. 'UTM' )  THEN 
                CALL GSMPRJ  ( cprj, avlon, 0.0, 0.0, dlatll, 
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'NOR' )  THEN 
		CALL GSMPRJ  ( 'ORT', 90.0, avlon, 0.0, dlatll,
     +			       dlonll, dlatur, dlonur, iret )
C*
	      ELSE IF  ( cprj .eq. 'SOR' )  THEN 
		CALL GSMPRJ ( 'ORT', -90.0, avlon, 0.0, dlatll, 
     +			      dlonll, dlatur, dlonur, iret )
C*
C*		If the projection was 'DEF', check to see that a
C*		map projection has been set and return.
C
	      ELSE IF  ( ( cprj .eq. 'DEF' ) .and. ( mset ) )  THEN
		iret = NORMAL	
		RETURN
C
C*		Return invalid projection code otherwise.
C
	      ELSE
		iret = NIPROJ
		RETURN
	    END IF
	END IF
C
C*	Save values specific to this projection type definition.
C
	IF  ( iret .eq. NORMAL )  mtype = 1
C*
	RETURN
	END
