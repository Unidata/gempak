	SUBROUTINE GQTOI  ( np, xm, ym, xi, yi, iret )	
C************************************************************************
C* GQTOI								*
C* 									*
C* This subroutine converts points in the graph coordinate system or	*
C* rotated grid lat/lon coordinates to the grid linear intermediate	*
C* coordinate system.							*
C* 									*
C* GQTOI  ( NP, XM, YM, XI, YI, IRET )					*
C* 									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C* 	XM (NP)		REAL		X map coordinates		*
C*	YM (NP)		REAL		Y map coordinates		*
C*									*
C* Output parameters:							*
C*	XI (NP)		REAL		X linear coordinates		*
C* 	YI (NP)		REAL		Y linear coordinates		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	6/86	Corrected names of scaling terms for	*
C*				graph transformations			*
C* M. desJardins/GSFC	10/86	Added polar coordinate system		*
C* M. desJardins/GSFC	 2/87	Corrected polar coord when graph not set*
C* M. desJardins/GSFC	 1/88	Missing data fix			*
C* M. desJardins/GSFC	 6/88	Adapted from GMTOG			*
C* K. Brill/EMC		 3/96	Made from GMTOI for general rotated proj*
C* K. Brill/EMC		 6/98	Rotate L coordinates for WAFS grids	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'ERROR.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xm (*), ym (*), xi (*), yi (*)
C*
C-------------------------------------------------------------------------
C*	Transform from map to linear intermediate grid coordinates.
C*	Select code for current mode.
C
	IF  ( ( igmode .eq. 1 ) .and. ( mgset ) ) THEN
C*
	    IF  ( mgclas .eq. MCCYL ) THEN
C*
		IF ( jtmtyp .eq. 1 .or. jtmtyp .eq. 3 ) THEN
		    xang2 = gangr2
		ELSE
		    xang2 = 0.0
		END IF
	    	CALL GCYLML ( mgproj, mgsppj, np, xm, ym, gangr1,
     +			      xang2, gangr3, gscmcd, grvrxy,
     +			      xi, yi, iret )
C
C*		Rotate L coordinates.
C
		IF ( rotagl ) CALL GROTTL ( 'G', +1, np, xi, yi, ier ) 
C*
	      ELSE IF  ( mgclas .eq. MCAZM ) THEN
		IF ( jtmtyp .eq. 1 ) THEN
		    xang2 = gangr2 + HALFPI
		ELSE
		    xang2 = 0.0
		END IF
 	    	CALL GAZMML ( mgproj, mgsppj, np, xm, ym, gangr1,
     +			      xang2, gangr3, gazsav, xi, yi, iret )
C*
	      ELSE IF  ( mgclas .eq. MCCON ) THEN
		IF ( jtmtyp .eq. 1 ) THEN
		    xang2 = gangr2
		ELSE
		    xang2 = -HALFPI
		END IF
	    	CALL GCONML ( mgproj, mgsppj, np, xm, ym, gangr1,
     +			      xang2, gangr3, gconcn, xi, yi, iret )
C*
	      ELSE
	    	iret = NIPROJ
	    END IF
C*
	  ELSE IF  ( ( igmode .eq. 2 ) .and. ( ggset ) )THEN
C
C*	    Check for special case of translating to and from polar coordinates.
C*	    The map coordinates are transformed to/from r, theta if
C*	    necessary to correspond to the grid coordinate definition.
C
	    IF  ( ( jgxtyp .eq. 5 ) .and. ( jxtyp .ne. 5 ) .and. 
     +					gset )  THEN
		DO  i = 1, np
		    CALL XYTORT ( xm (i), ym (i), r, theta, ier )
		    xm (i) = r
		    ym (i) = theta
		END DO
C
	      ELSE IF ( (jgxtyp .ne. 5) .and. (jxtyp .eq. 5) ) THEN
		DO  i = 1, np
		    CALL RTTOXY ( xm (i), ym (i), x, y, ier )
		    xm (i) = x
		    ym (i) = y
		END DO
	    END IF
C
C*	    Transform graph coordinates to linear intermediate coords.
C
	    jx = jgxtyp
	    jy = jgytyp
	    IF ( jx .eq. 5 ) jx = 1
	    IF ( jy .eq. 5 ) jy = 1
	    CALL GGRFML ( jx, jy, np, xm, ym, gglx1, gglx0, 
     +			  ggly1, ggly0, xi, yi, iret )
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
