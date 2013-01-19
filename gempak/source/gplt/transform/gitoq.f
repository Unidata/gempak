	SUBROUTINE GITOQ  ( np, xg, yg, xm, ym, iret )
C************************************************************************
C* GITOQ								*
C*									*
C* This subroutine converts points in the grid linear intermediate	*
C* coordinate system into values in the rotated grid lat/lon coordinate *
C* system.								*
C*									*
C* GITOQ  ( NP, XG, YG, XM, YM, IRET )					*
C*									*
C* Input parameters:							*
C*	NP		INTEGER		Number of points		*
C*	XG (NP)		REAL		X input coordinates		*
C*	YG (NP)		REAL		Y input coordinates		*
C*									*
C* Output parameters:							*
C*	XM (NP)		REAL		X output coordinates		*
C*	YM (NP)		REAL		Y output coordinates		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	10/86	Added polar coordinates			*
C* M. desJardins/GSFC	 2/87	Fixed polar coords when graph not set	*
C* M. desJardins/GSFC	 1/88	Fixed missing data			*
C* M. desJardins/GSFC	 6/88	Adapted from GGTOM			*
C* K. Brill/EMC		 3/96	Made from GITOM for general rotated proj*
C* K. Brill/EMC		 6/98	Rotate L coordinates for WAFS grids	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'XYDEF.CMN'
C*
	REAL		xg (*), yg (*), xm (*), ym (*)
C*
C------------------------------------------------------------------------
C*	Transform from linear intermediate to grid coordinates.
C
	IF  ( igmode .eq. 2 )  THEN
C
C*	  Check that a graph has been defined.
C
	  IF ( .not. ggset ) THEN
	    iret = NIPROJ
	   ELSE
	    iret = NORMAL
C
C*	    If grid is in polar coordinates, simply transform to 
C*	    r, theta at this point.
C
	    jx = jgxtyp
	    jy = jgytyp
	    IF  ( jx .eq. 5 ) jx = 1
	    IF  ( jy .eq. 5 ) jy = 1
	    CALL GGRFLM ( jx, jy, np, xg, yg, gglx1, gglx0, ggly1,
     +			  ggly0, xm, ym, iret )
C
C*	    Special cases for translating to and from polar coordinates.
C*	    The map coordinates are transformed to/from r, theta if
C*	    necessary to correspond to graph coordinate definition.
C
	    IF  ( (jgxtyp .eq. 5) .and. 
     +		  ( (jxtyp .ne. 5) .and. gset) ) THEN
		DO  i = 1, np
		    CALL RTTOXY ( xm (i), ym (i), x, y, ier )
		    xm (i) = x
		    ym (i) = y
		END DO
	      ELSE IF ( (jgxtyp .ne. 5) .and. (jxtyp .eq. 5 ) ) THEN
		DO  i = 1, np
		    CALL XYTORT ( xm (i), ym (i), r, theta, ier )
		    xm (i) = r
		    ym (i) = theta
		END DO
	    END IF
	  END IF
C
C*	  Map mode.
C
	 ELSE IF  ( igmode .eq. 1 )  THEN
C
	    IF  ( .not. mgset ) THEN
	        iret = NIPROJ
C*
	      ELSE IF  ( mgclas .eq. MCAZM ) THEN
		IF ( jtmtyp .eq. 1 ) THEN
		    xang2 = gangr2 + HALFPI
		ELSE
		    xang2 = 0.0
		END IF
	        CALL GAZMLM ( mgproj, mgsppj, np, xg, yg, gangr1,
     +                        xang2,
     +			      gangr3, gazsav, xm, ym, iret )
C*
	      ELSE IF  ( mgclas .eq. MCCON ) THEN
		IF ( jtmtyp .eq. 1 ) THEN
		    xang2 = gangr2
		ELSE
		    xang2 = -HALFPI
		END IF
	        CALL GCONLM ( mgproj, mgsppj, np, xg, yg, gangr1,
     +                        xang2,
     +			      gangr3, gconcn, xm, ym, iret )
C*
	      ELSE IF  ( mgclas .eq. MCCYL ) THEN
		IF ( jtmtyp .eq. 1 .or. jtmtyp .eq. 3 ) THEN
		    xang2 = gangr2
		ELSE
		    xang2 = 0.0
		END IF
C
C*		Rotate L coordinates.
C
		IF ( rotagl ) CALL GROTTL ( 'G', -1, np, xg, yg, ier )
	        CALL GCYLLM ( mgproj, mgsppj, np, xg, yg, gangr1,
     +                        xang2,
     +			      gangr3, gscmcd, grvrxy, xm, ym, iret )
	    END IF
	  ELSE
	    iret = NIMODE
	END IF
C*
	RETURN
	END
