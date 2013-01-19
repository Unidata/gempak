	SUBROUTINE GDTXRVF ( time, gvcord, gvecx, levt, levb, lavflg,
     +			    rgx, rgy, inttyp, lvert, gridu, gridv,
     +                      uu, vv, iret )
C************************************************************************
C* GDTXRVF								*
C*									*
C* This subroutine actually gets the wind data at a particular level	*
C*									*
C* GDTXRVF ( TIME, GVCORD, GVECX, LEVT, LEVB, LAVFLG, RGX, RGY, INTTYP,	*
C*	    LVERT, GRIDU, GRIDV, UU, VV, IRET)				*
C*									*
C* Input parameters:							*
C*	TIME(2)		  CHAR*		Time to read			*
C*	GVCORD		  CHAR*		User vertical coord		*
C*	GVECX		  CHAR*		User function			*
C*	LEVT		  INTEGER	Top level (or level if only 1)	*
C*	LEVB		  INTEGER	Bottom level			*
C*	LAVFLG		  LOGICAL	Flag for using layer average	*
C*	RGX(1)		  REAL		Grid point x			*
C*	RGY(1)		  REAL		Grid point y			*
C*	INTTYP		  INTEGER	Interpolation type		*
C*	LVERT		  LOGICAL	Vector type non-relative	*
C*                                                                      *
C* Work parameters:                                                     *
C*      GRIDU(*)        REAL            work array for grid             *
C*      GRIDV(*)        REAL            work array for grid             *
C*									*
C* Output parameters:							*
C*	UU(1)		  REAL		Output data u (missing ok)	*
C*	VV(1)		  REAL		Output data v (missing ok)	*
C*	IRET		  INTEGER	Return code			*
C*					  non zero if a problem		*
C**									*
C* Log:									*
C* T.W.Barker/WR/SSD	 8/91	                       			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	time(2), gvcord, gvecx 
	CHARACTER*80	parmu, parmv
        REAL		rgx(1) , rgy(1) , uu(1), vv(1)
C*
	LOGICAL		lavflg , lvert
C*
	CHARACTER	dattim (2)*20, glevel*20, pfunc*80, cbuf*8
	INTEGER		lev (2)
	REAL		gridu ( * ), gridv ( * )
C*
C*
C------------------------------------------------------------------------
C
	uu(1)=RMISSD
	vv(1)=RMISSD

	IF ( lavflg ) THEN
	    CALL ST_INLN (levt, glevel, lnth, iret)
	    CALL ST_INCH (levb, cbuf, iret)
	    glevel = glevel(1:lnth)//':'//cbuf
	ELSE
	    CALL ST_INCH (levt, glevel, iret)
	END IF
	if ( .not. lvert ) then
	    CALL DG_VECTN ( time, glevel, gvcord, gvecx, pfunc, gridu,
     +			   gridv, kx, ky, dattim, lev, ivc, parmu, 
     +			   parmv, iret )
	ELSE
	    CALL DG_VECRN ( time, glevel, gvcord, gvecx, pfunc, gridu,
     +			   gridv, kx, ky, dattim, lev, ivc, parmu,
     +			   parmv, iret )
	END IF
	IF ( iret .eq. 0 ) THEN
	   rkx = float (kx)
	   rky = float (ky)
	   IF ( (rgx (1) .gt. rkx ) .or. ( rgy (1) .gt. rky ) ) THEN
	      iret = -4
	      RETURN
	   END IF
 	   CALL GR_INTP (inttyp, rgx, rgy, 1, kx, ky, gridu, uu, iret )
 	   CALL GR_INTP (inttyp, rgx, rgy, 1, kx, ky, gridv, vv, iret )
	END IF
	RETURN
	END
