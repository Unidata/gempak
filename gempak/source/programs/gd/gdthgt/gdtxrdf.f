	SUBROUTINE GDTXRDF ( time, gvcord, gfunc, levt, levb, lavflg,
     +			    rgx, rgy, inttyp, grid, yy, iret)
C************************************************************************
C* GDTXRDF								*
C*									*
C* This subroutine actually gets the data at a particular level		*
C*									*
C* GDTXRDF ( TIME, GVCORD, GFUNC, LEVT, LEVB, LAVFLG, RGX, RGY, INTTYP,	*
C*	     GRID, YY, IRET)						*
C*									*
C* Input parameters:							*
C*	TIME(2)		  CHAR*		Time to read			*
C*	GVCORD		  CHAR*		User vertical coord		*
C*	GFUNC		  CHAR*		User function			*
C*	LEVT		  INTEGER	Top level (or level if only 1)	*
C*	LEVB		  INTEGER	Bottom level			*
C*	LAVFLG		  LOGICAL	Flag for using layer average	*
C*	RGX(1)		  REAL		Grid point x			*
C*	RGY(1)		  REAL		Grid point y			*
C*	INTTYP		  INTEGER	Interpolation type		*
C*									*
C* Work parameters:                                                     *
C*      GRID(*)         REAL            Work array for reading in grid  *
C*                                                                      *
C* Output parameters:							*
C*	YY(1)		  REAL		Output data (missing ok)	*
C*	IRET		  INTEGER	Return code			*
C*					  non zero if a problem		*
C**									*
C* Log:									*
C* T.W.Barker/WR/SSD	 8/91	                       			*
C* R. Tian/SAIC		10/04	Added TG_DUAL call			*
C* S. Gilbert/NCEP       8/07   From GDTXRD; added work array args      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	time(2), gvcord, gfunc 
	CHARACTER*80	parm
        REAL		rgx(1) , rgy(1) , yy(1)
C*
	LOGICAL		lavflg 
C*
	CHARACTER	dattim (2)*20, glevel*20, pfunc*80, cbuf*8,
     +			dualtm*40
	INTEGER		lev (2)
	REAL		grid ( * )
C*
C*
C------------------------------------------------------------------------

	yy(1) = RMISSD
	IF ( lavflg ) THEN
	    CALL ST_INLN ( levt, glevel, lnth, iret )
	    CALL ST_INCH ( levb, cbuf, iret )
	    glevel = glevel ( 1:lnth ) // ':' // cbuf
	ELSE
	    CALL ST_INCH ( levt, glevel, iret )
	END IF
	CALL TG_DUAL ( time, dualtm, ier )
	CALL DG_GRIDN ( dualtm, glevel, gvcord, gfunc, pfunc, grid, kx,
     +		       ky, dattim, lev, ivc, parm, iret )
	IF ( iret .eq. 0 ) THEN
	    rkx = float (kx)
	    rky = float (ky)
	    IF ( (rgx (1) .gt. rkx ) .or. ( rgy (1) .gt. rky ) ) THEN
		iret = -4
		RETURN
	    END IF
	    CALL GR_INTP ( inttyp, rgx, rgy, 1, kx, ky, grid, yy, iret )
	END IF
	RETURN
	END
