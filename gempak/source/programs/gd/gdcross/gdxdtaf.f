    	SUBROUTINE GDXDTAF ( iflno, gdatim, gvcord, ystrt,
     +                       ystop, gfunc, time, ivcord,
     +			     rgx, rgy, nhxs, grid, rlvl, xgrd,
     +			     nvxs, parm, ybeg, yend, iret )
C************************************************************************
C* GDXDTAF								*
C*									*
C* This subroutine gets the data to contour for a cross section.	*
C*									*
C* GDXDTAF  ( IFLNO, GDATIM, GVCORD, YSTRT, YSTOP, GFUNC, 		*
C*           TIME, IVCORD, RGX, RGY, NHXS, GRID, RLVL, XGRD,		*
C*           NVXS, PARM, YBEG, YEND, IRET )				*
C*									*
C* Input parameters:							*
C*      IFLNO             INTEGER       Grid file number                *
C*	GDATIM		  CHAR*		User input date/time		*
C*	GVCORD		  CHAR*		User input vert coord		*
C*      YSTRT             REAL          Bottom vert coord value		*
C*      YSTOP             REAL          Top vert coord value		*
C*	GFUNC	 	  CHAR*		User input function		*
C*	TIME  (2)	  CHAR*		Time to search for levels	*
C*	IVCORD		  INTEGER	Vertical coordinate for search	*
C*	RGX  (NHXS)	  REAL		X grid coordinates		*
C*	RGY  (NHXS)	  REAL		Y grid coordinates		*
C*	NHXS		  INTEGER	Number of xsect pts in horiz.	*
C*									*
C* Work parameters:							*
C*      GRID (*)	  REAL		Array used to read grid		*
C*									*
C* Output parameters:							*
C*      RLVL (NVXS)	  REAL		Vertical levels in grid		*	
C*	XGRD (NHXS, NVXS) REAL		Array of cross section values	*
C*      NVXS              INTEGER       Number of xsect pts in vert.	*
C*	PARM		  CHAR*		Parameter name			*
C*	YBEG		  REAL		Beginning y value of grid	*
C*      YEND		  REAL          Ending y value of grid		*
C*	IRET		  INTEGER	Return code			*
C*					  7 = GFUNC not specified	*
C*					  0 = normal return		*
C*					 -6 = GVCORD is invalid		*
C*					-12 = no levels found		*
C*					-13 = @level not allowed	*
C*					-14 = %vcord not allowed	*
C*				        -19 = GFUNC is not valid	*
C**									*
C* Log:									*
C* K. F. Brill/GSC       6/89   Created from GDPDTA			*
C* K. Brill/GSC         11/89   DATTIM(2)=' ' for GD_GLEV		*
C* K. Brill/GSC         12/89   Subset vertical levels to save time	*
C* S. Schotz/GSC	 6/90	Remove respnd flag			*
C* K. Brill/NMC		 1/91  	Add RLVLM1 for layer quantities, and	*
C*				clean up				*
C* K. Brill/HPC		 5/02	Allow cross section path to cross grid	*
C*				discontinuity.				*
C* R. Tian/SAIC		10/02	Remove COMMON/GDXS, GR_RARG, GSGPRJ	*
C* K. Brill/HPC		 2/04	Changes for new DG FT mngnmnt		*
C* S. Gilbert/NCEP	 7/08	From GDXDTA; Added work array argument	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gvcord, gfunc, time (2), parm
        REAL		rgx ( nhxs ), rgy ( nhxs )
	REAL		xgrd ( * ), rlvl  ( * )
C*
	LOGICAL		havsfc, lavflg
C*
	CHARACTER	dattim (2)*20, glevel*20, pfunc*80, cbuf*8
	INTEGER		lev (2)
	REAL		grid ( * )
C*
	INTEGER		level ( 2, LLMXLV )
C------------------------------------------------------------------------
	iret = 0
C
C*	Check to see if the function involves a layer average.
C
	CALL GDXCLA ( gfunc, lavflg, iret )
	IF ( iret .ne. 0 ) RETURN
C 
C* 	Check to see if GFUNC contains @level or %vcord specifiers.
C
	iat = index ( gfunc, '@' )
	IF ( iat .ne. 0 ) THEN
	  iret = -13
	  CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	  RETURN
	END IF
	iat = index ( gfunc, '%' )
	IF ( iat .ne. 0 ) THEN
	  iret = -14
	  CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	  RETURN
	END IF
C
C*      Set interpolation type.
C
        inttyp = 1
C
C*	Set the grid relative direction of the tangent vector.  (This
C*	is like a conventional wind direction with the y axis pointing
C*	north.)
C
	CALL GDXDXY ( gddx, gddy, iret )
	IF ( iret .ne. 0 ) RETURN
	dvx = ( rgx ( nhxs ) - rgx ( 1 ) ) * gddx
	dvy = ( rgy ( nhxs ) - rgy ( 1 ) ) * gddy
	ornang = ATAN2 ( -dvx, -dvy)
	CALL DG_OANG ( ornang, ier )
C
C*	Get levels which might have data.
C*	First translate date/time and vertical coordinate.
C
	dattim ( 1 ) = time ( 1 )
	dattim ( 2 ) = ' '
    	CALL DG_GLEV  ( iflno, dattim, ivcord, LLMXLV, level, nlev, ier )
	IF  ( nlev .eq. 0 )  THEN
	    iret = -12
	    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	    RETURN
	END IF
C
C*      Float and sort the levels.
C
	IF  ( ivcord .eq. 0 )  THEN
	    iret = -6
	    CALL ER_WMSG  ( 'GDCROSS', iret, ' ', ier )
	    RETURN
	END IF
C
C*      Load the levels for sorting and look for surface.
C
	havsfc = .false.
	DO i = 1, nlev
	   rlvl ( i ) = FLOAT ( level ( 1, i ) )
	   IF ( level ( 1, i ) .eq. 0 ) havsfc = .true.
	END DO
C
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
	IF ( iret .ne. 0 ) THEN
	   RETURN
	END IF
C
C*	Select the range of levels included by ystrt -- ystop.
C
	i = 1
	IF ( havsfc ) i = 2
	istrt = 0
	istop = 0
	DO WHILE ( i .lt. nlev .and.
     +           (istrt .eq. 0 .or. istop .eq. 0 ) )
	  i = i + 1
	  IF ( (ystrt .ge. rlvl ( i-1 ) .and. ystrt .lt. rlvl ( i ) )
     +                               .or.
     +         (ystrt .le. rlvl ( i-1 ) .and. ystrt .gt. rlvl ( i ) ) ) 
     +      istrt = i - 1
	  IF ( (ystop .gt. rlvl ( i-1 ) .and. ystop .le. rlvl ( i ) )
     +                               .or.
     +         (ystop .lt. rlvl ( i-1 ) .and. ystop .ge. rlvl ( i ) ) )
     +      istop = i
	END DO
	IF ( istrt .eq. 0 ) istrt = 1
	IF ( istop .eq. 0 ) istop = nlev
	IF ( lavflg ) THEN
	  IF ( istrt .lt. 2 ) istrt = 2
	  istop = istop + 1
	  IF ( istop .gt. nlev ) istop = nlev
	END IF
	IF ( havsfc .and. istrt .lt. 2 ) istrt = 2
	IF ( lavflg .and. havsfc .and. istrt .lt. 3 ) istrt = 3
C
C*	Loop through single levels finding data.  Also, reset rlvl
C*   	values so that rlvl(1) is the first level in the cross section.
C
	ilev = 0
	indx = 1
	IF ( lavflg ) rlvlm1 = rlvl ( istrt - 1 )
	DO  i = istrt, istop, 1
C
C*		Encode level and compute function.
C
	 	IF ( lavflg ) THEN
	 	  ilvl1 = int ( rlvl ( i ) )
	  	  ilvl2 = int ( rlvlm1 )
		  CALL ST_INLN ( ilvl1, glevel, lnth, ier )
	          CALL ST_INCH ( ilvl2, cbuf, ier )
	          glevel = glevel(1:lnth) // ':' // cbuf
		ELSE
		  intlvl = int ( rlvl ( i ) )
		  CALL ST_INCH  ( intlvl, glevel, ier )
	        END IF
		CALL DG_GRIDN ( gdatim, glevel, gvcord, gfunc, 
     +				pfunc,  grid,   kx, ky, dattim, lev, 
     +				jvcord, parm, ier )
		IF  ( ier .eq. 0 )  THEN
C
C*		    Interpolate to cross section points.
C
		    CALL GR_INTP  ( inttyp, 
     2               rgx, rgy, nhxs, kx, ky, grid, xgrd ( indx ), ier )
C
C*		    Increment the cross section grid array index.
C
	            indx = indx + nhxs
	            ilev = ilev + 1
		    IF ( lavflg ) THEN
	              IF ( ivcord .eq. 1 ) THEN
			rlvl ( ilev ) = SQRT ( rlvl ( i ) * rlvlm1 )
		      ELSE
	                rlvl ( ilev ) = .5 * ( rlvl ( i ) + rlvlm1 )
		      END IF
		      rlvlm1 = rlvl ( i )
		    ELSE
		      rlvl ( ilev ) = rlvl ( i )
		    END IF
	        END IF
	END DO
	IF ( ilev .eq. 0 ) THEN
	  iret = ier
	  CALL ER_WMSG ( 'DG', iret, pfunc, ier )
	  iret = -19
	  CALL ER_WMSG ( 'GDCROSS', iret, ' ', ier )
	  RETURN
	END IF
C*
	ybeg = rlvl ( 1 )
	yend = rlvl ( ilev )
C
C*      Set the second output array dimension (nhxs,nvxs).
C
        nvxs = ilev
C*
	RETURN
	END
