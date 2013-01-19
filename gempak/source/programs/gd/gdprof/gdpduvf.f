	SUBROUTINE GDPDUVF  ( iflno, gdatim, gvcord, gvect, gpoint, 
     +			     time, ivcord, ystrt, ystop,
     +   		     gridu, gridv, rgx, rgy, rlat, rlon,
     +			     npts, u, v, y, parmu, parmv, iret )
C************************************************************************
C* GDPDUVF								*
C*									*
C* This subroutine gets vector data to plot for a profile.		*
C*									*
C* GDPDUVF  ( IFLNO, GDATIM, GVCORD, GVECT, GPOINT, TIME, IVCORD,	*
C*           YSTRT, YSTOP, RGX, RGY, RLAT, RLON, NPTS, U, V, Y, PARMU,	*
C*	     PARMV, IRET )						*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		Grid file number		*
C*	GDATIM		CHAR*		User input date/time		*
C*	GVCORD		CHAR*		User input vert coord		*
C*	GVECT		CHAR*		User input function		*
C*	GPOINT		CHAR*		User input point to plot	*
C*	TIME  (2)	CHAR*		Time to search for levels	*
C*	IVCORD		INTEGER		Vertical coordinate for search	*
C*      YSTRT	 	REAL            Starting vert coord value	*
C*      YSTOP           REAL            Stopping vert coord value       *
C*									*
C* Work parameters:							*
C*      GRIDU(*)        REAL            work array for grid		*
C*      GRIDV(*)        REAL            work array for grid		*
C*									*
C* Output parameters:							*
C*	RGX		REAL		X grid coordinate		*
C*	RGY		REAL		Y grid coordinate		*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	NPTS		INTEGER		Number of points		*
C*	U    (NPTS)	REAL		u components 			*
C*      V    (NPTS)     REAL 		v components			*
C*	Y    (NPTS)	REAL		Y coordinates			*
C*	PARMU		CHAR*		Parameter name for u comp	*
C*      PARMV           CHAR*           Parameter name for v comp	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid grid point	*
C*					 -9 = no valid points		*
C*					-10 = no levels at this time	*
C**									*
C* Log:									*
C* K. Brill/GSC		11/89   Created from GDPDTA			*
C* S. Schotz/GSC	 6/90	Removed respnd flag			*
C* K. Brill/NMC		01/93	Made surface treatment same as GDPDTA	*
C* P. Bruehl/NWS	 7/96	Initialized gdrel to FALSE		*
C* D. Keiser/GSC	 8/96	Clean up				*
C* R. Tian/SAIC		10/02	Change call to DG_CXGP			*
C* K. Brill/HPC		12/02	Pass blank IJSKIP into DG_CXGP		*
C* R. Tian/SAIC          4/04   Changes for new DG FT mngnmnt           *
C* S. Gilbert/NCEP       8/07   From GDPDUV;  added work arrays         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdatim, gvcord, gvect, gpoint, time (2), parmu,
     +                  parmv
	REAL		u (*), v (*), y (*)
	LOGICAL		havsfc, lavflg, havgfs, grdrel
C*
	CHARACTER       gvecx*80
	CHARACTER	dattim (2)*20, glevel*20, pfunc*80, vcparm*4
	CHARACTER	ggddtt*48, pparm*12, cbuf*8, dparm*6, cfile
	INTEGER		lev (2)
	REAL		gridu ( * ), gridv ( * ),
     +			rlvl ( LLMXLV )
C*
	INTEGER		level ( 2, LLMXLV )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	npts = 0
	grdrel = .false.
C
C*	Find plotting location.
C
	CALL DG_CXGP  ( gpoint, ' ', 1, np, rgx, rgy, rlat, rlon, iret )
	IF ( iret .ne. 0 ) RETURN
C
C*	Check to see if grid relative flag (G) is appended to GVECT.
C
	CALL ST_LCUC ( gvect, gvecx, iret )
	IF ( iret .ne. 0 ) RETURN
	iat = index ( gvecx, '/G' )
	IF ( iat .ne. 0 ) THEN
	  grdrel = .true.
	  gvecx = gvecx ( : iat-1 )
	END IF
	
C
C*	Check to see if the function involves a layer average.
C
	CALL GDPCLA ( gvecx, lavflg, iret )
	IF ( iret .ne. 0 ) RETURN	
C
C*	Get levels which might have data.
C*	First translate date/time and vertical coordinate.
C
	dattim ( 1 ) = time ( 1 )
	dattim ( 2 ) = ' '
	CALL DG_GLEV  ( iflno, dattim, ivcord,
     +			LLMXLV, level, nlev, ier )
	IF  ( nlev .eq. 0 )  THEN
	    iret = -10
	    CALL ER_WMSG  ( 'GDPROF', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Float the levels for sorting and look for surface.
C
	havsfc = .false.
	DO i = 1, nlev
	  rlvl ( i ) = FLOAT ( level ( 1, i ) )
	  IF ( level ( 1, i ) .eq. 0 ) havsfc = .true.
	END DO
C*
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
C
C*      Assign all levels, temporarily.
C
	DO i = 1, nlev
	  y ( i ) = rlvl ( i )
	END DO
C
C*      If it exists, retrieve vert. coord. value at the surface.
C
	ysfc = RMISSD
	IF ( havsfc ) THEN
		        glevel = '0'
C
C*			Set time, and vertical coordinate to use.  If
C*			retrieve is successful, interpolate value; 
C*			otherwise set sfc vert. coord. value to missing.
C
			CALL LV_CCRD ( ivcord, vcparm, ier )
			ggddtt = time (1)
C
C*			Convert file number to character.
C
			CALL ST_INCH ( iflno, cfile, ier )
		        dparm = vcparm // '+' // cfile
			CALL DG_GRIDN ( ggddtt, glevel, vcparm, dparm,
     +				       pfunc, gridu, kx, ky,
     +				       dattim, lev, jvcord, pparm, ier )
			IF  ( ier .ge. 0 )  THEN
			    CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, 
     +					    gridu, ysfc, ier )
			ELSE
			    ysfc = RMISSD
			END IF
C*
	END IF
C
C*	Set surface value.
C
	IF ( havsfc .and. .not. ERMISS ( ysfc ) )
     +          y ( 1 ) = ysfc
	IF ( havsfc .and. ERMISS ( ysfc ) ) THEN	
	    ii = 0
	    DO i = 2, nlev
	    	ii = ii + 1
             	y ( ii ) = rlvl ( i )
	    	rlvl ( ii ) = y ( ii )
       	    END DO
	    nlev = nlev - 1
	    havsfc = .false.
	END IF
C
C*	If level 0 (surface) is present, then HAVSFC is T; 
C* 	otherwise HAVSFC is F.  If GVECX can be computed at
C*	level 0, then HAVGFS is T.
C
	havgfs = .false.
	IF ( havsfc .and. ( .not. lavflg ) ) havgfs = .true.
C*
C
C*	Do subset in the vertical--eliminate unneeded levels.
C
	IF ( havsfc ) THEN
	    IF ( ivcord .eq. 1 .and. ystrt .gt. ysfc ) THEN
	    	ys1 = ysfc
	    ELSE IF ( ivcord .ne. 1 .and. ystrt .lt. ysfc ) THEN
	    	ys1 = ysfc
            ELSE
	    	ys1 = ystrt
            END IF
	ELSE
	    ys1 = ystrt
	END IF
	IF ( ys1 .eq. ystrt ) havgfs = .false.
	i = 1
	istrt = 0
	istop = 0
	DO WHILE ( i .lt. nlev .and.
     +           (istrt .eq. 0 .or. istop .eq. 0 ) )
	  i = i + 1
	  IF ( (ys1 .ge. y ( i-1 ) .and. ys1 .lt. y ( i ) )
     +                               .or.
     +         (ys1 .le. y ( i-1 ) .and. ys1 .gt. y ( i ) ) ) THEN
	      istrt = i - 1
	      IF ( havgfs ) rlvl ( istrt ) = rlvl ( 1 )
	  END IF
	  IF ( (ystop .gt. y ( i-1 ) .and. ystop .le. y ( i ) )
     +                               .or.
     +         (ystop .lt. y ( i-1 ) .and. ystop .ge. y ( i ) ) )
     +      istop = i
	END DO
	IF ( istrt .eq. 0 ) istrt = 1
	IF ( istop .eq. 0 ) istop = nlev
	IF ( lavflg ) THEN
	    istrt = istrt - 1
	    istop = istop + 1
	    IF ( istrt .lt. 1 ) istrt = 1
	    IF ( istop .gt. nlev ) istop = nlev
	END IF
C
C*	Loop through single levels finding data.
C
	DO i = istrt, istop
	    IF ( i .eq. 1 .and. lavflg ) THEN
C
C*	    	Do nothing.
C
            ELSE IF ( i .eq. 2 .and. lavflg .and. havsfc ) THEN
C
C*	     	Do nothing.
C
	    ELSE
C
C*		Encode level and compute function.
C
	    	IF ( lavflg ) THEN
  		    ilvl1 = int ( rlvl ( i ) )
	            ilvl2 = int ( rlvl ( i - 1 ) )
		    CALL ST_INLN ( ilvl1, glevel, lnth, ier )
		    CALL ST_INCH ( ilvl2, cbuf, ier )
		    glevel = glevel(1:lnth) // ':' // cbuf
	 	ELSE
	            intlvl = int ( rlvl ( i ) )
	 	    CALL ST_INCH ( intlvl, glevel, ier )
		END IF
C*
	 	IF ( .not. grdrel ) THEN
		    CALL DG_VECTN ( gdatim, glevel, gvcord, gvecx, 
     +				   pfunc,  gridu, gridv, kx, ky, dattim,
     +		                   lev, jvcord, parmu, parmv, ierdg )
		ELSE
	            CALL DG_VECRN ( gdatim, glevel, gvcord, gvecx, 
     +				   pfunc,  gridu, gridv, kx, ky, dattim,
     +		                   lev, jvcord, parmu, parmv, ierdg )
	        END IF
C*
		IF  ( ierdg .eq. 0 )  THEN
C
C*		    Check that grid includes point to be found.
C
		    rkx = FLOAT ( kx )
		    rky = FLOAT ( ky )
		    IF  ( ( rgx .gt. rkx ) .or. ( rgy .gt. rky ) )  THEN
			iret = -4
			RETURN
		    END IF
C
C*		    Interpolate to correct point.
C
		    CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, gridu, uu, 
     +				    ieru )
		    CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, gridv, vv, 
     +				    ierv )
		    ier = ieru + ierv
	            IF ( ier .eq. 0 .and. ( .not. ERMISS ( uu ) .and.
     +                   .not. ERMISS ( vv ) ) ) THEN
                        npts = npts + 1
	                u (npts) = uu
		        v (npts) = vv
		        IF ( lavflg ) THEN
		      	    IF ( ivcord .eq. 1 ) THEN
			   	y ( npts ) = SQRT ( rlvl ( i ) *
     +			       		       rlvl ( i - 1 ) )
		            ELSE
		          	y ( npts ) = .5 * ( rlvl ( i ) +
     +					       rlvl ( i - 1 ) )
		            END IF
		      	ELSE
	                    y (npts) = rlvl ( i )
		      	END IF
		    ELSE IF ( i .eq. istrt .and. havgfs ) THEN
		      	havgfs = .false.
	            END IF
		ELSE IF ( i .eq. istrt .and. havgfs ) THEN
                    havgfs = .false.
		END IF
	    END IF
	END DO
C
C*	Check that there are some points.
C
	IF  ( npts .le. 0 )  THEN
	    CALL ER_WMSG  ( 'DG', ierdg , pfunc, ier )
	    iret = -9
	    CALL ER_WMSG  ( 'GDPROF', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Set vertical coordinate for gvecx value at surface.
C
	IF ( havgfs ) y ( 1 ) = ysfc 
C*
	RETURN
	END
