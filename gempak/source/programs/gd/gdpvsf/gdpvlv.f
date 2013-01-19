	SUBROUTINE GDPVLV  ( iflno, gdatim, gvcord, gfunc, glist, nlist,
     +			     time, ivcord, ystrt, ystop, ylev, ipmax,
     +			     ogrid, kx, ky, igrids, iret )
C************************************************************************
C* GDPVLV								*
C*									*
C* This subroutine interpolates to a functional surface.		*
C*									*
C* GDPDTA  ( IFLNO, GDATIM, GVCORD, GFUNC, GLIST, NLIST, TIME, IVCORD,  *
C*	     YSTRT, YSTOP, YLEV, OGRID, KX, KY, KGRIDS, IRET )		*
C*									*
C* Input parameters:							*
C*	IFLNO		INTEGER		Grid file number		*
C*	GDATIM		CHAR*		User input date/time		*
C*	GVCORD		CHAR*		User input vert coord		*
C*	GFUNC		CHAR*		User input function		*
C*	GLIST (*)	CHAR*		List of output grids		*
C*	NLIST		INTEGER		Number of output grids		*
C*	TIME  (2)	CHAR*		Time to search for levels	*
C*	IVCORD		INTEGER		Vertical coordinate for search	*
C*      YSTRT	 	REAL            Starting vert coord value	*
C*      YSTOP           REAL            Stopping vert coord value       *
C*	YLEV		REAL		Desired parameter level		*
C*	IPMAX		INTEGER		Maximum allowable pressure	*
C*									*
C* Output parameters:							*
C*	OGRID(LLMXGD,*)	REAL		Output grid			*
C*	KX		INTEGER		Number of x grid points		*
C*	KY		INTEGER		Number of y grid points		*
C*	IGRIDS		INTEGER		Number of output grids		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = invalid grid point	*
C*					 -9 = no valid points		*
C*					-10 = no levels at this time	*
C**									*
C* Log:									*
C* J. Nielsen/SUNYA	12/90	Adapted from gdpdta.for			*
C* J. N-G/TAMU		 5/96	Improved handling of missing levels	*
C* K.Tyle/UAlbany        4/03   Remove "GEMINC:" in INCLUDE     	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	(MAXLEV = 50)
	CHARACTER*(*)	gdatim, gvcord, gfunc, time (2), glist(*)
	REAL		ogrid ( LLMXGD, * ), igrid ( LLMXGD )
	LOGICAL		havsfc, lavflg, havgfs, gotone
C*
	CHARACTER	dattim (2)*20, glevel*20, pfunc*80 
	CHARACTER	cbuf*8, parm*12, gpfunc*12
	REAL		grid ( LLMXGD ), pgrid ( LLMXGD ), rlvl ( MAXLEV )
	REAL		tgrid ( LLMXGD, MAXLEV ), presgrid ( LLMXGD )
C*
	INTEGER		level ( 2, MAXLEV )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
d	type *,'in gdpvlv'
	iret = 0
	npts = 0
	gotone=.false.
	pmax=float(ipmax)
C
C*	Check to see if the function involves a layer average.
C
	CALL GDPVCL ( gfunc, lavflg, iret )
	IF ( iret .ne. 0 ) RETURN	
C
C*	Get levels which might have data.
C*	First translate date/time and vertical coordinate.
d	type *,'translating'
C
	dattim ( 1 ) = time ( 1 )
	dattim ( 2 ) = ' '
	CALL GD_GLEV  ( iflno, dattim, ivcord,
     +			LLMXLV, level, nlev, ier )
        IF  ( ier .ne. 0 )  THEN
            CALL ER_WMSG  ( 'DG', ier, pfunc, ier )
        ENDIF
	IF  ( nlev .eq. 0 )  THEN
	    iret = -10
	    CALL ER_WMSG  ( 'GDPVSF', iret, 
     +		'No data at this time and vertical coordinate.', ier )
	    RETURN
	ELSE IF  ( nlev .gt. 50 )  THEN
	    iret = -10
	    CALL ER_WMSG  ( 'GDPVSF', iret,
     +		'Exceeded maximum of fifty levels.', ier )
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
d	type *,'sorting'
	CALL LV_SORT ( ivcord, nlev, rlvl, iret )
C
C*      If surface value exists, delete it.
C
	IF ( havsfc ) then
	  DO i = 2, nlev
	    rlvl ( i-1 ) = rlvl ( i )
       	  END DO
	  nlev = nlev - 1
	  havsfc = .false.
	END IF
C
C*	Do subset in the vertical--eliminate unneeded levels.
C
	havgfs = .false.
	i = 1
	istrt = 0
	istop = 0
C*      Note: the following code is not always quite correct. -JN
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
C
C*	Orient search
C	
	IF ( ( ( ystrt - ystop .gt. 0 ) .and. ( ivcord .eq. 1 ) )
     +  .or. ( ( ystrt - ystop .lt. 0 ) .and. ( ivcord .ne. 1 ) ) ) THEN
	  idir = 1
	  IF ( istrt .eq. 0 ) istrt = 1
	  IF ( istop .eq. 0 ) istop = nlev
	ELSE
	  idir = -1
	  IF ( istrt .eq. 0 ) istrt = nlev
	  IF ( istop .eq. 0 ) istop = 1
	END IF
	IF ( lavflg ) THEN
	  istrt = istrt - idir
	  istop = istop + idir
	  IF ( istrt .lt. 1 ) istrt = 1
	  IF ( istop .gt. nlev ) istop = nlev
	  IF ( istrt .gt. nlev ) istrt = nlev
	  IF ( istop .lt. 1 ) istop = 1
	END IF
C
C*	Loop through single levels finding data.
C
d	type *,'about to begin loop'
	clev = RMISSD
	DO  i = istrt,istop,idir
	    IF  ( ( i .ne. 1 ) .or. ( .not. lavflg) ) THEN
C
C*		Encode level and compute function.
C
	    	IF ( lavflg ) THEN
  		  ilvl1 = int ( rlvl ( i ) )
	          ilvl2 = int ( rlvl ( i - 1 ) )
		  plev = clev
		  clev = float(ilvl1+ilvl2)/2.
		  pint = (float(i)*2.-1.)/2.
		  CALL ST_INLN ( ilvl1, glevel, lnth, ier )
		  CALL ST_INCH ( ilvl2, cbuf, ier )
		  glevel = glevel(1:lnth) // ':' // cbuf
	 	ELSE
	          intlvl = int ( rlvl ( i ) )
		  plev = clev
		  clev = float (intlvl)
		  pint = float (i)
	 	  CALL ST_INCH ( intlvl, glevel, ier )
		END IF
d	type *,'calling dggrid the first time'
		CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc, 
     +				pfunc,  grid,   kx, ky, dattim, lev, 
     +				jvcord, parm, ierdg )
                IF  ( ierdg .ne. 0 )  THEN
                  CALL ER_WMSG  ( 'DG', ierdg, pfunc, ier )
C
C*		Second chance if layer quantity
C
		  IF ( ( lavflg ) .and. ( i .ne. istop ) .and.
     +           .not. ( ( idir .eq. -1 ) .and. ( i .eq. 2 ) ) )  THEN
		    IF ( idir .eq. 1 ) THEN 
		      ilvl1 = int ( rlvl ( i + 1 ) )
		      pint = float(i)
		    ELSE
		      ilvl2 = int ( rlvl ( i - 2 ) )
		      pint = float(i-1)
		      CALL ST_INCH ( ilvl2, cbuf, ier )
		    END IF
                    CALL ST_INLN ( ilvl1, glevel, lnth, ier )
		    clev = float(ilvl1+ilvl2)/2.
		    glevel = glevel(1:lnth) // ':' // cbuf
d	type *,'calling dggrid the second time'
		    CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc,
     +                          pfunc,  grid,   kx, ky, dattim, lev,
     +                          jvcord, parm, ierdg )
                    IF  ( ierdg .ne. 0 )  THEN
                      CALL ER_WMSG  ( 'DG', ierdg, pfunc, ier )
		    ELSE
		      gotone=.true.
		    END IF
		  END IF
                ELSE
		  gotone=.true.
		END IF
C
C*		Don't try to interpolate if we don't have a function
C
		IF ( ierdg .ne. 0 )  THEN
		  clev = plev
		  GOTO 1000
		END IF
C
C*		Compute pressure (if using ipmax)
C
		IF  ( ipmax .ne. 0 )  THEN
		  IF  ( lavflg )  THEN
		    gpfunc = 'LAV(PRES)'
		  ELSE
		    gpfunc = 'PRES'
		  END IF
		  CALL DG_GRID  ( gdatim, glevel, gvcord, gpfunc, 
     +				pfunc,  presgrid,   kx, ky, dattim, lev, 
     +				jvcord, parm, ierdg )
		END IF
D		type *,'grid',ierdg
		IF  ( ierdg .ne. 0 )  THEN
		  CALL ER_WMSG  ( 'DG', ierdg, pfunc, ier )
		  iret = -1
		  CALL ER_WMSG  ( 'GDPVSF', iret, ' ', ier )
		  RETURN
		END IF		  
C
C*		Do the interpolation
C
d	type *,'about to do the interpolation'
		IF  ( plev .eq. RMISSD ) THEN
C
C*		First time through; initialize arrays
C
		  DO j = 1, kx*ky
C		    ogrid will contain the output grids
		    ogrid (j,1) = 0.
C		    pgrid keeps track of the latest functional (pv) value
		    pgrid (j) = 0.
C		    igrid stores the location of the surface between grid
C		     levels for interpolating other grid parameters
		    igrid (j) = 0.
C
C*		    Are we already beyond the range to interpolate?
C
		    IF  ( ( idir .eq. -1 ) .and. ( grid(j) .lt. ylev ) )
     +			THEN
			    ogrid ( j, 1 ) = RMISSD
			    igrid ( j ) = RMISSD
		    ELSE IF ( ( idir .eq. 1 ) .and. 
     +				( grid(j) .gt. ylev ) )  THEN
		            pgrid (j) = RMISSD
		    ELSE
C
C*		      Are we beneath the lowest pressure level?
C
		      IF  ( ipmax .ne. 0 )  THEN 
			IF  ( presgrid(j) .gt. pmax ) THEN
			  pgrid (j) = RMISSD
			ELSE
			  pgrid (j) = grid (j)
			END IF
		      ELSE 
C
C*		      Store current grid values
C
			    pgrid (j) = grid (j)
		      END IF
		    END IF
		  END DO
C
C*		Subsequent times through; do interpolation when necessary
C
		ELSE
		  DO j = 1, kx*ky
		    IF  ( ogrid (j,1) .eq. 0. ) THEN
C
C*		    Desired level has not yet been found
C
		      IF  ( ipmax .ne. 0 )  THEN
C
C*			Still beneath lowest pressure level?
C
			IF  ( presgrid (j) .GT. pmax )  grid (j) = RMISSD
		      END IF			     
C
C*		      Is grid function missing?
C
		      IF  ( grid (j) .eq. RMISSD ) THEN
			pgrid (j) = RMISSD
		      ELSE
C
			IF ( grid(j)*float(idir) .lt. ylev*float(idir) ) THEN
C
C*			  Have not yet reached desired level
C
			  pgrid (j) = grid (j)
C
			ELSE
			  IF ( pgrid (j) .ne. RMISSD ) THEN
C
C*			    Have reached desired level
C
			    prop = (pgrid(j)-ylev)/(pgrid(j)-grid(j))
C
C*			    Interpolate to proper level
C
			    ogrid (j,1) = plev + ( clev - plev ) * prop
C
C*			    Store interpolation information for other grids
C
			    igrid (j) = pint - float(idir) + 
     +					( prop * float(idir) )
			  END IF
			END IF
		      END IF
		    END IF
		  END DO
		END IF
	    END IF
1000	  CONTINUE
	END DO
C*
	IF (.not. gotone) then
                  iret = -1
                  CALL ER_WMSG  ( 'GDPVSF', iret, 
     +			'No functional grids could be found. ', ier )
                  RETURN
        END IF
	DO  j = 1, kx*ky
	  IF  ( ogrid (j,1) .eq. 0. )  THEN 
		ogrid (j,1) = RMISSD
		igrid (j) = RMISSD
	  END IF
	END DO
C*
C*	Now interpolate the other grids
C*
	igrids = 0
	DO  j = 1, nlist
	  gotone = .false.
	  DO  i = istrt,istop,idir
C
C*	    Encode level and compute function.
C
	    intlvl = int ( rlvl ( i ) )
	    clev = float (intlvl)
	    CALL ST_INCH ( intlvl, glevel, ier )
	    CALL DG_GRID  ( gdatim, glevel, gvcord, glist(j), 
     +				pfunc,  tgrid(1,i), kx, ky, dattim, lev, 
     +				jvcord, parm, ierdg )
	    IF ( ierdg .lt. 0 )  THEN
	      DO k = 1, kx*ky
		tgrid ( k, i ) = RMISSD
	      END DO
	    ELSE
	      gotone = .true. 
	    END IF
	  END DO
C*
	  IF ( gotone )  THEN
C
C*	    Update output grid information
C
	    igrids = igrids + 1
	    glist (igrids) = glist (j)
	    DO k = 1, kx*ky
	     IF  ( igrid ( k ) .eq. RMISSD )  THEN
C
C*	      Desired output level was not found
C
	      ogrid ( k, igrids+1 ) = RMISSD
C
	     ELSE
C
C*	      Interpolate between the proper grid levels
C
	      iflat = int ( igrid ( k ) )
	      budge = igrid ( k ) - float (iflat)
	      IF  ( tgrid ( k, iflat ) .eq. RMISSD )  THEN
		ogrid ( k, igrids+1 ) = RMISSD
	      ELSE IF  ( tgrid ( k, iflat + 1 ) .eq. RMISSD )  THEN
		ogrid ( k, igrids+1 ) = RMISSD
	      ELSE
		ogrid ( k, igrids+1 ) = tgrid ( k, iflat ) + budge *
     +				( tgrid ( k, iflat+1 ) - tgrid ( k, iflat ) )
	      END IF
	     END IF
	    END DO
	  END IF
	END DO
	RETURN
	END
