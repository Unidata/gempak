        SUBROUTINE GDTDTAF  ( glevel, gvcord, gfunc,
     +                       gpoint, grid, npts,   timfnd, rgx,
     +                       rgy,    rlat,   rlon,   x,      y,
     +                       parm,   level,  ivcord, iret )
C************************************************************************
C* GDTDTAF								*
C*									*
C* This subroutine gets the data to plot for a time series.		*
C*									*
C* GDTDTAF  ( GLEVEL, GVCORD, GFUNC, GPOINT, GRID, NPTS, TIMFND,         *
C*           RGX,    RGY,    RLAT,  RLON,   X,      Y,    PARM,         *
C*           LEVEL,  IVCORD, IRET )           				*
C*									*
C* Input parameters:							*
C*	GLEVEL		CHAR*		User input level		*
C*	GVCORD		CHAR*		User input vert coord		*
C*	GFUNC		CHAR*		User input function		*
C*	GPOINT		CHAR*		User input point to plot	*
C*									*
C* Work parameters:							*
C*	GRID(*)		REAL		Work array for reading grid	*
C*									*
C* Output parameters:							*
C*	NPTS		INTEGER		Number of points		*
C*      TIMFND (NPTS)   CHAR*           Time values                     *
C*	RGX		REAL		X grid coordinate		*
C*	RGY		REAL		Y grid coordinate		*
C*	RLAT		REAL		Latitude			*
C*	RLON		REAL		Longitude			*
C*	X      (NPTS)	REAL		X coordinates			*
C*	Y      (NPTS)	REAL		Y coordinates			*
C*	PARM		CHAR*		Parameter name			*
C*	LEVEL  (2)	INTEGER		Level for search		*
C*	IVCORD		INTEGER		Vertical coordinate for search	*
C*	IRET		INTEGER		Return code			*
C*					 +1 = points missing for ... 	*
C*					  0 = normal return		*
C*					 -4 = invalid grid point	*
C*					 -9 = no valid points for ...	*
C**									*
C* Log:									*
C* G. Huffman/GSC	 2/89	Adapted from GDPDTA			*
C* G. Huffman/USRA	 5/89	Time range parsing moved to GDTTIM	*
C* K. Brill/GSC         12/89   Added call to DG_AREA			*
C* S. Schotz/GSC	 6/90	Removed respnd flag			*
C* R. Tian/SAIC		10/02	Added call to DG_CXGP			*
C* K. Brill/HPC		12/02	Pass blank IJSKIP int DG_CXGP		*
C* R. Tian/SAIC         10/04   Changes for time/file management        *
C* S. Jacobs/NCEP	 5/06	Changed error -10 to warning +1		*
C* S. Gilbert/NCEP	 8/07	From GDTDTA; added work array arg	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)   glevel, gvcord, gfunc, gpoint, timfnd (*), parm
	REAL		x (*), y (*)
	INTEGER		level (2)
C*
	CHARACTER	dattim (2)*20, pfunc*80
	CHARACTER	time (2)*20, dualtm*36
	REAL		grid ( * )
	LOGICAL		misspt, gottm
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Loop through times finding data.  MISSPT flags uncomputables.
C
	npts = 0
        gottm = .true.
	misspt = .false.
        DO WHILE ( gottm )
	    yy = RMISSD
            CALL DG_NTIM ( .true., .true., time, gottm, ier )
            IF ( ier .eq. 0 .and. gottm ) THEN
C
C*	        Find plotting location.
C
	        CALL DG_CXGP  ( gpoint, ' ', 1, np, rgx, rgy,
     +                          rlat, rlon, iret )
	        IF  ( iret .ne. 0 ) RETURN
C
	        CALL TG_DUAL ( time, dualtm, ier )
	        CALL DG_GRIDN ( dualtm, glevel, gvcord, gfunc,
     +			        pfunc,  grid,   kx, ky, dattim,
     +			        level,  ivcord, parm,   ier )
	        IF  ( ier .eq. 0 )  THEN
C
C*		    Check that grid includes point to be found and
C*		    interpolate to the point.
C
		    rkx = FLOAT ( kx )
		    rky = FLOAT ( ky )
		    IF  ( ( rgx .gt. rkx ) .or. ( rgy .gt. rky ) )
     +              THEN
		        iret = -4
		        RETURN
		    END IF
C
		    CALL GR_INTP  ( 1, rgx, rgy, 1, kx, ky, grid, 
     +		                    yy, ier )
C
	          ELSE
		    misspt = .true.
	        END IF
C
C*	        For non-missing data convert the time found to the valid 
C*	        time, and set the X coordinate as the minutes after the 
C*	        first (good) time.  YY is still RMISSD if it didn't get 
C*	        a value above.
C
	        IF  ( .not. ERMISS ( yy ) )  THEN
		    npts = npts + 1
                    timfnd ( npts ) = dualtm
                    CALL TG_VALD  ( timfnd (npts), timfnd (npts), ier )
                    CALL TI_DIFF  ( timfnd (npts), timfnd (1), 
     +		                    minx, ier )
	            x (npts) = FLOAT ( minx )
	            y (npts) = yy
	          ELSE
		    misspt = .true.
	        END IF
	    END IF
	END DO
C
C*	If there are missing points, whack the time out of PFUNC (if 
C*	it's there) and use the PARM|LEVEL|GVCORD in error messages.
C
	IF  ( misspt )  THEN
	    ihat  = INDEX ( pfunc,           '^' )
	    inext = INDEX ( pfunc ( ihat: ), ' ' ) + 1
	    IF  ( ihat .ne. 0 )  pfunc = pfunc ( 1:ihat ) // 
     +			                 pfunc ( inext: )
	    IF  ( npts .eq. 0 )  THEN
	        iret = -9
	      ELSE
	        iret = 1
	    END IF
	    CALL ER_WMSG  ( 'GDTSER', iret, pfunc, ier )
	END IF
C*
	RETURN
	END
