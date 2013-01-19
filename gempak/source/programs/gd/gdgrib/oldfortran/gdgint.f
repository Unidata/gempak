        SUBROUTINE GDGINT ( grid, igx, igy, rnvblk, cpyfil,
     +			    proj, gdarea, kxky,
     +                      grdo, nxo, nyo, rnavo, igpds, navchg, iret )
C************************************************************************
C* GDGINT								*
C*									*
C*									*
C* This subroutine interplates the data in the input grid to the	*
C* output grid whose navigation is given by input in CPYFIL (grid # or	*
C* reference GEMPAK file) or explicitly by PROJ, GDAREA, and KXKY.	*
C*									*
C* The user input for PROJ follows the standard GEMPAK form:		*
C*									*
C*	name/angle1;angle2;angle3 (degrees)				*
C*									*
C* The input for GDAREA (GRDAREA) is rlat1;rlon1;rlat2;rlon2 (degrees). *
C* The input for KXKY is kx;ky.						*
C* 									*
C* PROJ, GRDAREA, and KXKY are used to build RNAVO.  Then apply the	*
C* following procedure to LLMXGD output grid points at a time:		*
C*									*
C*   1-  The new grid navigation is set in GPLT using GR_SNAV.		*
C*   2-  The lat/lons of the new grid points are computed using GTRANS.	*
C*   3-  The old navigation is reset in GPLT by calling GR_SNAV.	* 
C*   4-  The lat/lons computed in 3 are transformed to (I,J) relative	*
C*       to the old grid using GTRANS.					*
C*   5-  The old grid values are interpolated to the new grid using	*
C*       GR_INTP.							*
C*									*
C* IGPDS is 255 unless it is specified in CPYFIL, following the # sign.	*
C*									*
C* GDGINT ( GRID, IGX, IGY, RNVBLK, CPYFIL, PROJ, GDAREA, KXKY,		*
C*	    GRDO, NXO, NYO, RNAVO, IGPDS, NAVCHG, IRET )		*
C*									*
C* Input parameters:							*
C*	GRID (IGX,IGY)	REAL		Grid of data			*
C*	IGX		INTEGER		Grid X dimension		*
C*	IGY		INTEGER		Grid Y dimension		*
C*	RNVBLK (LLNNAV) REAL		Navigation block of GRID	*
C*	CPYFIL		CHAR*		Output grid # or ref file name	*
C*	PROJ		CHAR*		Output grid projection		*
C*	GDAREA		CHAR*		Output grid area		*
C*	KXKY		CHAR*		Output grid x,y dimensions	*
C*									*
C* Output parameters:							*
C*	GRDO (NXO,NYO)	REAL		Output grid of interplated data *
C*	NXO		INTEGER		# of output grid pts in x	*
C*	NYO		INTEGER		# of output grid pts in y	*
C*	RNAVO (LLNNAV)	REAL		Navigation block for output grid*
C*	IGPDS		INTEGER		Grid number for PDS from CPYFIL *
C*	NAVCHG		LOGICAL		Flag for navigation change	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -7 = interpolation error	*
C*					-14 = (i,j) -> lat/lon failed	*
C*					-15 = lat/lon -> (i,j) failed	*
C*					-16 = cannot set output grid nav*
C*					-17 = cannot set input grid nav *
C**									*
C* Log:									*
C* K. Brill/HPC		 9/99						*
C* K. Brill/HPC		 2/00	CALL GDGHIR instead of GR_INTP		*
C* K. Brill/HPC          2/00   CALL GDGCPY; write interp message	*
C* K. Brill/HPC		 3/00	Add NAVCHG flag				*
C* K. Brill/HPC		 5/00	Write success message only once		*
C* T. Lee/SAIC		 7/03	Changed LLMXTD -> LLMXTG		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	CHARACTER*(*)	cpyfil, proj, gdarea, kxky
	REAL 		grid (*), grdo (*), rnavo (*), rnvblk (*)
	LOGICAL		navchg
C*
	REAL		glat (LLMXTG), glon (LLMXTG)
	CHARACTER*4	cprj
	LOGICAL		navchk
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
	igpds = 255
	navchg = .false.
C*
	nxny = igx * igy
	IF ( proj .eq. ' ' .and. cpyfil .eq. ' ' ) THEN
	    DO i = 1, LLNNAV
		rnavo (i) = rnvblk (i)
	    END DO
	    navchk = .true.
	ELSE
C
C*	    Build the navigation block for the output grid.
C
	    CALL GDGCPY ( cpyfil, cprj, nxo, nyo, rnavo, igpds, iret )
	    IF ( iret .ne. 0 ) THEN
		CALL GDGNAV ( proj, kxky, gdarea, cprj, nxo, nyo,
     +			  rnavo, iret )
		igpds = 255
		IF ( iret .ne. 0 ) RETURN
	    END IF
	    CALL GR_CNAV ( rnvblk, rnavo, 16, navchk, ier )
	END IF
	IF ( navchk ) THEN
	    DO i = 1, nxny
		grdo (i) = grid (i)
	    END DO
	    nxo = igx
	    nyo = igy
	ELSE
	    nout = nxo * nyo
	    knt = 0
	    istrt = 1
	    istop = LLMXTG
	    DO WHILE ( knt .lt. nout )
		istop = MIN ( istop, nout )
C
C*		Compute (i,j) of each output grid point.
C
		np = 0
		DO ii = istrt, istop, 1
		    knt = knt + 1
		    i = MOD ( ii, nxo )
		    j = ii / nxo + 1
		    IF ( i .eq. 0 ) THEN
			i = nxo
			j = j - 1
		    END IF
		    np = np + 1
		    glat (np) = FLOAT (i)
		    glon (np) = FLOAT (j)
		END DO
C
C*		Set navigation of output grid in GPLT.
C
		CALL GR_SNAV ( 16, rnavo, ier )
		IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'GR', ier, ' ', ier2 )
		    iret = -16
		    RETURN
		END IF
C
C*		Transform output grid coords to lat/lon.
C
		CALL GTRANS ( 'G', 'M', np, glat, glon, glat, glon,
     +			      ier )
		IF ( ier .ne. 0 ) THEN
		    iret = -14
		    RETURN
		END IF
C
C*		Set navigation of input grid in GPLT.
C
		CALL GR_SNAV ( 16, rnvblk, ier )
		IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'GR', ier, ' ', ier2 )
		    iret = -17
		    RETURN
		END IF
C
C*		Transform lat/lon to input grid coords.
C
		CALL GTRANS ( 'M', 'G', np, glat, glon, glat, glon,
     +			      ier )
		IF ( ier .ne. 0 ) THEN
		    iret = -15
		    RETURN
		END IF
C
C*		Interpolate to output grid.
C
		CALL GDGHIR ( 0, glat, glon, np, igx, igy, grid,
     +			       grdo (istrt), ier )
		IF ( ier .ne. 0 ) THEN
		    CALL ER_WMSG ( 'GR', ier, ' ', ier2 )
		    iret = -7
		    RETURN
		ELSE
C
C*		    Check for navigation change.
C
		    navchg = rnavo (2) .ne. rnvblk (2)
		    navchg = navchg .or.
     +			   ( ABS ( rnavo(11) - rnvblk(11) ) .gt. .005 )
		    navchg = navchg .or.
     +			   ( ABS ( rnavo(12) - rnvblk(12) ) .gt. .005 )
		    navchg = navchg .or.
     +			   ( ABS ( rnavo(13) - rnvblk(13) ) .gt. .005 )
		END IF
C*
		istrt = istop + 1
		istop = istop + LLMXTG
	    END DO
	    WRITE (6,*) ' ** Successful Horizontal Interpolation ** '
	END IF
C*
	RETURN
	END
