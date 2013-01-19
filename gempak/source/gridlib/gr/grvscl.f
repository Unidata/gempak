	SUBROUTINE GR_VSCL  ( iscale, kx, ky, imin, jmin, imax, jmax,
     +			grid, grid2, rmin, rmax, rmin2, rmax2, iret )
C************************************************************************
C* GR_VSCL								*
C*									*
C* This subroutine scales a vector grid.  If the scaling factor    	*
C* is missing, undefined or greater than 20 in absolute value, 		*
C* an appropriate scaling factor will be computed.  If the data  	*
C* are too small to be scaled with ISCALE = 20, ISCALE is set 		*
C* to IMISSD and IRET = -14.  The maximun and minimun values for   	*
C* both grids are also returned.					*
C*									*
C* GR_VSCL  ( ISCALE, KX, KY, IMIN, JMIN, IMAX, JMAX, GRID, GRID2,	*
C*           RMIN,  RMAX, RMIN2, RMAX2, IRET )				*
C*									*
C* Input parameters:							*
C*	ISCALE		INTEGER		Input scale factor		*
C*	KX		INTEGER		Number of grid points in x dir	*
C*	KY		INTEGER		Number of grid points in y dir	*
C*	IMIN		INTEGER		Minimum x grid point		*
C*	JMIN		INTEGER		Minimum y grid point		*
C*	IMAX		INTEGER		Maximum x grid point		*
C*	JMAX		INTEGER		Maximum y grid point		*
C*									*
C* Input and output parameters:						*
C*	GRID (KX,KY)	REAL		First grid to scale		*
C*	GRID2 (KX,KY)   REAL		Second grid to scale		*
C*									*
C* Output parameters:							*
C*	RMIN		REAL		Data minimum for first grid	*
C*	RMAX		REAL		Data maximum for first grid	*
C*	RMIN2		REAL		Data minimum for second grid	*
C*	RMAX2		REAL		Data maximum for second grid 	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = no data in range		*
C*					 -9 = invalid subset range	*
C*					-14 = scaling not possible	*
C**									*
C* M. desJardins/GSFC	 6/88						*
C* G. Huffman/GSC	 1/89	Warn for SCALE in [-100,-20],[20,100]	*
C* M. desJardins/GSFC	 7/89	Scale larger to get integral intervals	*
C* K. Brill/GSC          5/90   Return missing scale for small grd val  *
C* K. Brill/GSC          5/90   Change SSCALE + 10. to * 10. & 5 to 9.01*
C* K. Brill/NMC		08/91	Allow for scaling when rmax = rmin	*
C* L. Sager/NMC		 8/93	GR_VSCL created from GR_SCAL		*
C* P. Bruehl/Unidata     9/94   Cleaned up (as per MdJ), scale < abs(20)*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
C*
	REAL		grid (*), grid2 (*)
C*
	LOGICAL		set
C*
        INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Get range of data.
C
	CALL GR_STAT  ( grid, kx, ky, imin, jmin, imax, jmax,
     +			rmin, rmax, ravg, rdev, iret )
	IF  ( iret .ne. 0 )  THEN
	    iscale = 0
	    RETURN
	END IF
	CALL GR_STAT  ( grid2, kx, ky, imin, jmin, imax, jmax,
     +			rmin2, rmax2, ravg2, rdev2, iret )
	IF  ( iret .ne. 0 )  THEN
	    iscale = 0
	    RETURN
	END IF
	
C
C*	If value is not within -20 to 20 range, or if it is missing,
C*	compute a new value.
C
	set = .false.
	IF  ( ( iscale .eq. IMISSD ) .or. ( iscale .lt. -20 ) .or.
     +	      ( iscale .gt.  20 ) )  THEN
C
C*	    Compute the logarithm for the absolute value.
C
	    set   = .true.
	    sscale = 1.0
	    savg  = ( ABS ( rmin ) + ABS ( rmax ) ) / 2.
	    scale = 0.
	    IF  ( savg .ne. 0. )  THEN
		rlog = ALOG10  ( ABS  ( savg ) )
		IF  ( rlog .lt. 0. )  THEN
		    irlog = -rlog
		    IF  ( FLOAT (-irlog) .ne. rlog )  irlog = irlog + 1
		    scale = irlog
		END IF
	    END IF
C
C*	    Set integer scaling term.
C
	    iscale = NINT ( scale )
	END IF
C
C*	Make sure scaling is valid
C
	IF  ( iscale .lt. -20 )  THEN
		iscale = -20
	ELSE IF  ( iscale .gt. 20  ) THEN
		iscale = 20
	END IF
C
C*	Scale data if necessary
C
	IF ( iscale .ne. 0 ) THEN
	    sscale = 10. ** iscale
C
C*	    Scale rmax and rmin to new values.
C
	    rmin = rmin * sscale
	    rmax = rmax * sscale
	    rmin2 = rmin2 * sscale
	    rmax2 = rmax2 * sscale
	END IF
C
C*	Check difference between min and max to ensure integral
C*	intervals.
C
	rdif = rmax - rmin
	IF  ( set .and. ( rdif .lt. 9.01 ) .and. ( rdif .ne. 0 ) ) THEN
	    iscale = iscale + 1
	    sscale = sscale * 10.
	    rmin   = rmin * 10.
	    rmax   = rmax * 10.
	    rmin2  = rmin2* 10.
	    rmax2  = rmax2* 10.
	END IF
C
C*	Scale the data.
C
	IF  ( iscale .ne. 0 )  THEN
	    DO  i = 1, kx*ky
		IF  ( .not. ERMISS ( grid (i) ) )
     +                 grid (i) = grid (i) * sscale
		IF  ( .not. ERMISS ( grid2 (i) ) )
     +                 grid2 (i) = grid2 (i) * sscale
	    END DO
	END IF
C*
	RETURN
	END
