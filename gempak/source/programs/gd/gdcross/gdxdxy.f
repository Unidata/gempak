	SUBROUTINE GDXDXY ( grddx, grddy, iret )
C************************************************************************
C* GDXDXY								*
C*									*
C* This subroutine gets the grid increments along x and y directions	*
C* for CED grids.  For all other grids the increments in x and y are	*
C* assumed to be the same and are set to 1.				*
C*									*
C* GDXDXY ( GRDDX, GRDDY, IRET )					*
C*									*
C* Output parameters:							*
C*      GRDDX		REAL		Delta x				*
C*      GRDDY		REAL		Delta y				*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. F. Brill/GSC       7/89  						*
C* S. Jacobs/NMC	 1/95		Fix GRDDX for globe-wrapping grd*
C* T. Lee/SAIC		 3/02		Increased PRJC character size	*
C************************************************************************
	CHARACTER 	prjc*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Call GEMPLT routine to get lat-lon bounds and projection.
C
	CALL GQGPRJ ( prjc, a1, a2, a3, kx, ky, rltll, rlnll,
     +		      rltur, rlnur, iret )
C*
	IF ( iret .ne. 0 ) RETURN
C*
	IF ( prjc .eq. 'CED' .or. prjc .eq. 'MCD' ) THEN
	  rdiff = rlnur - rlnll
	  IF  ( rdiff .eq. 0. )  rdiff = 360.
	  grddx = ABS ( rdiff / FLOAT ( kx - 1 ) )
	  grddy = ABS ( ( rltur - rltll ) / FLOAT ( ky - 1 ) )
	ELSE
          grddx = 1.
	  grddy = 1.
	END IF
C*
	RETURN
	END
