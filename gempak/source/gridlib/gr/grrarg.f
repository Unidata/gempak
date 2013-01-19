	SUBROUTINE GR_RARG  ( kxin, kyin, grid, iret )
C************************************************************************
C* GR_RARG								*
C*									*
C* This subroutine rearranges a globe wrapping grid on either a CED or	*
C* MER projection.  The rearrangement is done for any display map	*
C* projection when the grid discontinuity occurs within the map area.	*
C* The rearrangement creates a continuous grid of data over the map	*
C* area.								*
C* 									*
C* GR_RARG  ( KXIN, KYIN, GRID, IRET )					*
C*									*
C* Input parameters:							*
C*	KXIN		INTEGER		Number of input points in x dir	*
C*	KYIN		INTEGER		Number of input points in y dir	*
C*									*
C* Input and output parameters:						*
C*	GRID(KXIN*KYIN)	REAL		Grid of data			*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-21 = cannot fix wrap around	*
C**									*
C* Log:									*
C* K. Brill/NMC		03/93						*
C* T. Lee/GSC		 9/97	Fixed wrapping grid index		*
C* T. Lee/GSC		 8/00	Grid shifting for CED/MER grids		*
C* T. Lee/GSC		 8/00	Checked non-global CED grids		*
C* K. Brill/HPC		 8/02	CALL GR_SETR and GR_DORG		*
C************************************************************************
	REAL		grid (*)
C------------------------------------------------------------------------
	iret = 0
C*
	CALL GR_SETR ( kxin, kyin, ishft, iret )
	IF ( iret .eq. 0 ) THEN
	    CALL GR_DORG ( kxin, kyin, ishft, grid, iret )
	END IF
C*
	RETURN
	END
