	SUBROUTINE GR_ACOL  ( kxin, kyin, grid, kxout, kyout, iret )
C************************************************************************
C* GR_ACOL								*
C*									*
C* This subroutine adds a column of data to a grid.			*
C*									*
C* This is mainly used for adding a column to grids on cylindrical	*
C* projections. The data should be a global grid which does not		*
C* completely wrap around the earth, but is only one grid point short.	*
C* This routine will then repeat the first column of data as the last	*
C* column in order to achieve a complete wrap.				*
C* 									*
C* GR_ACOL  ( KXIN, KYIN, GRID, KXOUT, KYOUT, IRET )			*
C*									*
C* Input parameters:							*
C*	KXIN		INTEGER		Number of input points in x dir	*
C*	KYIN		INTEGER		Number of input points in y dir	*
C*									*
C* Input and output parameters:						*
C*	GRID (KX+1,KY)	REAL		Grid of data			*
C*									*
C* Output parameters:							*
C*	KXOUT		INTEGER		Number of output points in x dir*
C*	KYOUT		INTEGER		Number of output points in y dir*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* K. Brill/NMC		03/93						*
C* S. Jacobs/EAI	11/93		Adapted from GR_RARG		*
C************************************************************************
	REAL		grid (*)
C------------------------------------------------------------------------
	iret = 0
C
C*	Add the extra column.
C
C*	Run the loops from the last point to the first point,
C*	in order to add a column of data.
C
	DO  jj = kyin, 1, -1
	    DO  ii = kxin+1, 1, -1
		IF  ( ii .eq. kxin+1 )  THEN
		    grid((jj-1)*(kxin+1)+ii) = grid((jj-1)*kxin+1)
		ELSE
		    grid((jj-1)*(kxin+1)+ii) = grid((jj-1)*kxin+ii)
		END IF
	    END DO
	END DO
C*
	kxout = kxin + 1
	kyout = kyin
C*
	RETURN
	END
