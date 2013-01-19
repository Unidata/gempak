	SUBROUTINE GR_DORG  ( kxin, kyin, ishft, grid, iret )
C************************************************************************
C* GR_DORG								*
C*									*
C* This subroutine rearranges a globe wrapping grid on either a CED or	*
C* MER projection.							*
C* 									*
C* This rearrangement creates a continuous grid of data over the map	*
C* area.								*
C* 									*
C* CALL GR_SETR to obtain the value of ISHFT before calling GR_DORG.	*
C* 									*
C* GR_DORG  ( KXIN, KYIN, ISHFT, GRID, IRET )				*
C*									*
C* Input parameters:							*
C*	KXIN		INTEGER		Number of input points in x dir	*
C*	KYIN		INTEGER		Number of input points in y dir	*
C*	ISHFT		INTEGER		X index grid shift required	*
C*									*
C* Input and output parameters:						*
C*	GRID(KXIN*KYIN)	REAL		Grid of data			*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/HPC		08/02	Created from original GR_RARG code	*
C* K. Brill/HPC         11/11   Increase buffer size from 2048 to 8192  *
C*				NOTE:  buffer holds only a grid row     *
C************************************************************************
	REAL		grid (*)
C*
	REAL		buffer (8192)
C------------------------------------------------------------------------
	iret = 0
C*
	IF ( ishft .ne. 0 ) THEN
	    kxm1 = kxin - 1
C
C*	    Rearrange the grid so that position ISHFT becomes the
C*	    first (and last) position in every row.
C
	    indx = 0
	    DO j = 1, kyin
C
C*		Save grids to a buffer.
C
		ist = kxin * ( j - 1 )
		DO i = 1, kxm1
		    buffer (i) = grid  ( ist + i )
		END DO
C
C*		Rearrange the grid one row at a time.
C
		ixold = ishft - 1
		DO i = 1, kxm1
		    ixold = ixold + 1
		    IF ( ixold .gt. kxm1 ) ixold = 1
		    indx = indx + 1
		    grid  ( indx ) = buffer ( ixold )
		END DO
C
C*		Add a column.
C 
		indx = indx + 1
		grid  ( indx ) = buffer ( ishft )
	    END DO
	END IF
C*
	RETURN
	END
