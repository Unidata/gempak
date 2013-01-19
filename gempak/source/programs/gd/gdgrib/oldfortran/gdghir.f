        SUBROUTINE GDGHIR ( inttyp, glat, glon, np, maxx, maxy, grid,
     +			    grdo, iret )
C************************************************************************
C* GDGHIR								*
C*									*
C* This subroutine interplates the data in the input grid to the	*
C* output grid whose positions relative to the input grid are given	*
C* in GLAT and GLON as I and J positions.				*
C*									*
C* GRID and GRDO may be the same array.					*
C*									*
C* GDGHIR ( INTTYP, GLAT, GLON, NP, MAXX, MAXY, GRID, GRDO, IRET )	*
C*									*
C* Input parameters:							*
C*	INTTYP		INTEGER		Interpolation type		*
C*					  0 = linear interpolation	*
C*					  1 = nearest point assignment	*
C*	GLAT (NP)	REAL		GRID relative I positions	*
C*	GLON (NP)	REAL		GRID relative J positions	*
C*	NP		INTEGER		Number of output points		*
C*	MAXX		INTEGER		Maximum I dimension of GRID	*
C*	MAXY		INTEGER		Maximum J dimension of GRID	*
C*	GRID (MAXX,MAXY)REAL		Grid of data			*
C*									*
C* Output parameters:							*
C*	GRDO (*)	REAL		Output grid of interplated data *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-18 = invalied interp type	*
C**									*
C* Log:									*
C* K. Brill/HPC		 2/00						*
C* T. Lee/SAIC		 7/03		Changed LLMXGD -> LLMXTG	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	REAL 		grid (*), grdo (*), glat (*), glon (*)
C*
	REAL		gout (LLMXTG)
C*
	INCLUDE		'ERMISS.FNC'
	INDXQ ( lx, ly, kx ) = ( ly - 1 ) * kx + lx
C------------------------------------------------------------------------
	iret = 0
C
C*	Interpolate to output grid at valid points.
C
	IF ( inttyp .eq. 0 ) THEN
C
C*	    Bilinear interpolation.  Note:  GR_INTP is not used
C*	    because it does no check for missing coordinates.
C
	    DO ii = 1, np
		IF ( .not.
     +		  ( ERMISS ( glat (ii) ) .or. ERMISS ( glon (ii) )
     +		 .or. glat (ii) .lt. 1 .or. glon (ii) .lt. 1
     +		 .or. glat (ii) .gt. maxx .or. glon (ii) .gt. maxy ) )
     +		THEN
		    icol = glat (ii)
		    irow = glon (ii)
		    IF ( icol .eq. maxx ) icol = icol - 1
		    IF ( irow .eq. maxy ) irow = irow - 1
		    icolp1 = icol + 1
		    irowp1 = irow + 1
		    c = glat (ii) - FLOAT (icol)
		    r = glon (ii) - FLOAT (irow)
		    omc = 1. - c
		    omr = 1. - r
		    IF ( ERMISS (grid(INDXQ(icol,irow,maxx)))
     +			 .or.
     +			 ERMISS (grid(INDXQ(icolp1,irow,maxx)))
     +			 .or.
     +			 ERMISS (grid(INDXQ(icol,irowp1,maxx)))
     +			 .or.
     +			 ERMISS (grid(INDXQ(icolp1,irowp1,maxx))) )
     +		    THEN
			gout (ii) = RMISSD
		    ELSE
			gout (ii) =
     +			( grid (INDXQ(icol,irow,maxx)) * omc +
     +			  grid (INDXQ(icolp1,irow,maxx)) * c ) * omr +
     +		        ( grid (INDXQ(icol,irowp1,maxx)) * omc +
     +		          grid (INDXQ(icolp1,irowp1,maxx) ) * c ) * r
		    END IF
		ELSE
		    gout (ii) = RMISSD
		END IF
	    END DO
	ELSE IF ( inttyp .eq. 1 ) THEN
C
C*	    Nearest value interpolation (NVI).
C
	    DO ii = 1, np
		IF ( .not.
     +		  ( ERMISS ( glat (ii) ) .or. ERMISS ( glon (ii) )
     +		 .or. glat (ii) .lt. 1 .or. glon (ii) .lt. 1
     +		 .or. glat (ii) .gt. maxx .or. glon (ii) .gt. maxy ) )
     +		THEN
		    nrx = NINT ( glat (ii) )
		    nry = NINT ( glon (ii) )
		    gout (ii) = grid (INDXQ(nrx,nry,maxx))
		ELSE
		    gout (ii) = RMISSD
		END IF
	    END DO
	ELSE
	    iret = -18
	    RETURN
	END IF
	DO ii = 1, np
	    grdo (ii) = gout (ii)
	END DO
C*
	RETURN
	END
