	SUBROUTINE DP_UDIF  ( idata, kxky, nbits, p1, difmin, scale,
     +			      misflg, kx, grid, iret )
C************************************************************************
C* DP_UDIF								*
C*									*
C* This subroutine unpacks a grid in the GEMPAK DIF format.  The	*
C* value of the difference between a point and the previous point	*
C* is packed using the equations:					*
C*									*
C*	IDIF  =  NINT ( ( GDIF - DIFMIN ) / SCALE )			*
C*	GDIF  =  DIFMIN  +  IDIF * SCALE				*
C*									*
C* DP_UDIF  ( IDATA, KXKY, NBITS, P1, DIFMIN, SCALE, MISFLG, KX,	*
C*            GRID, IRET )						*
C*									*
C* Input parameters:							*
C*	IDATA (*)	INTEGER		Packed data			*
C*	KXKY		INTEGER		Total number of grid points	*
C*	NBITS		INTEGER		Number of bits			*
C*	P1		REAL		Value of first grid point	*
C*	DIFMIN		REAL		Minimum value of differences	*
C*	SCALE		REAL		Scaling factor			*
C*	MISFLG		LOGICAL		Missing data flag		*
C*	KX		INTEGER		Number of points in x dir	*
C*									*
C* Output parameters:							*
C*	GRID (KX,KY)	REAL		Grid data			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					-10 = NBITS invalid		*
C*					-11 = invalid data range	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 3/89						*
C* T. Piper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid  (KX,*)
	INTEGER		idata (*)
	LOGICAL		misflg
C*
	LOGICAL		first, line
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for valid input.
C
	IF  ( ( nbits .le. 1 ) .or. ( nbits .gt. 31 ) )  THEN
	    iret = -10
	    RETURN
	END IF
C
C*	Set missing data value and compute ky.
C
	imiss = 2 ** nbits - 1
	ky    = kxky / kx
C
C*	Loop through all the data points.
C
	first = .true.
	ibit  = 1
	iword = 1
	DO  j = 1, ky
	    line = .false.
	    DO  i = 1, kx
C
C*		Get the integer from the buffer.
C
		jshft = nbits + ibit - 33
		idat  = 0
		idat  = ISHFT ( idata (iword), jshft )
		idat  = IAND  ( idat, imiss )
C
C*		Check to see if packed integer overflows into next word.
C
		IF  ( jshft .gt. 0 )  THEN
		    jshft = jshft - 32
		    idat2 = 0
		    idat2 = ISHFT ( idata (iword+1), jshft )
		    idat  = IOR ( idat, idat2 )
		END IF
C
C*		Set location for next word.
C
		ibit = ibit + nbits
		IF  ( ibit .gt. 32 )  THEN
		    ibit  = ibit - 32
		    iword = iword + 1
		END IF
C
C*		Compute data value.
C
		IF  ( misflg .and. ( idat .eq. imiss ) )  THEN
		    grid (i,j) = RMISSD
		  ELSE
		    IF  ( first )  THEN
			grid (i,j) = p1
			psav  = p1
			plin  = p1
			line  = .true.
			first = .false.
		      ELSE
			IF  ( .not. line )  THEN
			    grid (i,j) = plin + (difmin + idat * scale)
			    line = .true.
			    plin = grid (i,j)
			  ELSE
			    grid (i,j) = psav + (difmin + idat * scale)
			END IF
			psav = grid (i,j)
		    END IF
		END IF
	    END DO
	END DO
C*
	RETURN
	END
