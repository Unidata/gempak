	SUBROUTINE GR_SUBX  ( kxin, kyin, grid, iminx, iminy, imaxx,
     +			      imaxy, iskpxy, misflg, kxsub, kysub,
     +			      subgrd, ioffx, ioffy, iskip, iret )
C************************************************************************
C* GR_SUBX								*
C*									*
C* This subroutine subsets a grid.					*
C*									*
C* GR_SUBX ( KXIN, KYIN, GRID, IMINX, IMINY, IMAXX, IMAXY, ISKPXY,	*
C*	     MISFLG, KXSUB, KYSUB, SUBGRD, IOFFX, IOFFY, ISKIP, IRET )	*
C*									*
C* Input parameters:							*
C*	KXIN		INTEGER		Number of input points in x dir	*
C*	KYIN		INTEGER		Number of input points in y dir	*
C*	GRID 		REAL		Input grid			*
C*	 (KXIN,KYIN)							*
C*	IMINX		INTEGER		Lower left corner of subgrid	*
C*	IMINY		INTEGER		Lower left corner of subgrid	*
C*	IMAXX		INTEGER		Upper right corner of subgrid	*
C*	IMAXY		INTEGER		Upper right corner of subgrid	*
C*	ISKPXY		INTEGER		X and Y skip factor		*
C*	MISFLG		LOGICAL		Missing data interpolation flag	*
C*									*
C* Output parameters:							*
C*	KXSUB		INTEGER		Number of output points in x dir*
C*	KYSUB		INTEGER		Number of output points in y dir*
C*	SUBGRD		REAL		Subset grid			*
C*	 (KXSUB,KYSUB)							*
C*	IOFFX		INTEGER		Offset to first grid point	*
C*	IOFFY		INTEGER		Offset to first grid point	*
C*	ISKIP		INTEGER		Skip factor used		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -9 = invalid grid subset	*
C**									*
C* Log:									*
C* M. desJardins/NMC	11/91						*
C* S. Jacobs/EAI	 3/93	Fixed typo ky --> kyin			*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C* T. Piper/GSC		 3/99	Corected prolog				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		grid ( kxin, kyin ), subgrd (*)
	LOGICAL		misflg
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Check values for subgrid.
C
	ix1 = iminx
	iy1 = iminy
	ix2 = imaxx
	iy2 = imaxy
	IF  ( ix1 .le. 0 )  ix1 = 1
	IF  ( iy1 .le. 0 )  iy1 = 1
	IF  ( ( ix2 .eq. 0 ) .or. ( ix2 .gt. kxin ) ) ix2 = kxin
	IF  ( ( iy2 .eq. 0 ) .or. ( iy2 .gt. kyin ) ) iy2 = kyin
C
C*	Test for valid subgrid specifications.
C
	nx = ix2 - ix1 + 1
	ny = iy2 - iy1 + 1
C
C*	Check skip factor.  If negative or too large, set to 0.
C
	nxc = nx / 2
	nyc = ny / 2
	IF  ( ( iskpxy .ge. 0 ) .and. ( iskpxy .lt. nxc ) .and.
     +	      ( iskpxy .lt. nyc ) )  THEN
	    iskip = iskpxy
	  ELSE
	    iskip = 0
	END IF
	iskip1 = iskip + 1
C
C*	Compute number of subgrid points.
C
	kxsub = ( nx - 1 ) / iskip1 + 1
	kysub = ( ny - 1 ) / iskip1 + 1
	IF  ( ( kxsub .lt. 2 ) .or. ( kysub .lt. 2 ) )  THEN
	    iret = -9
	    RETURN
	END IF
C
C*	Compute offset to first point.
C
	ioffx = ix1 - 1
	ioffy = iy1 - 1
C
C*	Move data. Interpolate if necessary.
C
	jj = 0
	DO  j = iy1, iy2, iskip1
	  jj = jj + 1
	  ii = 0
	  DO  i = ix1, ix2, iskip1
	    ii = ii + 1
	    IF ( ( .not. misflg ) .or.
     +		 ( .not. ERMISS ( grid (i,j ) ) ) )  THEN
	      subgrd ( (jj-1) * kxsub + ii ) = grid (i,j)
	    ELSE IF ( (i .gt. 1) .and. (i .lt. kxin) .and.
     +		      (.not. ERMISS ( grid (i-1,j) ) ) .and.
     +		      (.not. ERMISS ( grid (i+1,j) ) ) )  THEN
	      subgrd ( (jj-1) * kxsub + ii ) =
     +		      0.5 * ( grid (i+1,j) + grid (i-1,j) )
	    ELSE IF ( (j .gt. 1) .and. (j .lt. kyin) .and.
     +		      (.not. ERMISS (grid (i,j-1) ) ) .and.
     +		      (.not. ERMISS (grid (i,j+1) ) ) )  THEN
	      subgrd ( (jj-1) * kxsub + ii ) =
     +		       0.5 * ( grid (i,j+1) + grid (i,j-1) )
	    ELSE IF ( (i .gt. 2) .and.
     +                (.not. ERMISS (grid (i-2, j) ) ) .and.
     +		      (.not. ERMISS (grid (i-1, j) ) ) )  THEN
	      subgrd ( (jj-1) * kxsub + ii ) =
     +		       2.0 * grid (i-1,j) - grid (i-2,j)
	    ELSE IF ( (i .lt. kxin - 1 ) .and.
     +		     (.not. ERMISS (grid (i+1, j) ) ) .and.
     +		     (.not. ERMISS (grid (i+2, j) ) ) ) THEN
	      subgrd ( (jj-1) * kxsub + ii ) =
     +		       2.0 * grid (i+1,j) - grid (i+2,j)
	    ELSE IF ( (j .gt. 2) .and.
     +		      (.not. ERMISS (grid (i,j-2) ) ) .and.
     +		      (.not. ERMISS (grid (i,j-1) ) ) ) THEN
	      subgrd ( (jj-1) * kxsub + ii ) =
     +		       2.0 * grid (i,j-1) - grid (i,j-2)
	    ELSE IF ( (j .lt. kyin - 1) .and.
     +		      (.not. ERMISS (grid (i,j+2) ) ) .and.
     +		      (.not. ERMISS (grid (i,j+1) ) ) ) THEN
	      subgrd ( (jj-1) * kxsub + ii ) =
     +		       2.0 * grid (i,j+1) - grid (i,j+2)
	    ELSE
	      subgrd ( (jj-1) * kxsub + ii ) = RMISSD
	  END IF
	  END DO
	END DO
C*
	RETURN
	END
