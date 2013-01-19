	SUBROUTINE OAGAGN  ( gltln, extend, deltax, deltay, datflg, 
     +			     grltln, eltln, iextnd, kx, ky, dltln, 
     +			     iret )
C************************************************************************
C* OAGAGN								*
C*									*
C* This subroutine aligns the grid and extended areas on grid points.	*
C*									*
C* OAGAGN  ( GLTLN, EXTEND, DELTAX, DELTAY, DATFLG, GRLTLN, ELTLN,	*
C*           IEXTND, KX, KY, DLTLN, IRET )				*
C*									*
C* Input parameters:							*
C*	GLTLN (4)	REAL		Input grid area			*
C*	EXTEND		CHAR*		Input extend			*
C*	DELTAX		REAL		X grid spacing			*
C*	DELTAY		REAL		Y grid spacing			*
C*	DATFLG		LOGICAL		Flag to compute data area	*
C*									*
C* Output parameters:							*
C*	GRLTLN (4)	REAL		Actual grid area		*
C*	ELTLN (4)	REAL		Extended grid area		*
C*	IEXTND (4)	INTEGER		Extend grid numbers		*
C*	KX		INTEGER		Number of points in x		*
C*	KY		INTEGER		Number of points in y		*
C*	DLTLN (4)	REAL		Data area			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -8 = invalid DELTAX/DELTAY	*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/NMC          9/90   Fix for 0-360 lon range			*
C************************************************************************
	CHARACTER*(*)	extend
	REAL		gltln (4), grltln (4), eltln (4), dltln (4)
	INTEGER		iextnd (4)
	LOGICAL		datflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for valid deltax and deltay.
C
	IF  ( ( deltax .eq. 0. ) .or. ( deltay .eq. 0. ) )  THEN
	    iret = -8
	    CALL ER_WMSG  ( 'OAGRID', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Convert extend to integers.  Use a default of 2 if any numbers are
C*	missing.
C
	CALL ST_ILST  ( extend, ';', 2, 4, iextnd, n, ier )
	DO  i = 1, 4
	    IF  ( iextnd (i) .lt. 0 )  iextnd (i) = 2
	END DO
C
C*	Compute the number of grid points in the x direction.
C*	Correct the northeast grid corner to lie on a grid line.
C*	Compute the longitude corners of the extended area.
C
	itst = IFIX ( gltln (4) - gltln (2) )
	nlon = IFIX ( ( gltln (4) - gltln (2) ) /deltax )
	IF ( itst .ne. 360 )  THEN
            kx   = nlon + 1
	ELSE
	    kx   = nlon
	END IF
	grltln (2) = gltln (2)
	grltln (4) = grltln (2) + nlon * deltax
	IF ( itst .eq. 360 ) THEN
	    iextnd (1) = 0
	    iextnd (3) = 0
	END IF
	eltln  (2) = grltln (2) - iextnd (1) * deltax
	eltln  (4) = grltln (4) + iextnd (3) * deltax
C
C*	Do the same computations for the latitude.
C
	nlat = IFIX ( ( gltln (3) - gltln (1) ) / deltay )
	ky   = nlat + 1
	grltln (1) = gltln (1)
	grltln (3) = grltln (1) + nlat * deltay
	eltln  (1) = grltln (1) - iextnd (2) * deltay
	eltln  (3) = grltln (3) + iextnd (4) * deltay
	IF ( eltln (1) .lt. -90. ) eltln (1) = -90.
        IF ( eltln (3) .gt.  90. ) eltln (3) =  90.
C
C*	IF data area was not input by the user, use extended area.
C
	IF  ( .not. datflg )  THEN
	    DO  i = 1, 4
		dltln (i) = eltln (i)
	    END DO
	END IF
C*
	RETURN
	END
