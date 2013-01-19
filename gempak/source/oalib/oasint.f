	SUBROUTINE OA_SINT  ( ngrid, nstn, data, srow, scol, kex, key,
     +			      grid, iextnd, sdint, rms, isn, iret )
C************************************************************************
C* OA_SINT								*
C*									*
C* This subroutine interpolates data from a grid back to the stations	*
C* using a bilinear interpolation, and computes the difference		*
C* between the original data and the interpolated values.  Data are	*
C* interpolated to all stations in the extend area, but only stations	*
C* within the grid area are used to compute the RMS values.  ISN is	*
C* the number of stations used to compute the RMS value.		*
C*									*
C* OA_SINT ( NGRID, NSTN, DATA, SROW, SCOL, KEX, KEY, GRID, IEXTND,	*
C*	     SDINT, RMS, ISN, IRET )					*
C*									*
C* Input parameters:							*
C*	NGRID		INTEGER		Number of grids			*
C*	NSTN		INTEGER		Number of stations		*
C*	DATA		REAL		Station data			*
C*	 (NGRID,NSTN)							*
C*	SROW   (NSTN)	REAL		Station rows			*
C*	SCOL   (NSTN)	REAL		Station columns			*
C*	KEX		INTEGER		X points in extend grid		*
C*	KEY		INTEGER		Y points in extend grid		*
C*	GRID		REAL		Grid data			*
C*	 (NGRID,KEX,KEY)						*
C*	IEXTND (4)	INTEGER		Grid area extensions		*
C*									*
C* Output parameters:							*
C*	SDINT		REAL		Station differences		*
C*	 (NGRID,NSTN)							*
C*	RMS   (NGRID)	REAL		RMS values			*
C*	ISN   (NGRID)	INTEGER		# of stations in grid area	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	10/88	Documentation				*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	REAL		srow ( * ), scol ( * ), data ( NGRID, * )
	REAL		sdint ( NGRID, * ), rms ( * )
	REAL		grid  ( NGRID, KEX, KEY )
	INTEGER		iextnd (4), isn ( * )
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize the station difference array and the rms array.
C
	DO  igrid = 1, ngrid
	    rms (igrid) = 0.
	    isn (igrid) = 0
	    DO  is = 1, nstn
		sdint (igrid,is) = RMISSD
	    END DO
	END DO
C
C*	Compute start and end coordinates for grid area.
C
	fx1  = iextnd (1) + 1
	fx2  = kex - iextnd (3)
	fy1  = iextnd (2) + 1
	fy2  = key - iextnd (4)
	fkey = FLOAT (key)
	fkex = FLOAT (kex)
C
C*	Loop through all the stations.
C
	DO  is = 1, nstn
C
C*	    Get the row and col for the station.  Check that the station
C*	    is within the extended grid.
C
	    row = srow (is)
	    col = scol (is)
	    IF ( ( row .ge. 1. ) .and. ( row .le. fkey ) .and.
     +		 ( col .ge. 1. ) .and. ( col .le. fkex ) ) THEN
C
C*		Define values for interpolation.
C
		irow   = row
		icol   = col
		irowp1 = irow + 1
		icolp1 = icol + 1
		r      = row - FLOAT (irow)
		c      = col - FLOAT (icol)
		omr    = 1. - r
		omc    = 1. - c
		IF  ( row .eq. fkey ) THEN
		    irow   = irow - 1
		    irowp1 = irow + 1
		    r      = 1.
		    omr    = 0.
		END IF
		IF  ( col .eq. fkex ) THEN
		    icol   = icol - 1
		    icolp1 = icol + 1
		    c      = 1.
		    omc    = 0.
		END IF
C
C*		Loop through all levels and parameters.
C
		DO  igrid = 1, ngrid
C
C*		    Get data value and check that it is valid.
C*		    Check also that the surrounding grid points exist.
C
		    sdata = data ( igrid, is )
		    IF  ( ( .not. ERMISS (sdata) )  .and.
     +			  ( .not. ERMISS (grid (igrid,icol,irow))) .and.
     +			  ( .not. ERMISS (grid (igrid,icolp1,irow)))
     +						.and.
     +			  ( .not. ERMISS (grid (igrid,icol,irowp1)))
     +						.and.
     +			  ( .not. ERMISS (grid (igrid,icolp1,irowp1))) )
     +						THEN
			sg = (grid (igrid,icol,irow) * omc +
     +			      grid (igrid,icolp1,irow) * c) * omr +
     +			     (grid (igrid,icol,irowp1) * omc +
     +			      grid (igrid,icolp1,irowp1) * c) * r
			sdint (igrid,is) = sdata - sg
C
C*			Add to RMS computation if within grid area.
C
			IF ( (row .ge. fy1) .and. (row .le. fy2)
     +			   .and. (col .ge. fx1) .and. (col .le. fx2))
     +						THEN
			    rms (igrid) = rms (igrid) +
     +					  sdint (igrid,is) **2
			    isn (igrid) = isn (igrid) + 1
			END IF
		    END IF
		END DO
	    END IF
	END DO
C
C*	Compute the RMS values.
C
	DO  igrid  = 1, ngrid
	    IF  ( isn (igrid) .gt. 0 )  THEN
		rms (igrid) = SQRT (rms (igrid) / FLOAT (isn (igrid)))
	      ELSE
		rms (igrid) = RMISSD
	    END IF
	END DO
C*
	RETURN
	END
