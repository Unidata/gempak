	SUBROUTINE OA_GINT  ( ngrid, nstn, stid, data, srow, scol, 
     +			      kex, key, qcntl, grid, iextnd, sdint, 
     +			      rms, isn, nqc, stns, iret )
C************************************************************************
C* OA_GINT								*
C*									*
C* This subroutine interpolates data from a first guess grid to the	*
C* stations using a bilinear interpolation, and computes the difference	*
C* between the original data and the interpolated values.  Data are	*
C* interpolated to all stations in the extend area, but only stations	*
C* within the grid area are used to compute the RMS values.  ISN is the	*
C* number of stations used to compute the RMS value. If the difference	*
C* between the original data and the interpolated values is greater 	*
C* than certain threshold values, the stations data will be discarded	*
C*									*
C* OA_GINT ( NGRID, NSTN, STID, DATA, SROW, SCOL, KEX, KEY, QCNTL,	*
C*	     GRID, IEXTND, SDINT, RMS, ISN, NQC, STNS, IRET )		*
C*									*
C* Input parameters:							*
C*	NGRID		INTEGER		Number of grids			*
C*	NSTN		INTEGER		Number of stations		*
C*	STID (*)	CHAR*		Station ID			*
C*	SROW   (NSTN)	REAL		Station rows			*
C*	SCOL   (NSTN)	REAL		Station columns			*
C*	KEX		INTEGER		X points in extend grid		*
C*	KEY		INTEGER		Y points in extend grid		*
C*	QCNTL (NGRID)	REAL		Quality control threshold	*
C*	GRID		REAL		Grid data			*
C*	 (NGRID,KEX,KEY)						*
C*	IEXTND (4)	INTEGER		Grid area extensions		*
C*									*
C* Input and Output parameters:						*
C*	DATA		REAL		Station data			*
C*	 (NGRID,NSTN)							*
C*									*
C* Output parameters:							*
C*	SDINT		REAL		Station differences		*
C*	 (NGRID,NSTN)							*
C*	RMS   (NGRID)	REAL		RMS values			*
C*	ISN   (NGRID)	INTEGER		# of stations in grid area	*
C*	NQC   (NGRID,2)	INTEGER		# of stations eliminated by QC	*
C*	STNS		CHAR*		Eliminated Station IDs		*
C*	 (NGRID,LLSTFL*MMFILE)						*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/GSC		 3/99	Adapted from OA_SINT			*
C* T. Lee/GSC		 3/99	Output discarded stations to terminal	*
C* T. Lee/GSC		 3/99	Moved terminal output to main program	*
C* T. Lee/GSC		11/99	Set SDINT to RMISSD for bad data	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	stid(*), stns( NGRID, * )
	REAL		srow ( * ), scol ( * ), data ( NGRID, * )
	REAL		sdint ( NGRID, * ), rms ( * ), qcntl ( * )
	REAL		grid  ( NGRID, KEX, KEY )
	INTEGER		iextnd (4), isn ( * ), nqc ( NGRID, * )  
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
	    DO  j = 1, 2
		nqc  (igrid,j) = 0
	    END DO
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
			sdiff = sdata - sg
			sdint (igrid,is) = sdiff
C
C*			Add to RMS computation if within grid area and 
C*			QC threshold. Else, discard the station report 
C*			and save the station ID.
C
			IF ( (row .ge. fy1) .and. (row .le. fy2)
     +			   .and. (col .ge. fx1) .and. (col .le. fx2))
     +						THEN
			    IF ( ( ABS (sdiff) .le. qcntl (igrid) ) .or.
     +				 ( qcntl (igrid) .eq. 0. ) )  THEN
				rms (igrid) = rms (igrid) +
     +						sdint (igrid,is) **2
				isn (igrid) = isn (igrid) + 1
			      ELSE
				data  ( igrid, is ) = RMISSD
				sdint ( igrid, is ) = RMISSD
				nqc ( igrid, 1 ) = nqc (igrid, 1) + 1
				IF  ( stid (is) .ne. ' ' )  THEN
				  nqc ( igrid, 2 ) = nqc (igrid, 2) + 1
				  stns (igrid, nqc(igrid,2)) = stid (is)
				END IF
			    END IF
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
