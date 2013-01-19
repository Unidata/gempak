	SUBROUTINE POLY_CCWA ( kx, ky, ng, nc, iret )
C************************************************************************
C* POLYT_CCWA 								*
C*									*
C* This subroutine creates final polygon grids.  It transverses from	*
C* the lowest quarter and then turn counter-clockwise.			*
C*									*
C* POLY_CCWA ( KX, KY, NG, NC, IRET )					*
C*									*
C* Input parameters:							*
C*	KX		INTEGER		X-direction grid for a polygon	*
C*	KY		INTEGER		Y-direction grid for a polygon	*
C*	NG		INTEGER		Nth group			*
C*	NC		INTEGER		Nth warning category		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C**									*
C* Log:									*
C* T. Lee/SAIC		03/08						*
C* T. Lee/SAIC		06/08	Added points if dx or dy > 1		*
C************************************************************************
	INCLUDE		'gpolyg.cmn'
	LOGICAL		new
C-----------------------------------------------------------------------
	iret = 0
C
C*	Transverse the southernmost points along each column.
C
	npts = 1
	ist = isnake ( 1, ng, nc )
	jst = jsnake ( 1, ng, nc )
	skx = kx * skipct / 100.
	sky = ky * skipct / 100.
C
	DO ii = ist + 1, ist + kx
	    nexti = ii
	    nextj = igyd + 1
	    DO np = 1, numpts ( ng, nc )
		IF ( polygi ( np, ng, nc ) .eq. nexti )  THEN 
		    IF ( polygj ( np, ng, nc ) .lt. nextj )  THEN
			nextj = polygj ( np, ng, nc )
		    END IF
		END IF
	    END DO
C
	    IF ( nextj .ne. ( igyd + 1 ) )  THEN
		oldi = isnake ( npts, ng, nc )
		oldj = jsnake ( npts, ng, nc )
		idx = nexti - oldi
		idy = nextj - oldj
C
		IF ( ABS ( idy ) .gt. 1 )  THEN
		    IF ( idy .gt. 0 )  THEN
			DO kk = oldj + 1, nextj - 1
			    newi = 1
			    newj = kk
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( (jpp .eq. kk) .and. (ipp .gt. newi)
     +				      .and. (ipp .le. nexti) ) THEN
				    newi = ipp
				END IF
			    END DO
C
			    IF ( newi .ne. 1 )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		      ELSE IF ( idy .lt. 0 )  THEN
			DO kk = oldj - 1, nextj + 1, -1 
			    newi = igxd
			    newj = kk
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( (jpp .eq. kk) .and. (ipp .lt. newi)
     +				      .and. (ipp .ge. nexti) ) THEN
				    newi = ipp
				END IF
			    END DO
C
			    IF ( newi .ne. igxd )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		    END IF
		END IF
C
		CALL POLY_EXIST ( nc, ng, npts, nexti, nextj, new, ier ) 
		IF ( new )  THEN
		    npts = npts + 1
		    isnake ( npts, ng, nc ) = nexti
		    jsnake ( npts, ng, nc ) = nextj
		END IF
	    END IF
	END DO
C
C*	Transverse the easternmost points along each row. Get the 
C*	relative max J value along the extreme x-axis, instead of using
C*	"JST+KY".  Use that value to transverse on the eastern side.  By
C*	doing this, it will give northern side a better depiction of the
C*	polygon.
C		

	mst = imargn ( 2, ng, nc )
	kst = jmargn ( 1, ng, nc )
	DO np = 1, numpts ( ng, nc )
	    newi = polygi ( np, ng, nc )
	    newj = polygj ( np, ng, nc )
	    IF ( (newi .eq. mst) .and. (newj .gt. kst) )  THEN
		kst = newj
	    END IF
	END DO
C
	ist = isnake ( npts, ng, nc )
	jst = jsnake ( npts, ng, nc )
	DO jj = jst + 1, kst
C
C*	    Transverse the easternmost points along each row.
C
	    nextj = jj
	    nexti = 0
	    DO np = 1, numpts ( ng, nc )
		IF ( polygj ( np, ng, nc ) .eq. nextj )  THEN 
		    IF ( polygi ( np, ng, nc ) .gt. nexti )  THEN
			nexti = polygi ( np, ng, nc )
		    END IF
		END IF
	    END DO
C
	    IF ( nexti .ne. 0 )  THEN
		oldi = isnake ( npts, ng, nc )
		oldj = jsnake ( npts, ng, nc )
		idx = nexti - oldi
		idy = nextj - oldj
C
		IF ( ABS ( idx ) .gt. 1 )  THEN
		    IF ( idx .gt. 0 )  THEN
			DO kk = oldi + 1, nexti - 1
			    newi = kk
			    newj = igyd
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( (ipp .eq. kk) .and. (jpp .lt. newj)
     +				      .and. (jpp .ge. nextj) ) THEN
				    newj = jpp
				END IF
			    END DO
C
			    IF ( newj .ne. igyd )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		      ELSE IF ( idx .lt. 0 )  THEN
			DO kk = oldi - 1, nexti - 1, -1 
			    newi = kk
			    newj = 1
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( (ipp .eq. kk) .and. (jpp .gt. newj)
     +				      .and. (jpp .le. nextj) ) THEN
				    newj = jpp
				END IF
			    END DO
C
			    IF ( newj .ne. 1 )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		    END IF
		END IF
C
		CALL POLY_EXIST ( nc, ng, npts, nexti, nextj, new, ier ) 
		IF ( new )  THEN
		    npts = npts + 1
		    isnake ( npts, ng, nc ) = nexti
		    jsnake ( npts, ng, nc ) = nextj
		END IF
	    END IF
	END DO
C
C*	Transverse the northernmost points along each column.
C
	ist = isnake ( npts, ng, nc )
	jst = jsnake ( npts, ng, nc )
	DO ii = ist - 1, ist - kx, -1
	    nexti = ii
	    nextj = 0
	    DO np = 1, numpts ( ng, nc )
		IF ( polygi ( np, ng, nc ) .eq. nexti )  THEN 
		    IF ( polygj ( np, ng, nc ) .gt. nextj )  THEN
			nextj = polygj ( np, ng, nc )
		    END IF
		END IF
	    END DO
C
	    IF ( nextj .ne. 0 )  THEN
		oldi = isnake ( npts, ng, nc )
		oldj = jsnake ( npts, ng, nc )
		idx = nexti - oldi
		idy = nextj - oldj
C
		IF ( ABS ( idy ) .gt. 1 )  THEN
		    IF ( idy .gt. 0 )  THEN
			DO kk = oldj + 1, nextj - 1
			    newi = 1
			    newj = kk
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( (jpp .eq. kk) .and. (ipp .gt. newi)
     +				      .and. (ipp .le.oldi) ) THEN
				    newi = ipp
				END IF
			    END DO
C
			    IF ( newi .ne. 1 )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		      ELSE IF ( idy .lt. 0 )  THEN
			DO kk = oldj - 1, nextj + 1, -1 
			    newi = igxd
			    newj = kk
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( (jpp .eq. kk) .and. (ipp .lt. newi)
     +				      .and. (ipp .ge. nexti) ) THEN
				    newi = ipp
				END IF
			    END DO
C
			    IF ( newi .ne. igxd )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		    END IF
		END IF
C
		CALL POLY_EXIST ( nc, ng, npts, nexti, nextj, new, ier ) 
		IF ( new )  THEN
		    npts = npts + 1
		    isnake ( npts, ng, nc ) = nexti
		    jsnake ( npts, ng, nc ) = nextj
		END IF
	    END IF
	END DO
C
C*	Transverse the westernmost points along each row.
C		
	ist = isnake ( npts, ng, nc )
	jst = jsnake ( npts, ng, nc )
	mpoint ( ng, nc ) = npts
	DO jj = jst - 1, jsnake ( 1, ng, nc ),  -1
C
	    nextj = jj
	    nexti = igxd
	    DO np = 1, numpts ( ng, nc )
		IF ( polygj ( np, ng, nc ) .eq. nextj )  THEN 
		    IF ( polygi ( np, ng, nc ) .lt. nexti )  THEN
			nexti = polygi ( np, ng, nc )
		    END IF
		END IF
	    END DO
C
	    IF ( nexti .ne. igxd )  THEN
		oldi = isnake ( npts, ng, nc )
		oldj = jsnake ( npts, ng, nc )
		idx = nexti - oldi
		idy = nextj - oldj
C
		IF ( ABS ( idx ) .gt. 1 )  THEN
		    IF ( idx .gt. 0 )  THEN
			DO kk = oldi + 1, nexti - 1
			    newi = kk
			    newj = igyd
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( ( ipp .eq. kk ) .and.
     +				     ( jpp .lt. newj ) .and.
     +				     ( jpp .ge. nextj ) .and.
     +				     ( jpp .lt. oldj ) )  THEN
				    newj = jpp
				END IF
			    END DO
C
			    IF ( newj .ne. igyd )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		      ELSE IF ( idx .lt. 0 )  THEN
			DO kk = oldi - 1, nexti - 1, -1 
			    newi = kk
			    newj = 1
C
			    DO np = 1, numpts ( ng, nc )
				ipp = polygi ( np, ng, nc )
				jpp = polygj ( np, ng, nc )
				IF ( (ipp .eq. kk) .and. (jpp .gt. newj)
     +				      .and. (jpp .le. nextj) ) THEN
				    newj = jpp
				END IF
			    END DO
C
			    IF ( newj .ne. 1 )  THEN
				CALL POLY_EXIST ( nc, ng, npts, 
     +					          newi, newj, new, ier ) 
			      ELSE
				new = .false.
			    END IF
C
			    IF ( ABS ( oldi - newi ) .gt. skx .or.
     +				 ABS ( oldj - newj ) .gt. sky )
     +				new = .false.
C
			    IF ( new )  THEN
				npts = npts + 1
				isnake ( npts, ng, nc ) = newi
				jsnake ( npts, ng, nc ) = newj
			    END IF
			END DO
		    END IF
		END IF
C
		CALL POLY_EXIST ( nc, ng, npts, nexti, nextj, new, ier ) 
		IF ( new )  THEN
		    npts = npts + 1
		    isnake ( npts, ng, nc ) = nexti
		    jsnake ( npts, ng, nc ) = nextj
		END IF
	    END IF
C
	END DO
C
	IF ( isnake ( npts, ng, nc ) .eq. isnake ( 1, ng, nc ) .and.
     +	     jsnake ( npts, ng, nc ) .eq. jsnake ( 1, ng, nc ) )  THEN
	    mpoint ( ng, nc ) = npts
	  ELSE
	    npts = npts + 1
	    mpoint ( ng, nc ) = npts
	    isnake ( npts, ng, nc ) = isnake ( 1, ng, nc )
	    jsnake ( npts, ng, nc ) = jsnake ( 1, ng, nc )
	END IF
C*
	RETURN
	END
