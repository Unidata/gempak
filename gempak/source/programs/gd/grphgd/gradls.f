	SUBROUTINE GRADLS ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GRADLS                                                               *
C*                                                                      *
C* This subroutine performs the radial search and gridpoint value	*
C* assignment for the graph-to-grid algorithm.				*
C*                                                                      *
C* GRADLS ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )		*
C*                                                                      *
C* Input parameters:                                                    *
C*      IMN		INTEGER		Lower I range of GRID		*
C*      IMX		INTEGER		Upper I range of GRID		*
C*      JMN		INTEGER		Lower J range of GRID		*
C*      JMX		INTEGER		Upper J range of GRID		*
C*      KX		INTEGER		Number of grid points in X	*
C*      KY		INTEGER		Number of grid points in Y	*
C*                                                                      *
C* Input and Output parameters:                                         *
C*      HIST (KX,KY)	REAL		Grid to keep history		*
C*                                                                      *
C* Output parameters:                                                   *
C*      GRID (KX,KY)	REAL		Grid to calculate point values	*
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98                                           *
C* W.D.Plummer/NCEP     12/02	Add history parameters			*
C* D.W.Plummer/NCEP     09/03	Pass line number back from GINDEX	*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
	REAL		grid(kx,ky), hist(kx,ky)
C
	REAL		d1(8,2), d2(8,2)
	LOGICAL		hit
C*
	NUMBIG = ( kx + ky ) * ( kx + ky )
C
C*	Loop over range of gridpoints
C
	DO  j = jmn, jmx
C
	  DO  i = imn, imx
C
C*	    Only try to calculate if gridpoint unassigned.
C
	    IF ( hist(i,j) .eq. INIT )  THEN
C
	      DO  ndir = 1, 8
C
		d1(ndir,1) = RMISSD
		d1(ndir,2) = RMISSD
		d2(ndir,1) = RMISSD
		d2(ndir,2) = RMISSD
C
C*	        For each direction, get closest intersections
C
		CALL GCIIJJ ( i, j, ky, ndir, ij, iret )
C
		CALL GINDEX ( i, j, ij, ndir, fi1, fj1, lin1, 
     +			      fi2, fj2, lin2, iret )
C
     		val1 = RMISSD
     		IF ( lin1 .ne. IMISSD )  THEN
C
C*	            For closest intersection,
C*	            compute distance & save value.
C
     		    val1 = value(lin1)
		    d1(ndir,1) = (fi1-i)*(fi1-i) + (fj1-j)*(fj1-j) 
		    d1(ndir,2) = val1
		END IF
C
     		val2 = RMISSD
     		IF ( value(lin2) .ne. RMISSD )  THEN
C
C*	            For second closest intersection, 
C*		    compute distance & save value.
C
     		    val2 = value(lin2)
		    d2(ndir,1) = (fi2-i)*(fi2-i) + (fj2-j)*(fj2-j) 
		    d2(ndir,2) = val2
		END IF
C
	      END DO
C
C*	      Sort directional arrays to find closest intersection
C
	      CALL GSORTD ( d1, 8, 2, 8, iret )
	      CALL GSORTD ( d2, 8, 2, 8, iret )
C
C*	      Check to see if two intersections exist with differing values.
C*	      Ignore intersections with lines with value RMISSD.
C
      	      hit = .false.
	      ndir1 = 1
	      DO WHILE ( d1(ndir1,2) .eq. RMISSD  .and. ndir1 .le. 8 ) 
	          ndir1 = ndir1 + 1
	      END DO
      	      DO ndir = ndir1, 8
C
                IF ( ( d1(ndir,2) .ne. RMISSD ) .and. 
     &		     ( d1(ndir,2) .ne. d1(ndir1,2) ) .and.
     &		     .not. hit )  THEN
		    ndir2 = ndir
                    hit = .true.
                END IF
C
	      END DO

              IF ( hit ) THEN
C
C*		Found two contours with differing values;
C*		Scan all lines with each value for closer vertices
C*		and assign the distance to the d array.
C
		dist1 = d1(ndir1,1)
		dist2 = d1(ndir2,1)
		DO  nl = 1, nlines
C
		    IF ( value(nl) .eq. d1(ndir1,2) )  THEN
C
 			DO  np = 1, npts(nl)
C
 			    dist = (fi(np,nl)-i) * (fi(np,nl)-i) + 
     +				   (fj(np,nl)-j) * (fj(np,nl)-j)
 			    IF ( dist .lt. dist1 )  THEN
 			        d1(ndir1,1) = dist
 			        dist1 = dist
 			    END IF
C
 			END DO
C
		    ELSE IF ( value(nl) .eq. d1(ndir2,2) )  THEN
C
 			DO  np = 1, npts(nl)
C
 			    dist = (fi(np,nl)-i) * (fi(np,nl)-i) + 
     +				   (fj(np,nl)-j) * (fj(np,nl)-j)
 			    IF ( dist .lt. dist2 )  THEN
 				d1(ndir2,1) = dist
 				dist2 = dist
 			    END IF
C
 			END DO
C
		    END IF
C
		END DO
C
		d1(ndir1,1) = sqrt ( d1(ndir1,1) )
		d1(ndir2,1) = sqrt ( d1(ndir2,1) )
C
                v1 = ( d1(ndir1,2) / d1(ndir1,1) ) + 
     +		     ( d1(ndir2,2) / d1(ndir2,1) )
                v2 = ( 1.0 / d1(ndir1,1) ) + ( 1.0 / d1(ndir2,1) )
                grid(i,j) = v1 / v2
                hist(i,j) = RADIAL
C
              END IF
C
	    END IF
C
	  END DO
C
	END DO
C
	RETURN
C
	END
