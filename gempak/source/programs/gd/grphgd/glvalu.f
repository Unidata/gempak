	SUBROUTINE GLVALU ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GLVALU                                                               *
C*                                                                      *
C* This subroutine performs the gradient search and gridpoint value     *
C* assignment for the graph-to-grid algorithm for gridpoints surrounded	*
C* by like-values (possibly a closed contour, but not necessarily). 	*
C*                                                                      *
C* GLVALU ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )              *
C*                                                                      *
C* Input parameters:                                                    *
C*      IMN             INTEGER         Lower I range of GRID           *
C*      IMX             INTEGER         Upper I range of GRID           *
C*      JMN             INTEGER         Lower J range of GRID           *
C*      JMX             INTEGER         Upper J range of GRID           *
C*      KX              INTEGER         Number of grid points in X      *
C*      KY              INTEGER         Number of grid points in Y      *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      HIST (KX,KY)    REAL            Grid to keep history            *
C*                                                                      *
C* Output parameters:                                                   *
C*      GRID (KX,KY)    REAL            Grid to calculate point values  *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98	Remove skip for zero gradients		*
C* D.W.Plummer/NCEP      2/99	Skip points with all zero gradients	*
C* D.W.Plummer/NCEP     12/02	Rename from GGRADS to GLVALU, add parms	*
C* D.W.Plummer/NCEP      9/03	Add minima/maxima processing		*
C************************************************************************
C
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
	REAL		grid(kx,ky), hist(kx,ky)
C
	REAL		d(8,2,2), f(8), v(8), r(8), grad(8)
	LOGICAL		proces, edge(8), found
	INTEGER		lin1s(8), lin2s(8)
C
	DATA		d/ 32*RMISSD /
C------------------------------------------------------------------------
C
C*      Loop over range of gridpoints
C
	DO  i = imn, imx
C
	  DO  j = jmn, jmx
C
C*          Only try to calculate if gridpoint unassigned.
C
	    IF ( hist(i,j) .eq. INIT )  THEN
C
	      DO  ndir = 1, 8
C
C*		The 'd' array contains value and distance information 
C*		for each direction; initialize for each gridpoint.
C
		d(ndir,1,1) = RMISSD
		d(ndir,1,2) = RMISSD
		d(ndir,2,1) = RMISSD
		d(ndir,2,2) = RMISSD
		edge(ndir) = .false.
C
C*		Search for intersections in direction ndir.
C*		Get intersection index for direction ndir.
C
		CALL GCIIJJ ( i, j, ky, ndir, ij, iret )
C
C*		Get the two closest intersections in direction ndir.
C
		CALL GINDEX ( i, j, ij, ndir, ffi1, ffj1, lin1s(ndir), 
     +			      ffi2, ffj2, lin2s(ndir), iret )
                lin1 = lin1s(ndir)
		val1 = RMISSD
		IF ( lin1 .ne. IMISSD )  THEN
		  val1 = value(lin1)
		  d(ndir,1,1) = val1
		  d(ndir,1,2) = sqrt((ffi1-i)*(ffi1-i)+(ffj1-j)*(ffj1-j))
	        END IF
                lin2 = lin2s(ndir)
		val2 = RMISSD
		IF ( lin2 .ne. IMISSD )  THEN
		  val2 = value(lin2)
		  d(ndir,2,1) = val2
		  d(ndir,2,2) = sqrt((ffi2-i)*(ffi2-i)+(ffj2-j)*(ffj2-j))
	        END IF
C
		IF ( val1 .ne. RMISSD .and. val2 .eq. RMISSD )  THEN
C
C*		    First found but not second; try to find a value
C*		    on the grid beyond the first contour.
C
		    CALL GLASTV (int(ffi1), int(ffj1), ndir, grid, hist, 
     +				 kx, ky, ffi2, ffj2, val2, iret )
C
		    IF ( val2 .ne. RMISSD )  THEN
C
C*		      Found a value beyond the first contour.
C
		      d(ndir,2,1) = val2
		      d(ndir,2,2) = sqrt( (ffi2-i)*(ffi2-i) +
     +                                    (ffj2-j)*(ffj2-j) )
		      edge(ndir) = .true.
C
		    END IF
C
		END IF
C
		IF ( d(ndir,1,1) .ne. RMISSD .and. 
     +		     d(ndir,1,2) .ne. RMISSD .and.
     +		     d(ndir,2,1) .ne. RMISSD .and.
     +		     d(ndir,2,2) .ne. RMISSD )  THEN
C
C*		    Have two values in direction ndir, compute grad(ndir)
C
		    grad(ndir) = ( d(ndir,2,1) - d(ndir,1,1) ) / 
     +			         ( d(ndir,2,2) - d(ndir,1,2) )
C
C*		    Use grad(ndir) to compute v(ndir), the gridpoint value.
C
		    v(ndir) = d(ndir,1,1) - (grad(ndir) * d(ndir,1,2))
C
C*		    Figure range limit r(ndir) (do not allow range limit to 
C*		    equal the next-valued contour), r(ndir), and 
C*		    set to v(ndir) if criteria are met.
C
		    r(ndir) = d(ndir,1,1) + 
     +			0.99 * (d(ndir,1,1) - d(ndir,2,1))
C
		    IF ( ABS(d(ndir,1,1)-v(ndir)) .gt. 
     +			 ABS(d(ndir,1,1)-r(ndir)))  THEN
			v(ndir) = r(ndir)
		    END IF
C
		ELSE
C
C*		    If any of this fails, set gridpoint value in this
C*		    direction to missing.
C
		    v(ndir) = RMISSD
C
	        END IF
C
	      END DO
C
C*	      Gridpoint values for each directional gradient are computed.
C
C*	      If gridpoint can "see" only one line number, and
C*	      gridpoint can also "see" one or more maxima/minima (mm), then
C*	      compute a linear interpolation between the closest mm and
C*	      a point opposite from the mm.
C
	      proces = .true.
	      ndir1 = 1
	      found = .false.
	      DO WHILE ( .not. found  .and.  ndir1 .le. 8 )
		IF ( lin1s(ndir1) .ne. IMISSD )  THEN
		  IF ( value(lin1s(ndir1)) .ne. RMISSD )  THEN
     		    found = .true.
		  END IF
	        END IF
		IF ( .not. found )  ndir1 = ndir1 + 1
	      END DO
	      IF ( ndir1 .gt. 8 )  proces = .false.
	      IF ( proces )  THEN
	        DO  ndir = ndir1, 8
	          IF ( ( lin1s(ndir) .ne. IMISSD .and.
     &			 value(lin1s(ndir)) .ne. RMISSD ) .and.
     &		     ( value(lin1s(ndir)) .ne. value(lin1s(ndir1)) .or.
     &		       lin1s(ndir) .ne. lin1s(ndir1) ) )  
     &			proces = .false.
	        END DO
	      END IF
C
	      IF ( proces )  THEN
C
C*	        Gridpoint can "see" only one line number, in direction ndir1
C
		tval =  RMISSD
     		dist = -RMISSD
C
     		DO  mm = 1, nmm
C
C*	          For each mm, 
C*		  compute the direction of the mm from (i,j) - idirmm.
C*	          Also compute the opposite direction - idir.
C
		  fix = i
		  fjx = j
C
C*	          d0 - distance from grid pt (i,j) to closest
C                      intersection in same direction as mm
C*	          d1 - distance from grid pt (i,j) to mm
C
     		  d1 = sqrt((i-fimm(mm))**2+(j-fjmm(mm))**2)
C
C*	          d2 - distance from grid pt (i,j) to int on line
C*	               in opposite direction from mm
C
		  d0 = -RMISSD
		  nl = 1
		  DO WHILE ( nl .le. nlines .and. d0 .gt. d1 )
 		    CALL LNSGI1 ( fix, fjx, fimm(mm), fjmm(mm),
     &			npts(nl), fi(1,nl), fj(1,nl),
     &			xint, yint, iret )
     		    IF ( xint .ne. RMISSD .and. yint .ne. RMISSD )  THEN
     		      distx = sqrt((i-xint)*(i-xint)+(j-yint)*(j-yint))
     		      d0 = MIN(d0, distx)
		    END IF
		    nl = nl + 1
     		  END DO
C
		  IF ( d1 .lt. d0 .and. d1 .lt. dist )  THEN
C
		    valopp = RMISSD
		    d2 = -RMISSD
		    DO  nl = 1, nlines
 		      CALL LNSGI1 ( fimm(mm), fjmm(mm), fix, fjx,
     &			npts(nl), fi(1,nl), fj(1,nl),
     &			xint, yint, iret )
     		      IF ( xint .ne. RMISSD .and. yint .ne. RMISSD )  THEN
     		        distx = sqrt((i-xint)*(i-xint)+(j-yint)*(j-yint))
			IF ( distx .lt. d2 )  THEN
     		          d2 = distx
		          valopp = value(nl)
		        END IF
		      END IF
     		    END DO
C
C*	            Compute linearly interpolated value only if mm 
C*		    is inside closest contour. The opposite value is from
C*		    the intersection line in direction idir. If that line
C*		    value was missing, then use the value from the one line.
C
		    IF ( valopp .eq. RMISSD ) 
     &			  valopp=value(lin1s(ndir1))
C
		    IF ( d1 .lt. dist )  THEN
		      tval = ( d2/(d1+d2) ) * valuemm(mm) +
     &			     ( d1/(d1+d2) ) * valopp
		      dist = d1
		    END IF
C
		  END IF
C
		END DO
		
     	        IF ( tval .ne. RMISSD )  THEN
C
C*	          Assign value and history.
C
		  grid(i,j) = tval
		  hist(i,j) = LIKVALMM
C
	        END IF
C
	      END IF
C
	      IF ( hist(i,j) .eq. INIT )  THEN
C
C*	          Discontinue processing if all available gradients are zero.
C
	          proces = .false.
	          ndir = 1
	          DO WHILE ( ndir .le. 8 .and. .not. proces )  
C
		    IF ( v(ndir) .ne. RMISSD )  THEN
C
		      IF ( d(ndir,1,1) .ne. d(ndir,2,1) )  proces = .true.
C
	            END IF
C
		    ndir = ndir + 1
C
	          END DO
C
	      END IF
C
C*	      Check for areas where no contours are opposite.
C*	      If none are found, weaken the gradient and assign a new value.
C
	      IF ( proces .and. hist(i,j) .eq. INIT )  THEN
C
	          ngood = 0
	          DO  ndir = 1, 8
		    IF ( .not. edge(ndir) .and. v(ndir) .ne. RMISSD )
     +			  ngood = ngood + 1
	          END DO
C
	          IF ( ngood .ge. 1 )  THEN
C
		    DO  ndir = 1, 8
		      IF ( edge(ndir) )  v(ndir) = RMISSD
		    END DO
C
	          END IF
C
	          DO  ndir = 1, 8
C
		    ndopp = ndir + 4
		    IF ( ndopp .gt. 8 )  ndopp = ndopp - 8
		    IF ( v(ndir) .ne. RMISSD .and. 
     +		       d(ndopp,1,2) .eq. RMISSD )  THEN
		      CALL GLASTV ( i, j, ndopp, grid, hist, kx, ky,
     +				  ffi, ffj, val, iret )
		      d0 = sqrt( (ffi-i)*(ffi-i) + (ffj-j)*(ffj-j) )
		      v1 = v(ndir) + d(ndir,1,1) - (( d(ndir,1,2) /
     +			(d(ndir,1,2) + d0)) * ( d(ndir,1,1)-r(ndir)))
		      v1 = v1 / 2.0
		      IF ( ABS(d(ndir,1,1)-v1     ) .lt. 
     +			 ABS(d(ndir,1,1)-v(ndir)) )  THEN
 			v(ndir) = v1
		      END IF
C
		    END IF
C
	          END DO
C
C*	          Compute a weighted average of all valid directional gradients.
C
	          f(1) = 0.0
	          f(2) = 0.0
	          n = 0
	          DO  ndir = 1, 8
C
		    IF ( v(ndir) .ne. RMISSD )  THEN
		      f(1) = f(1) + ( v(ndir) / d(ndir,1,2) )
		      f(2) = f(2) + (     1.0 / d(ndir,1,2) )
		      n = n + 1
		    END IF
C
	          END DO
C
C*	          Assign value to gridpoint if at least one direction was used.
C
	          IF ( n .gt. 0 )  THEN
		    grid(i,j) = f(1) / f(2)
		    hist(i,j) = LIKVAL
	          END IF
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
	END
