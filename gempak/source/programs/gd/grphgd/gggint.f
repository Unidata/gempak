	SUBROUTINE GGGINT ( grid, hist, kx, ky, iret )
C************************************************************************
C* GGGINT                                                               *
C*                                                                      *
C* This subroutine performs the radial search and gridpoint value       *
C* assignment for the graph-to-grid algorithm.                          *
C*                                                                      *
C* GGGINT ( GRID, HIST, KX, KY, IRET )					*
C*                                                                      *
C* Input parameters:                                                    *
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
C* D.W.Plummer/NCEP      9/98                                           *
C* D.W.Plummer/NCEP      9/98	Re-do range check for intersections	*
C* D.W.Plummer/NCEP      2/99	Throw out intersections outside grid	*
C* D.W.Plummer/NCEP     10/99	Comment out diagnostic prints		*
C* D.W.Plummer/NCEP      9/01	Add iret check for LNSGIN calls		*
C* W.D.Plummer/NCEP	12/02	Chgs for minmax, dev coords, new parms	*
C* W.D.Plummer/NCEP	07/03	Chgs for intersection efficiency	*
C* W.D.Plummer/NCEP     09/03   Chgs for wrap-around bounds             *
C* D.W.Plummer/NCEP	01/04	Calling seq chg to CGR_RANGE		*
C* D.W.Plummer/NCEP	03/05	Error checking for max int exceedance 	*
C* D.W.Plummer/NCEP	09/05	Add 'extend' param to GGAPSM call seq	*
C* D.W.Plummer/NCEP	03/07	Access contours beyond grid domain	*
C* D.W.Plummer/NCEP	05/07	Correct xin2,yin2 vals from prev deliv	*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ERROR.PRM'
	INCLUDE		'grphgd.cmn'
C
	REAL		grid(kx,ky), hist(kx,ky)
C
	REAL            xin2(2), yin2(2)
	REAL            fiz(MAXPPL), fjz(MAXPPL)
C
	REAL		xints(MAXINT), yints(MAXINT)
	REAL		fi_D(LLMXPT), fj_D(LLMXPT)
	REAL		fi_Dx(LLMXPT), fj_Dx(LLMXPT)
C
	CHARACTER	sys_D*2, sys_G*2, sys_I*2
	LOGICAL		qpoly, extend
C
	INTEGER		X_MN, X_MX, Y_MN, Y_MX
	LOGICAL		ONGRID
C
	SCALE ( x ) = ( NINT( x * 1000.0 ) / 1000.0 )
C------------------------------------------------------------------------
	iret = NORMAL
C
	sys_D = 'D'
	CALL ST_NULL ( sys_D, sys_D, lens, ier )
	sys_G = 'G'
	CALL ST_NULL ( sys_G, sys_G, lens, ier )
	sys_I = 'I'
	CALL ST_NULL ( sys_I, sys_I, lens, ier )
C
C*	Only create line extensions if BND option not in effect.
C
	extend = ( bnds .eq. ' ' )
C
C*	Initialize the intersection arrays
C
	DO  ii = 1, MAXINT
	    intsct(ii,INT_X) = RMISSD
	    intsct(ii,INT_Y) = RMISSD
	    intinfo(ii,INT_DRCT) = IMISSD
	    intinfo(ii,INT_INDX) = IMISSD
	    intinfo(ii,INT_LINE) = IMISSD
	END DO
	DO  j = 1, 4
	    DO  i = 1, MAXDIM
		intptrs(i,j,NINTS) = 0
		intptrs(i,j,STPTR) = IMISSD
	    END DO
	END DO
	ntotint = 0
C
C*	Process relative minima and maxima; convert (lat,lon) to (fi,fj)
C
	DO  n = 1, nmm
C
	    CALL GTRANS ( 'M', 'G', 1, flatmm(n), flonmm(n), 
     +			  fimm(n), fjmm(n), iret )
	    fimm(n) = SCALE( fimm(n) )
	    fjmm(n) = SCALE( fjmm(n) )
C
	END DO
C
	qpoly = .true.
C
	X_MN = 1
	X_MX = kx
	Y_MN = 1
	Y_MX = ky
C    
	IF ( visib ) THEN
	  DO  nl = 1, nlines
	    CALL GTRANS ( 'M', 'G', npts(nl), flat(1,nl),
     +	  		  flon(1,nl), fiz, fjz, iret )
	    DO  n = 1, npts(nl)
	        X_MN = MIN ( X_MN, INT(fiz(n)) )
	        X_MX = MAX ( X_MX, INT(fiz(n)) )
	        Y_MN = MIN ( Y_MN, INT(fjz(n)) )
	        Y_MX = MAX ( Y_MX, INT(fjz(n)) )
	    END DO
	  END DO
	  X_MN = X_MN - 2
	  X_MX = X_MX + 2
	  Y_MN = Y_MN - 2
	  Y_MX = Y_MX + 2
	END IF

C
C*      Process contour lines; convert (lat,lon) pairs to (fi,fj)
C
	DO  nl = 1, nlines
C
	  CALL GTRANS ( 'M', 'D', npts(nl), flat(1,nl),
     +			flon(1,nl), fi_D, fj_D, iret )
C
          IF ( ismth(nl).eq.IMISSD) ismth(nl) = 0
	  ism = ismth(nl)
	  CALL GGAPSM ( fi_D, fj_D, npts(nl), kx, ky, ism,
     +			closed(nl), extend, fi_Dx, fj_Dx, np, iret )
	  CALL GTRANS ( 'D', 'G', np, fi_Dx, fj_Dx, fiz, fjz, iret )
	  npts(nl) = np
C
C*	  Adjust line values to 2 decimal places (this allows
C*	  LNSGIN to perform its calculations more definitively); 
C*	  also calculate range of line in G (grid) coordinates.
C
	  CALL CGR_RANGE ( sys_D, np, fi_Dx, fj_Dx, qpoly, sys_G,
     &		iwrap, nout, fiz, fjz, 
     &		rngi(nl,1), rngj(nl,1), rngi(nl,2), rngj(nl,2), ier )
     	  npts(nl) = nout
	  DO  np = 1, npts(nl)
	    fi(np,nl) = SCALE( fiz(np) )
	    fj(np,nl) = SCALE( fjz(np) )
	  END DO
          IF ( iwrap .gt. 0 )  THEN
	      xii = 1.0
	      yii = 1.0
	    CALL GTRANS ( 'G', 'I', 1, xii, yii, xi, yi, ier)
     	    xip = xi + TWOPI
	    CALL GTRANS ( 'I', 'G', 1, xip, yi, xg, yg, ier)
	    dx = xg - 1.0
	  END IF
C
          DO  iw = 0, iwrap
C
C*	    First, intersect with columns
C
	    intdim(1) = kx 
C
	    yin2(1) = 1
	    yin2(2) = ky
	    DO  ix = 1, kx 
C
		xin2(1) = ix + iw*dx
		xin2(2) = ix + iw*dx
C
		IF ( rngj(nl,2) .lt. yin2(1) .or. 
     +		     rngj(nl,1) .gt. yin2(2) .or.
     +		     rngi(nl,1) .gt. xin2(1) .or. 
     +		     rngi(nl,2) .lt. xin2(1) )  THEN
 		ELSE
C
		    CALL LNSGIN ( xin2, yin2, fi(1,nl), 
     +				fj(1,nl), npts(nl), MAXINT, nmint, 
     +				xints, yints, iret )
		    CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
C
		    IF ( nmint .gt. 0 )  THEN
C
		        DO  nmi = 1, nmint
C
                            iy = yints(nmi)
			    IF ( ix .ge. X_MN .and. ix .le. X_MX .and.
     +				 iy .ge. Y_MN .and. iy .le. Y_MX )  THEN
			     IF ( ntotint+1 .le. MAXINT )  THEN
			      ntotint = ntotint + 1
			      intsct(ntotint,INT_X) = xints(nmi)-iw*dx
			      intsct(ntotint,INT_Y) = yints(nmi)
			      intinfo(ntotint,INT_DRCT) = 1
			      intinfo(ntotint,INT_INDX) = ix
			      intinfo(ntotint,INT_LINE) = nl
C
                              IF ( ONGRID(ix,iy,kx,ky) .and.
     +				   iy .eq. yints(nmi) .and.
     +                             hist(ix,iy) .eq. INIT )  THEN
                                hist(ix,iy) = EXACT
                                grid(ix,iy) = value(nl)
                              END IF
                             ELSE
			      CALL ER_WMSG ( 'GRPHGD', +3, 'Columns', ier )
                             END IF
                            END IF

			END DO
C    
		    END IF
C
		END IF
C
	    END DO
C
C*	    Second, intersect with rows
C
	    intdim(2) = ky 
C
	    xin2(1) = 1 + iw*dx
	    xin2(2) = kx + iw*dx
	    DO  iy = 1, ky 
C
	        yin2(1) = iy
	        yin2(2) = iy
C
		IF ( rngj(nl,2) .lt. yin2(1) .or. 
     +		     rngj(nl,1) .gt. yin2(1) .or.
     +		     rngi(nl,1) .gt. xin2(2) .or. 
     +		     rngi(nl,2) .lt. xin2(1) )  THEN
		ELSE
C
		    CALL LNSGIN ( xin2, yin2, fi(1,nl), 
     +				  fj(1,nl), npts(nl), MAXINT, nmint, 
     +				  xints, yints, iret )
		    CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
C
		    IF ( nmint .gt. 0 )  THEN
C
			DO  nmi = 1, nmint
C
                            ix = xints(nmi)-iw*dx
			    IF ( ix .ge. X_MN .and. ix .le. X_MX .and.
     +				 iy .ge. Y_MN .and. iy .le. Y_MX )  THEN
			     IF ( ntotint+1 .le. MAXINT )  THEN
			      ntotint = ntotint + 1
			      intsct(ntotint,INT_X) = xints(nmi)-iw*dx
			      intsct(ntotint,INT_Y) = yints(nmi)
			      intinfo(ntotint,INT_DRCT) = 2
			      intinfo(ntotint,INT_INDX) = iy
			      intinfo(ntotint,INT_LINE) = nl
C
                              IF ( ONGRID(ix,iy,kx,ky) .and. 
     +				   ix .eq. xints(nmi) .and.
     +                             hist(ix,iy) .eq. INIT )  THEN
                                hist(ix,iy) = EXACT
                                grid(ix,iy) = value(nl)
                              END IF
                             ELSE
			      CALL ER_WMSG ( 'GRPHGD', +3, 'Rows', ier )
                             END IF
                            END IF
C
			END DO
C    
		    END IF
C
		END IF
C
	    END DO
C
C*	    Third, intersect with positive diagonal
C
	    i3 = 0
C
	    xin2(1) = 0 + iw*dx
	    yin2(2) = ky + 1
C
	    DO  iy = ky, 1, -1
C
		i3 = i3 + 1
		yin2(1) = iy - 1
		xin2(2) = ky - iy + 2 + iw*dx
C
		IF ( rngj(nl,2) .lt. yin2(1) .or. 
     +		     rngj(nl,1) .gt. yin2(2) .or.
     +		     rngi(nl,1) .gt. xin2(2) .or. 
     +		     rngi(nl,2) .lt. xin2(1) )  THEN
		ELSE
C
		    CALL LNSGIN ( xin2, yin2, fi(1,nl), 
     +				  fj(1,nl), npts(nl), MAXINT, nmint, 
     +				  xints, yints, iret )
		    CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
C
		    IF ( nmint .gt. 0 )  THEN
C
		        DO  nmi = 1, nmint
C
			    ixx = xints(nmi)-iw*dx
			    iyy = yints(nmi)
			    IF ( ixx .ge. 1 .and. ixx .le. kx .and.
     +				 iyy .ge. 1 .and. iyy .le. ky )  THEN
			     IF ( ntotint+1 .le. MAXINT )  THEN
			      ntotint = ntotint + 1
			      intsct(ntotint,INT_X) = xints(nmi)-iw*dx
			      intsct(ntotint,INT_Y) = yints(nmi)
			      intinfo(ntotint,INT_DRCT) = 3
			      intinfo(ntotint,INT_INDX) = i3
			      intinfo(ntotint,INT_LINE) = nl
C
			      IF ( ONGRID(ixx,iyy,kx,ky) .and. 
     +				   ixx .eq. xints(nmi) .and. 
     +				   iyy .eq. yints(nmi) .and.
     +				   hist(ixx,iyy) .eq. INIT )  THEN
				hist(ixx,iyy) = EXACT
				grid(ixx,iyy) = value(nl)
			      END IF
                             ELSE
			      CALL ER_WMSG ( 'GRPHGD', +3, 'Pos Diags (1)', ier )
                             END IF
			    END IF
C
		        END DO
C    
		    END IF
C
	        END IF
C
	    END DO
C
	    yin2(1) = 0
	    xin2(2) = kx + 1 + iw*dx
C
	    DO  ix = 2, kx 
C
	        i3 = i3 + 1
	        xin2(1) = ix - 1 + iw*dx
	        yin2(2) = kx - ix + 2
C
		IF ( rngj(nl,2) .lt. yin2(1) .or. 
     +		     rngj(nl,1) .gt. yin2(2) .or.
     +		     rngi(nl,1) .gt. xin2(2) .or. 
     +		     rngi(nl,2) .lt. xin2(1) )  THEN
		ELSE
C
		    CALL LNSGIN ( xin2, yin2, fi(1,nl), 
     +				  fj(1,nl), npts(nl), MAXINT, nmint, 
     +				  xints, yints, iret )
		    CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
C
		    IF ( nmint .gt. 0 )  THEN
C
			DO  nmi = 1, nmint
C
			    ixx = xints(nmi)-iw*dx
			    iyy = yints(nmi)
			    IF ( ixx .ge. 1 .and. ixx .le. kx .and.
     +				 iyy .ge. 1 .and. iyy .le. ky )  THEN
			     IF ( ntotint+1 .le. MAXINT )  THEN
			      ntotint = ntotint + 1
			      intsct(ntotint,INT_X) = xints(nmi)-iw*dx
			      intsct(ntotint,INT_Y) = yints(nmi)
			      intinfo(ntotint,INT_DRCT) = 3
			      intinfo(ntotint,INT_INDX) = i3
			      intinfo(ntotint,INT_LINE) = nl
C
			      IF ( ONGRID(ixx,iyy,kx,ky) .and. 
     +  			   ixx .eq. xints(nmi) .and. 
     +				   iyy .eq. yints(nmi) .and.
     +				   hist(ixx,iyy) .eq. INIT )  THEN
				hist(ixx,iyy) = EXACT
				grid(ixx,iyy) = value(nl)
			      END IF
                             ELSE
			      CALL ER_WMSG ( 'GRPHGD', +3, 'Pos Diags (2)', ier )
                             END IF
			    END IF
C
			END DO
C    
		    END IF
C
		END IF
C
	    END DO
C
	    intdim(3) = i3
C
C*	    Fourth, intersect with negative diagonal
C
	    i4 = 0
C
	    yin2(1) = 0
	    xin2(2) = 0 + iw*dx
C
	    DO  ix = 1, kx 
C
		i4 = i4 + 1
		xin2(1) = ix + 1 + iw*dx
		yin2(2) = ix + 1
C
		IF ( rngj(nl,2) .lt. yin2(1) .or. 
     +		     rngj(nl,1) .gt. yin2(2) .or.
     +		     rngi(nl,1) .gt. xin2(1) .or. 
     +		     rngi(nl,2) .lt. xin2(2) )  THEN
		ELSE
C
		    CALL LNSGIN ( xin2, yin2, fi(1,nl), 
     +				  fj(1,nl), npts(nl), MAXINT, nmint, 
     +				  xints, yints, iret )
		    CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
C
		    IF ( nmint .gt. 0 )  THEN
C
			DO  nmi = 1, nmint
C
			    ixx = xints(nmi)-iw*dx
			    iyy = yints(nmi)
			    IF ( ixx .ge. 1 .and. ixx .le. kx .and.
     +				 iyy .ge. 1 .and. iyy .le. ky )  THEN
			     IF ( ntotint+1 .le. MAXINT )  THEN
			      ntotint = ntotint + 1
			      intsct(ntotint,INT_X) = xints(nmi)-iw*dx
			      intsct(ntotint,INT_Y) = yints(nmi)
			      intinfo(ntotint,INT_DRCT) = 4
			      intinfo(ntotint,INT_INDX) = i4
			      intinfo(ntotint,INT_LINE) = nl
C
			      IF ( ONGRID(ixx,iyy,kx,ky) .and. 
     + 				   ixx .eq. xints(nmi) .and. 
     +				   iyy .eq. yints(nmi) .and.
     +				   hist(ixx,iyy) .eq. INIT )  THEN
				hist(ixx,iyy) = EXACT
				grid(ixx,iyy) = value(nl)
			      END IF
                             ELSE
			      CALL ER_WMSG ( 'GRPHGD', +3, 'Neg Diags (1)', ier )
                             END IF
			    END IF
C
			END DO
C    
		    END IF
C
		END IF
C
	    END DO
C
	    xin2(1) = kx + 1 + iw*dx
	    yin2(2) = ky + 1
C
	    DO  iy = 2, ky 
C
		i4 = i4 + 1
		yin2(1) = iy - 1
		xin2(2) = kx - ky + iy - 1 + iw*dx
C
		IF ( rngj(nl,2) .lt. yin2(1) .or. 
     +		     rngj(nl,1) .gt. yin2(2) .or.
     +		     rngi(nl,1) .gt. xin2(1) .or. 
     +		     rngi(nl,2) .lt. xin2(2) )  THEN
		ELSE
C
		    CALL LNSGIN ( xin2, yin2, fi(1,nl), 
     +				  fj(1,nl), npts(nl), MAXINT, nmint, 
     +				  xints, yints, iret )
		    CALL ER_WMSG ( 'GRPHGD', iret, ' ', ier )
C
		    IF ( nmint .gt. 0 )  THEN
C
			DO  nmi = 1, nmint
C
			    ixx = xints(nmi)-iw*dx
			    iyy = yints(nmi)
			    IF ( ixx .ge. 1 .and. ixx .le. kx .and.
     +				 iyy .ge. 1 .and. iyy .le. ky )  THEN
			     IF ( ntotint+1 .le. MAXINT )  THEN
			      ntotint = ntotint + 1
			      intsct(ntotint,INT_X) = xints(nmi)-iw*dx
			      intsct(ntotint,INT_Y) = yints(nmi)
			      intinfo(ntotint,INT_DRCT) = 4
			      intinfo(ntotint,INT_INDX) = i4
			      intinfo(ntotint,INT_LINE) = nl
C
			      IF ( ONGRID(ixx,iyy,kx,ky) .and.
     + 				   ixx .eq. xints(nmi) .and. 
     +				   iyy .eq. yints(nmi) .and.
     +				   hist(ixx,iyy) .eq. INIT )  THEN
				hist(ixx,iyy) = EXACT
				grid(ixx,iyy) = value(nl)
			      END IF
                             ELSE
			      CALL ER_WMSG ( 'GRPHGD', +3, 'Neg Diags (1)', ier )
                             END IF
			    END IF
C
			END DO
C    
		    END IF
C
	        END IF
C
	    END DO
C
	    intdim(4) = i4
C
	  END DO
C
	END DO
C
	CALL GGSORT ( ntotint, intsct(1,INT_X), intsct(1,INT_Y),
     &                intinfo(1,1), intinfo(1,2), intinfo(1,3), ier )
        ndup = ier
C
C*	Fill in the intptrs array which contains the number of ints per
C*	row/col/diagonal as well as where they start in the intinfo array.
C
	nptr = 1
     	DO  ndir = 1, 4
	    DO  ndim = 1, intdim(ndir)
C
	        IF ( intinfo(nptr,INT_DRCT) .eq. ndir .and. 
     &	             intinfo(nptr,INT_INDX) .eq. ndim )  THEN
	            intptrs(ndim,ndir,STPTR) = nptr
	            DO WHILE ( intinfo(nptr,INT_DRCT) .eq. ndir .and.
     &                         intinfo(nptr,INT_INDX) .eq. ndim )
	            nptr = nptr + 1
	            intptrs(ndim,ndir,NINTS) = 
     &			intptrs(ndim,ndir,NINTS) + 1
	            END DO
	        END IF
	    END DO
	END DO
C
	RETURN
	END
C
	LOGICAL FUNCTION ONGRID ( ix, iy, kx, ky )
	ONGRID = .false.
	IF ( ix .ge. 1 .and. ix .le. kx .and.
     +	     iy .ge. 1 .and. iy .le. ky )  THEN
	     ONGRID = .true.
	END IF
	RETURN
	END
