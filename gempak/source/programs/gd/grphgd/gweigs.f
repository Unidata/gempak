	SUBROUTINE GWEIGS ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GWEIGS                                                               *
C*                                                                      *
C* This subroutine performs the weighted search and gridpoint value     *
C* assignment for the graph-to-grid algorithm.                          *
C*                                                                      *
C* GWEIGS ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )		*
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
C* D.W.Plummer/NCEP      9/98                                           *
C* B.E.McDonald/NCSP     3/00   Loop backwards as well          	*
C* W.D.Plummer/NCEP	12/02	Use new history parameters		*
C* W.D.Plummer/NCEP	 9/03	Chg calling seq to GINDX1		*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
C------------------------------------------------------------------------
C
	REAL		grid(kx,ky), hist(kx,ky)
C
	REAL		d(8,2), r(2)
C
C------------------------------------------------------------------------
C
C*      Loop from lower left and then from upper right.
C
	DO  kk = 1, 2
C
	IF ( kk .eq. 1 ) THEN
C
            i1 = imn
            i2 = imx
            j1 = jmn
            j2 = jmx
            incr = 1
C
          ELSE IF ( kk .eq. 2 ) THEN
C
            i1 = imx
            i2 = imn
            j1 = jmx
            j2 = jmn
            incr = -1
C
	END IF
C
C*      Loop over range of gridpoints
C
	DO  i = i1, i2, incr
C
	  DO  j = j1, j2, incr
C
C*          Only try to calculate if gridpoint unassigned.
C
	    IF ( hist(i,j) .eq. INIT )  THEN
C
	      r(1) = 0.0
	      r(2) = 0.0
	      n = 0
	      DO  ndir = 1, 8
C
		d(ndir,1) = RMISSD
		d(ndir,2) = RMISSD
		dstint = RMISSD
		dstnxt = RMISSD
C
		CALL GCIIJJ ( i, j, ky, ndir, ij, iret )
C
		CALL GINDX1 ( i, j, ij, ndir, fiint, fjint, linint, iret )
		valint = RMISSD
		IF ( linint .ne. IMISSD )  THEN
		    valint = value(linint)
		    dstint = (fiint-i)*(fiint-i) + (fjint-j)*(fjint-j) 
		END IF
C
		CALL GNEXTV ( i, j, ndir, grid, hist, kx, ky,
     +			      finxt, fjnxt, valnxt, iret )
C
		IF ( valnxt .ne. RMISSD )  THEN
		    dstnxt = (finxt-i)*(finxt-i) + (fjnxt-j)*(fjnxt-j) 
		END IF
C
		IF ( dstnxt .ne. RMISSD )  THEN
C
		    d(ndir,1) = valnxt
		    d(ndir,2) = dstnxt
		    n = n + 1
C
		    IF ( dstint .ne. RMISSD .and. valint .ne. RMISSD .and.
     &			 dstint .lt. dstnxt )  THEN
                        d(ndir,1) = valint
			d(ndir,2) = dstint
		    END IF
C
		    d(ndir,2) = sqrt ( d(ndir,2) )
C
		END IF
C
	      END DO
C
	      IF ( n .gt. 0 )  THEN
C
		DO ndir = 1, 8
C
                  IF ( d(ndir,2) .ne. RMISSD ) THEN
C
		    r(1) = r(1) + ( d(ndir,1) / d(ndir,2) )
		    r(2) = r(2) + ( 1.0       / d(ndir,2) )
C
                  ENDIF
C
                END DO
C
		grid(i,j) = r(1) / r(2)
		hist(i,j) = WEIGHTED
C
              END IF
C
	    END IF
C
	  END DO
C
	END DO
C
	END DO
C
	RETURN
	END
