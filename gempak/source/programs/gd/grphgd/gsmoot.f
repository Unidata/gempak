	SUBROUTINE GSMOOT ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GSMOOT                                                               *
C*                                                                      *
C* This subroutine performs smoothing for the graph-to-grid algorithm.  *
C*                                                                      *
C* GSMOOT ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )              *
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
C* D.W.Plummer/NCEP      2/99	Skip points which were assigned hist 2	*
C* 				(exact contour value)			*
C* D.W.Plummer/NCEP      9/99	Only do pts w/ assigned values .gt. 2	*
C* W.D.Plummer/NCEP     12/02	Use new history parameters		*
C* W.D.Plummer/NCEP      9/03	Chg calling seq to GINDX1		*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
	REAL		grid(kx,ky), hist(kx,ky)
C
	REAL		d(8), dchk(8), smooth
	LOGICAL		close
C
	DATA		smooth/ 8 /
	DATA		d/ 8*0 /
	DATA		dchk/   1.414, 1.0, 1.414, 1.0,
     +				1.414, 1.0, 1.414, 1.0 /
C
C*      Loop over range of gridpoints
C
	DO  i = imn, imx
C
	  DO  j = jmn, jmx
C
	      IF ( hist(i,j) .gt. EXACT )  THEN
C
	          n = 0
	          close = .false.
	          DO  ndir = 1, 8
C
                    CALL GCIIJJ ( i, j, ky, ndir, ij, iret )
C
                    CALL GINDX1 ( i, j, ij, ndir, ffi, ffj, lin, iret )
C
		    val = value(lin)
		    dist = sqrt( (ffi-i)*(ffi-i) + (ffj-j)*(ffj-j) )
C
		    IF ( dist .lt. dchk(ndir) )  close = .true.
C
		    CALL GNEXTV ( i, j, ndir, grid, hist, kx, ky,
     +			          finxt, fjnxt, valnxt, iret )
C
		    IF ( valnxt .ne. RMISSD )  THEN
		        d(ndir) = valnxt
		        n = n + 1
		    ELSE
		        d(ndir) = RMISSD
		    END IF
C
	          END DO
C
	          IF ( n .gt. 0 .and. .not. close )  THEN
C
	            r = 0.0
		    DO ndir = 1, 8
C
                      IF ( d(ndir) .ne. RMISSD ) THEN
		        r = r + d(ndir)
                      ENDIF
C
                    END DO
C
		    grid(i,j) = (grid(i,j) + (smooth/n)*r) / (1+smooth)
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
