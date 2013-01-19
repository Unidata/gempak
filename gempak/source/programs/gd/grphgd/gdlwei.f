	SUBROUTINE GDLWEI ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GDLWEI                                                               *
C*                                                                      *
C* This subroutine performs the weighted search and gridpoint value     *
C* assignment for the graph-to-grid algorithm.                          *
C*                                                                      *
C* GDLWEI ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )		*
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
C* H. Zeng/SAIC		04/05	initial coding				*
C* D.W.Plummer/NCEP	07/05	Consider existence of intervening line	*
C* D.W.Plummer/NCEP	08/05	Rescan again to cover all shadow areas 	*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
C------------------------------------------------------------------------
C
	REAL		grid(kx,ky), hist(kx,ky)
	LOGICAL		done
C
C------------------------------------------------------------------------
C
C*      Loop from lower left and then from upper right.
C
 	DO  kk = 1, 4
C
	IF ( kk .eq. 1 .or. kk .eq. 3 ) THEN
C
            i1 = imn
            i2 = imx
            j1 = jmn
            j2 = jmx
            incr = 1
C
          ELSE IF ( kk .eq. 2 .or. kk .eq. 4 ) THEN
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
C*          Only try to calculate if grid point unassigned.
C
	    IF ( hist(i,j) .eq. INIT )  THEN
C
C*	      Initialize logical variable assigned
C
	      done = .false.
	      ndir = 1
	      DO WHILE ( ndir .le. 8 .and. .not. done )
C
		dstint = RMISSD
		dstnxt = RMISSD
C
		CALL GCIIJJ ( i, j, ky, ndir, ij, iret)
C 
		CALL GINDX1 ( i, j, ij, ndir, fiint, fjint, linint, iret)
		valint = RMISSD 
		IF ( linint .ne.  IMISSD) THEN
		    valint = value(linint)
		    dstint = (fiint-i)*(fiint-i) + (fjint-j)*(fjint-j) 
		END IF

	        CALL GNEXTV ( i, j, ndir, grid, hist, kx, ky,
     +			      finxt, fjnxt, valnxt, iret )
C
	        IF ( valnxt .ne. RMISSD )  THEN
		    dstnxt = (finxt-i)*(finxt-i) + (fjnxt-j)*(fjnxt-j)
		END IF
C
	        IF ( dstnxt .ne. RMISSD .and. valnxt .ne. RMISSD ) THEN
	          IF ( hist(finxt,fjnxt) .ne. EXACT ) THEN
	            IF ( ( dstint .eq. RMISSD .or.
     +                   ( dstint .ne. RMISSD .and. 
     +			   dstnxt .lt. dstint ) ) )  THEN
C 
		      grid(i,j) = valnxt
	              hist(i,j) = DLWEIGHTED
 		      done = .true.
C
	            END IF
	          END IF
C
	        ELSE 
C
		  CONTINUE
	        END IF
C
		ndir = ndir + 1
C
C*	        The end of WHILE
C
	      END DO
C
C*	    The end of IF ( hist(i,j)...
C
	    END IF
C
C*	  Grid point end do
	    IF ( kk .eq. 4 .and. hist(i,j) .eq. INIT )  THEN
	      grid(i,j) = defalt
	      hist(i,j) = DLWDEFALT
	    END IF
C
	  END DO
C
	END DO
C
	END DO
C
	RETURN
C
	END
