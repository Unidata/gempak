	SUBROUTINE GDLINE ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GDLINE                                                               *
C*                                                                      *
C* This subroutine performs the radial search and gridpoint value	*
C* assignment for the graph-to-grid algorithm.				*
C*                                                                      *
C* GDLINE ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )		*
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
C* H. Zeng/SAIC		04/05	initial coding				*
C* D.W.Plummer/NCEP	07/05	Use cgr_segdist to find closest segment	*
C* D.W.Plummer/NCEP	07/05	Use cgr_qrol to determine right-of-line	*
C* M. Li/SAIC		04/06	Compute for left-of-line		*
C* D.W.Plummer/NCEP	12/06	Add tol to cgr_qrol calling sequence	*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
	REAL		grid(kx,ky), hist(kx,ky)
	LOGICAL		assigned1, assigned2

	tol = 0.0
C
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
C*	      Initialize assigned1 and assigned2
C
	      assigned1 = .false.
	      assigned2 = .false.
C
	      DO  ndir = 1, 8
C
C*	        For each direction, get closest intersections
C
	        CALL GCIIJJ ( i, j, ky, ndir, ij, iret )
	        CALL GINDX1 ( i, j, ij, ndir, fi1, fj1, lin1, iret )
C
	        IF ( (lin1 .ne. IMISSD) .and. 
     +	             (value(lin1) .ne. RMISSD) ) THEN
C
		  qx = i
		  qy = j
C
C*	          Call cgr_qrol to determine whether the point is to the
C*	          right of the line
C
	          CALL CGR_QROL ( npts(lin1), fi( 1, lin1), fj( 1, lin1),
     &			closed(lin1), qx, qy, tol, irol, iret )
C
C*	          Call in_discq to determine whether it is discrete or 
C*	          continuous
C
	          CALL IN_DISCQ (istate, iret)
C
	          IF ( istate .eq. 1 ) THEN
C
		    CALL IN_DLINQ (istat, istatL, eps, iret)

	            IF ( irol .eq. 1 ) THEN
C
	              IF ( .not. assigned1 .AND. istat .eq. 1 ) THEN
C
		        grid(i,j) = value(lin1)
		        hist(i,j) = DLINE
	                assigned1 = .true.	 
C
		      END IF 
C
		    ELSE 

		       IF ( .not. assigned1 .AND. istatL .eq. 1 ) THEN
C
			CALL IN_DISCQLEFT ( value(lin1), valnew, ier )
                        grid(i,j) = valnew 
                        hist(i,j) = DLINE
                        assigned1 = .true.      
C
                      END IF
C
		    END IF
C
	          ELSE
C
	            IF ( .not. assigned2 ) THEN
C
C*		      Get epsilon value
C
		      CALL IN_DLINQ (istat, istatL, eps, iret)
C
	              IF ( irol .eq. 1 ) THEN
C
		        grid(i,j) = value(lin1) + eps
		        hist(i,j) = DLINE	  
C
		      ELSE
C
		        grid(i,j) = value(lin1) - eps
		        hist(i,j) = DLINE
C
		      END IF
C	  
	              assigned2 = .true.
C
	            END IF
C
	          END IF
C
C*	        The end of IF ( (lin1 .ne. IMISSD) .and. 
C
	        END IF
C
C*            Direction cycle end do
C
	      END DO
C
C*	    The end of IF ( hist(i,j) .eq. INIT )...
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
