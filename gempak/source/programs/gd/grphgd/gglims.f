	SUBROUTINE GGLIMS ( imn, imx, jmn, jmx,
     +			    grid, hist, kx, ky, iret )
C************************************************************************
C* GGLIMS                                                               *
C*                                                                      *
C* This subroutine applies limits to the final graph-to-grid grid.	*
C*                                                                      *
C* GGLIMS ( IMN, IMX, JMN, JMX, GRID, HIST, KX, KY, IRET )              *
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
C* D.W.Plummer/NCEP      9/99                                           *
C* B.E.McDonald/NCEP     3/00   Added default check for case of 	*
C*                              nolines>0 but grid is missing   	*
C* D.W.Plummer/NCEP	12/02	Use new parameters			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'grphgd.cmn'
C
C------------------------------------------------------------------------
C
	REAL		grid(kx,ky), hist(kx,ky)
C
C------------------------------------------------------------------------
	iret = 0
C
C*      Loop over range of gridpoints for lower and upper limits
C
	IF ( lochk .ne. RMISSD )  THEN
C
	  DO  i = imn, imx
	    DO  j = jmn, jmx
C
	      IF ( hist(i,j) .ge. EXACT )  THEN
C
                  IF ( grid(i,j) .le. lochk )  grid(i,j) = loval
C
	      END IF
C
	    END DO
	  END DO
C
	END IF
C
	IF ( hichk .ne. RMISSD )  THEN
C
	  DO  i = imn, imx
	    DO  j = jmn, jmx
C
	      IF ( hist(i,j) .ge. EXACT )  THEN
C
                  IF ( grid(i,j) .ge. hichk )  grid(i,j) = hival
C
	      END IF
C
	    END DO
	  END DO
C
	END IF
C
        IF ( defalt .ne. RMISSD )  THEN
C
          DO  i = imn, imx
            DO  j = jmn, jmx
C
              IF ( hist(i,j).ne.INIT .or. grid(i,j).ne.RMISSD )  THEN
C
                RETURN
C
              END IF
C
            END DO
          END DO
C
          DO  i = imn, imx
            DO  j = jmn, jmx
C
              grid(i,j) = defalt
C
            END DO
          END DO
C
        END IF
C
	RETURN
	END
