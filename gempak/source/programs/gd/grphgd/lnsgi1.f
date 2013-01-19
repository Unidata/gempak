        SUBROUTINE LNSGI1 ( x1, y1, x2, y2, nlpts, xlins, ylins,
     +                      xint, yint, iret )
C************************************************************************
C* LNSGI1                                                               *
C*                                                                      *
C* This subroutine calculates the intersection point(s) between a 	*
C* polygon (xlins,ylins) & the ray					*
C*									*
C*	RAY = [ (x1,y1) - (x2,y2) ) i.e., from (x1,y1) thru (x2,y2)	*
C*									*
C* and returns the intersection point closest to the point (x1,y1).	*
C* If no intersection points meet the criteria, (RMISSD,RMISSD)		*
C* is returned for (xint,yint).						*
C*									*
C*                                                                      *
C* LNSGI1 ( XLIN, YLIN, XLINS, YLINS, NPTS, XINT, YINT, IRET )		*
C*                                                                      *
C* Input parameters:                                                    *
C*      X1              REAL            Ray starting X coord		*
C*      Y1              REAL            Ray starting Y coord		*
C*      X2              REAL            Ray ending X coord		*
C*      Y2              REAL            Ray ending Y coord		*
C*	NLPTS		INTEGER		Number of points along X/YLINS	*
C*      XLINS (NLPTS)   REAL            Poly line X input coords	*
C*      YLINS (NLPTS)   REAL            Poly line Y input coords	*
C*                                                                      *
C* Output parameters:                                                   *
C*      XINT            REAL            X intersection                  *
C*      YINT            REAL            Y intersection                  *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP	 9/03						*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
	INCLUDE         'grphgd.cmn'
C
	REAL	xlins(*), ylins(*)
	REAL	xint, yint
	REAL	xlin(2), ylin(2)
C
	REAL	xints(50), yints(50)
C
C------------------------------------------------------------------------
C
	iret = 0
C
	xint = RMISSD
	yint = RMISSD
C
	xlin ( 1 ) = x1
	xlin ( 2 ) = x2
	ylin ( 1 ) = y1
	ylin ( 2 ) = y2
	CALL LNSGIN ( xlin, ylin, xlins, ylins, nlpts, MAXINT, numint, 
     &		xints, yints, iret )
C
	IF ( numint .eq. 0 )  THEN
	    RETURN
	END IF
C
	DO  ii = 1, numint
C
	    IF ( xlin(1) .gt. xlin(2) )  THEN
	    	IF ( ii .eq. 1 )  xint = 0
		IF ( xints(ii) .gt. xint .and. 
     &		     xints(ii) .lt. xlin(1) )  THEN
		    xint = xints(ii)
		    yint = yints(ii)
	        END IF
	    ELSE IF ( xlin(1) .lt. xlin(2) )  THEN
	    	IF ( ii .eq. 1 )  xint = 1000
		IF ( xints(ii) .lt. xint .and. 
     &		     xints(ii) .gt. xlin(1) )  THEN
		    xint = xints(ii)
		    yint = yints(ii)
	        END IF
	    ELSE
	        IF ( ylin(1) .gt. ylin(2) )  THEN
	    	    IF ( ii .eq. 1 )  yint = 0
		    IF ( yints(ii) .gt. yint .and. 
     &		         yints(ii) .lt. ylin(1) )  THEN
		        xint = xints(ii)
		        yint = yints(ii)
	            END IF
	        ELSE IF ( ylin(1) .lt. ylin(2) )  THEN
	    	    IF ( ii .eq. 1 )  yint = 1000
		    IF ( yints(ii) .lt. yint .and. 
     &		         yints(ii) .gt. ylin(1) )  THEN
		        xint = xints(ii)
		        yint = yints(ii)
	            END IF
	        END IF
	    END IF
C
	END DO
C
	RETURN
	END
