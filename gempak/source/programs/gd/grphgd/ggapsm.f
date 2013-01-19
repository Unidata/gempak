        SUBROUTINE GGAPSM ( fii, fji, npi, kx, ky, ismo,
     +			    iclsd, extend, fio, fjo, npo, iret )
C************************************************************************
C* GGAPSM                                                               *
C*                                                                      *
C* This subroutine takes a line (consisting of multiple segments) and	*
C* 1) performs smoothing of the line by adding points consistent with	*
C*    a parametric curve fitting algorithm, and				*
C* 2) adds endpoints to the line fording the ends to be outside the	*
C*    grid area.							*
C*                                                                      *
C* GGAPSM ( FII, FJI, NPI, KX, KY, ISMO, ICLSD, FIO, FJO, NPO, IRET )	*
C*                                                                      *
C* Input parameters:                                                    *
C*      FII (NPI)	REAL		I values in grid coords		*
C*      FJI (NPI)	REAL		J values in grid coords		*
C*	NPI		INTEGER		Number of points		*
C*      KX              INTEGER         Number of grid points in X      *
C*      KY              INTEGER         Number of grid points in Y      *
C*	ISMO		INTEGER		Smoothing factor		*
C*	ICLSD		INTEGER		Closed line indicator		*
C*                                                                      *
C* Output parameters:                                                   *
C*      FIO (NPO)	REAL		I values in grid coords		*
C*      FJO (NPO)	REAL		J values in grid coords		*
C*	NPO		INTEGER		Number of points		*
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98                                           *
C* D.W.Plummer/NCEP      9/98	Change calling sequence of CV_PRMT	*
C* W.D.Plummer/NCEP     12/02	Add GQCVSC call for device curve scale	*
C* W.D.Plummer/NCEP     07/03	Chg MAXPTS to MAXPPL			*
C* W.D.Plummer/NCEP     09/05	Add 'extend' param to calling seq 	*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'grphgd.cmn'
C
        REAL            fii(*), fji(*), fio(*), fjo(*)
        LOGICAL		extend
C
	REAL		m
C*
	CALL GQCVSC ( dvcvsc, ier )
C
	dens = ismo
	IF ( ismo .eq. 2 )  dens = 5.0
	istrt = 0
	iend  = 0
	CALL CV_PRMT ( npi, fii, fji, dens, MAXPPL, dvcvsc,
     +		       istrt, iend, npo, fio, fjo, iret )
C
C*	Add point to starting endpoint iff inside grid area and 
C*	curve is not closed AND the BNDS parameter blank
C
	IF ( extend )  THEN
C
	IF ( fio(1) .ge. 1 .and. fio(1) .le. kx  .and.
     +	     fjo(1) .ge. 1 .and. fjo(1) .le. ky  .and.
     +	     iclsd .eq. 0 )  THEN
C
	    IF ( fio(2) .eq. fio(1) )  THEN
		xnew = fio(1)
		IF ( fio(1) .lt. fio(2) )  THEN
		    ynew = 0.0
		ELSE
		    ynew = ky + 1.0
		END IF
	    ELSE
	        m = ( fjo(2) - fjo(1) ) / ( fio(2) - fio(1) )
	        b = fjo(1) - ( m * fio(1) )
		IF ( fio(1) .lt. fio(2) )  THEN
	            xnew = 0.0
		ELSE
	            xnew = kx + 1.0
		END IF
	        ynew = m * xnew + b
		IF ( ynew .gt. ky+1 )  THEN
		    ynew = ky + 1
		    xnew = ( ynew - b ) / m
		ELSE IF ( ynew .lt. 0 )  THEN
		    ynew = 0
		    xnew = ( ynew - b ) / m
		END IF
	    END IF
C
	    DO  n = npo, 1, -1
		fio(n+1) = fio(n)
		fjo(n+1) = fjo(n)
	    END DO
	    fio(1) = xnew
	    fjo(1) = ynew
C
	    npo = npo + 1
C
	END IF
C
C*	Add point to ending endpoint iff inside grid area and 
C*	curve is not closed
C
	IF ( fio(npo) .ge. 1 .and. fio(npo) .le. kx  .and.
     +	     fjo(npo) .ge. 1 .and. fjo(npo) .le. ky  .and.
     +       iclsd .eq. 0 )  THEN
C
	    IF ( fio(npo-1) .eq. fio(npo) )  THEN
		xnew = fio(npo)
		IF ( fio(npo) .lt. fio(npo-1) )  THEN
		    ynew = 0.0
		ELSE
		    ynew = ky + 1.0
		END IF
	    ELSE
	        m = ( fjo(npo-1) - fjo(npo) ) / ( fio(npo-1) - fio(npo) )
	        b = fjo(npo) - ( m * fio(npo) )
		IF ( fio(npo) .lt. fio(npo-1) )  THEN
	            xnew = 0.0
		ELSE
	            xnew = kx + 1.0
		END IF
	        ynew = m * xnew + b
		IF ( ynew .gt. ky+1 )  THEN
		    ynew = ky + 1
		    xnew = ( ynew - b ) / m
		ELSE IF ( ynew .lt. 0 )  THEN
		    ynew = 0
		    xnew = ( ynew - b ) / m
		END IF
	    END IF
C
	    npo = npo + 1
	    fio(npo) = xnew
	    fjo(npo) = ynew
C
	END IF
C
	END IF
C
	RETURN
	END
