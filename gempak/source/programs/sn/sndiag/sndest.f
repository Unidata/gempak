	SUBROUTINE SNDEST ( delz, nlun, lun, nparms, nlevel, hdata,
     +			    iret )
C************************************************************************
C* SNDEST								*
C*									*
C* This routine will estimate the temperature and height of the 	*
C* tropopause.								*
C*									*
C* SNDEST ( DELZ, NLUN, LUN, NPARMS, NLEVEL, HDATA, IRET )		*
C*									*
C* Input parameters:							*
C*	DELZ		CHAR*		Height increment		*
C*	NLUN		INTEGER		Number of file numbers		*
C*	LUN (NLUN)	INTEGER		File numbers			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	NLEVEL		INTEGER		Number of levels		*
C*	HDATA (LLMXLV)	REAL		Interpolated data		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up header		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sndiag.prm'
C*
	CHARACTER*(*)	delz
	INTEGER		lun(*)
	REAL		hdata(*)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
	tmin = 50.
	hmin = 0.
	tlst = -50.
C
	CALL ST_CRNM ( delz, deltaz, ier )
	IF  ( ier .ne. 0 )  deltaz = 500.
C
C*	Find the minimum temperature for the sounding.
C
	DO  i = 1, nlevel
	    IF  ( hdata((i-1)*nparms+ITEMP) .lt. tmin )  THEN
		tmin = hdata((i-1)*nparms+ITEMP)
		hmin = hdata((i-1)*nparms+IHGHT)
	    END IF
	END DO
C
C*	Find all inversions in the sounding.
C
        DO  i = 2, nlevel-1
            IF  ( .not. ERMISS(hdata((i-1+1)*nparms+ITEMP)) .and.
     +            .not. ERMISS(hdata((i-1)*nparms+ITEMP  )) .and.
     +            .not. ERMISS(hdata((i-1-1)*nparms+ITEMP)) )  THEN
                x1 = ( hdata((i-1+1)*nparms+ITEMP) -
     +                 hdata((i-1)*nparms+ITEMP) ) * 1000. / deltaz
                x2 = ( hdata((i-1)*nparms+ITEMP) -
     +                 hdata((i-1-1)*nparms+ITEMP) ) * 1000. / deltaz
		xlapse = (x1 + x2) / 2.
            ELSE
                xlapse = RMISSD
            END IF
	    IF  ( xlapse .ge. 0. .and. tlst .lt. 0. )  THEN
		temp = hdata((i-1)*nparms+ITEMP)
		hght = hdata((i-1)*nparms+IHGHT)
C
C*		Write the output for the inversions.
C
		DO  k = 1, nlun
		    WRITE ( lun(k), 1000 ) hght, temp
		END DO
	    END IF
	    tlst = xlapse
	END DO
C
C*	Write the minimum temperature and its height.
C
	DO  k = 1, nlun
	    WRITE ( lun(k), 2000 ) hmin, tmin
	END DO
C
1000	FORMAT ( /, ' Inversion located:',/,
     +              '    Height      : ', F10.2, ' m', /,
     +              '    Temperature : ', F10.2, ' C' )
2000	FORMAT ( /, ' Minimum temperature in the sounding located:',/,
     +              '    Height      : ', F10.2, ' m', /,
     +              '    Temperature : ', F10.2, ' C' )
C*
	RETURN
	END
