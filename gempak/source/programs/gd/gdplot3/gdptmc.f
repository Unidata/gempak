	SUBROUTINE GDPTMC ( gdatim, gdfile, cycle, sep, maxlen,
     +			    ntimes, timstr, length, iret )
C************************************************************************
C* GDPTMC								*
C*									*
C* This subroutine creates an array of times for use by the subroutine	*
C* GDPLTB.								*
C**									*
C* GDPTMC  ( GDATIM, GDFILE, CYCLE, SEP, MAXLEN, 			*
C*	     NTIMES, TIMSTR, LENGTH, IRET )				*
C*									*
C* Input parameters:							*
C*	GDATIM		CHAR*		GDPLOT date/time string		*
C*	GDFILE		CHAR*		GDPLOT grid file name		*
C*	CYCLE		CHAR*		Cycle reference time		*
C*	SEP		CHAR*		String separator		*
C*	MAXLEN		INTEGER		Max length of TIMSTR		*
C*									*
C* Output parameters:							*
C*	NTIMES		INTEGER		Number of times in timstr	*
C*	TIMSTR		CHAR*	  	Array of times as string w/ sep	*
C*	LENGTH		INTEGER		Length of TIMSTR		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*                                       -1 = error in processing gdatim*
C*                                      -20 = no times selected         *
C*                                      -29 = file open failure         *
C*                                      -30 = error opening file        *
C*                                      -31 = navigation not the same   *
C*                                      -33 = too many files to open    *
C*                                      -34 = more than one output file *
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	1/97	New for gdplot.				*
C* D.W.Plummer/NCEP	8/98	Changed calling sequence for GDPTMS	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'gdplot.cmn'
C*
	CHARACTER*(*)	gdatim, gdfile, sep, timstr, cycle
	CHARACTER	timfnd(LLMXGT)*20
C
	iret = 0
C
	CALL GDPTMS ( gdatim, gdfile, cycle, LLMXGT, 
     +		      ntimes, timfnd, iret )
C
C
	IF ( iret .eq. 0 )  THEN
C
		lt = 0
		CALL ST_LSTR ( timfnd(1), lf, iret )
		timstr = timfnd(1)(:lf)
		lt = lt + lf
		DO  n = 2, ntimes
			timstr = timstr(:lt) // sep(:1)
			lt = lt + 1
			CALL ST_LSTR ( timfnd(n), lf, iret )
			IF ( lt+lf .le. maxlen )  THEN
				timstr = timstr(:lt) // timfnd(n)(:lf)
				lt = lt + lf
			END IF
		END DO
C
		length = lt
		timstr = timstr(:lt) // CHNULL
C
	END IF
C*
	RETURN
	END
