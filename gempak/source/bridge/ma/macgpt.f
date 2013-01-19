	SUBROUTINE MA_CGPT ( fldnin, prsdon, rmkdon, fldnou, ier )
C************************************************************************
C* MA_CGPT                                                              *
C*                                                                      *
C* This subroutine decodes the altimeter or pressure and any numerical  *
C* remarks fields in a single report.  Parameters used which are not in	*
C* the calling sequence are found in common.				*
C*                                                                      *
C* MA_CGPT  ( FLDNIN, PRSDON, RMKDON, FLDNOU, IER )	        	*
C*                                                                      *
C* Input parameters:                                                    *
C*      FIELDS          CHAR*(*)        Array of fields found in input  *
C*                                      string                          *
C*      LENSF           INTEGER         Array of lengths of fields      *
C*      RIVALS(IRSELV)  REAL            Station elevation (m)           *
C*      RIVALS(IRTMPF)  REAL            Air temperature (deg F)         *
C*      RIVALS(IRMNTH)  REAL            Observation month               *
C*      FLDNIN          INTEGER         Number of field to work on.     *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      PRSDON          LOGICAL         If true, pressure field has been*
C*                                      decoded.                        *
C*      RMKDON          LOGICAL         If true, remarks fields have    *
C*                                      been decoded.                   *
C* Output parameters:                                                   *
C*      RIVALS(IRALTI)  REAL            Altimeter setting (in. Hg)      *
C*      RIVALS(IRPMSL)  REAL            Sea-level pressure (mb)         *
C*      RIVALS(IRPRES)  REAL            Station pressure (mb)           *
C*      RIVALS(IRMXTM)  REAL            Maximum temperature (deg C)     *
C*      RIVALS(IRMITM)  REAL            Minimum temperature (deg C)     *
C*      FLDNOU          INTEGER         Number of next field to work on *
C*      IER             INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       non-zero = Problem             *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01   Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_PRTM.	*
C*				Changed sequence order of parameters.	*
C*				Changed	prologue regarding common parm.	*
C* F. J. Yen/NCEP	 7/01	Included GEMPRM.PRM.(S.Chiswell/Unidata)*
C* D. Kidwell/NCEP	 3/02	Corrected prologue                      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
	INTEGER		fldnin, fldnou
	LOGICAL		prsdon, rmkdon
C------------------------------------------------------------------------
C
C*	Field is either pressure or max/min temp. in Remarks.
C*	Check length to see if it's 4 digit altimeter (e.g. 2982), 
C*	4 digit barometer (e.g. 1018), first 2 digits of 5-digit alti
C*	(e.g. '29' of 29.82), or 5-digit max/min temp (e.g. 45441, where
C*	'54' is max temp and '41' is min temp).  Also check for max/min
C*	temperature field encoded as '454//' (min temp missing) or
C*	'4//41' (max temp missing).
C
	i = fldnin
	IF ( lensf(i) .eq. 5 ) THEN
	    IF ( fields(i)(1:1) .eq. '4' ) THEN
C
C*              Get maximum temperature.         
C
		CALL MA_CGMM ( fields(i)(2:3), 1, ier )
C
C*          	Get minimum temperature.         
C
		CALL MA_CGMM ( fields(i)(4:5), 2, ier )
		prsdon = .true.
		rmkdon = .true.
		fldnou = fldnin + 1
 	      ELSE
C
C*		Bad group or format error.
C
		logmsg = ' Invalid group/format error (len = 5) ' //
     +                   'in numerical rmks.'
		CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		fldnou = fldnin + 1
	    END IF
	  ELSE IF ( lensf(i) .eq. 4) THEN
	    IF ( .not. prsdon ) THEN
		IF ( fields(i)(1:1) .eq. '1' ) THEN
C
C*		    Pressure in millibars.  Check upper bound before
C*		    saving and copy into pmsl if selv .lt. 7.5 meters.
C
		    CALL ST_INTG ( fields(i)(1:4), ist1, ier )
		    IF ( ist1 .le. 1100 ) THEN
			rivals(irpres) = FLOAT( ist1 )
			IF ( rivals(irselv) .lt. 7.5 ) THEN
			    rivals(irpmsl) = rivals(irpres)
			END IF
		    END IF
		  ELSE IF ( fields(i)(1:1) .eq. '2' .or.
     +			   fields(i)(1:1) .eq. '3' ) THEN
C
C*          Altimeter reading in inches of mercury.  Check upper bound
C*          before saving.
C
		    CALL ST_INTG ( fields(i)(1:4), ist1, ier )
		    IF ( ist1 .le. 3350 ) THEN
			rivals(iralti) = FLOAT( ist1 ) / 100.
		    END IF
		  ELSE
C
C*		    Bad group or format error.
C
		    logmsg = ' Invalid group/format error (len = 4)' //
     +                       ' in numerical rmks.'
		    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		END IF
		prsdon = .true.
		fldnou = fldnin + 1
	      ELSE
C
C*		Bad group or format error.
C
		logmsg = ' Invalid group/format error (len = 4) in' //
     +                   ' numerical rmks.'
		CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
	    END IF
	  ELSE IF ( lensf(i) .eq. 3) THEN
	    IF (fields(i)(1:1) .eq. '4') THEN
C
C*		Maximum temperature
C
                CALL MA_CGMM ( fields(i)(2:3), 1, ier )
	      ELSE
C
C*		Bad group or format error.
C
		logmsg = ' Invalid group/format error (len = 3) in' //
     +                   ' numerical rmks.'
		CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
	    END IF
	    prsdon = .true.
	    rmkdon = .true.
	    fldnou = fldnin + 2
	  ELSE IF ( lensf(i) .eq. 2 ) THEN
	    IF ( .not. prsdon ) THEN
C
C*		Check to see if it's first 2 digits of altimeter.
C
		CALL ST_INTG ( fields(i)(1:2), ist1, ier)
		tmp1alti = FLOAT( ist1 )
		IF (fields(i+1) .ne. '.') THEN
C
C*		    Format error (alti not of optional form xx.xx).
C
		    logmsg = ' Format error in alti/baro.'
		    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		    prsdon = .true.
		    fldnou = fldnin + 1
		    RETURN
		  ELSE
C
C*		    Get last 2 digits (10th's and 100th's) of altimeter.
C*		    Check upper bound before saving.
C
		    CALL ST_INTG ( fields(i+2)(1:2), ist1, ier)
		    tmp2alti = FLOAT( ist1 )
		    altitmp = tmp1alti + ( tmp2alti / 100. )
		    IF ( altitmp .le. 33.50 ) THEN
			rivals(iralti) = altitmp 
		    END IF
		    fldnou = fldnin + 3
		    prsdon = .true.
		END IF
	    END IF
	  ELSE IF ( lensf(i) .eq. 1) THEN
	    IF (fields(i)(1:1) .eq. '4') THEN
		IF ( lensf(i+1) .eq. 2 .and.
     +		     fields(i+1) .eq. '//' ) THEN
C
C*		    Max temp. is missing so get min temp.
C
		    CALL MA_CGMM ( fields(i+2)(1:2), 2, ier )
		END IF
	      ELSE
C
C*		Bad group or format error.
C
		logmsg = ' Invalid group/format error (len = 1) in' //
     +                   ' numerical rmks.'
		CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
	    END IF
	    prsdon = .true.
	    rmkdon = .true.
	    fldnou = fldnin + 3
	  ELSE
	    fldnou = fldnin + 1
	END IF
C
C*	Check to see if both max temp. and min temp were saved.
C*	If so, and max is less than min, set both to missing.
C
	IF ( rivals(irmxtm) .ne. RMISSD .and.
     +	           rivals(irmitm) .ne. RMISSD ) THEN
	    IF ( rivals(irmxtm) .lt. rivals(irmitm) ) THEN
		WRITE  ( UNIT = logmsg, FMT = '( A, 2F6.2 )' )
     +                ' Max temp lt min temp...mxtm, mitm =',
     +                rivals(irmxtm), rivals(irmitm)
		CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		rivals(irmxtm) = RMISSD
		rivals(irmitm) = RMISSD
	    END IF
	END IF
C*
	RETURN 
	END
