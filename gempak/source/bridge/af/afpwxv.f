	SUBROUTINE AF_PWXV  ( report, iswxv, iewxv, iret )
C************************************************************************
C* AF_PWXV								*
C*									*
C* This subroutine decodes and stores the weather data from within a	*
C* PIREP report.							*
C*									*
C* AF_PWXV  ( REPORT, ISWXV, IEWXV, IRET )				*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		PIREP report 			*
C*	ISWXV		INTEGER		Pointer to start of weather	*
C*					data within REPORT 		*
C*	IEWXV		INTEGER		Pointer to end of weather	*
C*					data within REPORT 		*
C*									*
C* Output parameters:							*
C*	RIVALS (IRVSBY)	REAL		Visibility in miles		*
C*	RIVALS (IRNPWX)	REAL		Number of weather levels	*
C*	CIVALS (ICWCOD)	CHAR*		Weather string			*
C*	RIVALS (IRHBWX)	REAL		Base of weather in feet  	*
C*	RIVALS (IRHTWX)	REAL		Top of weather in feet  	*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		10/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NP12		10/97	Use PT_WNMT to test for pwx observation	*
C* D. Kidwell/NCEP	 4/98	Use DC_WTHR to test for pwx observation	*
C* D. Kidwell/NCEP	 7/99	Changed meters to feet in prologue      *
C* D. Kidwell/NCEP	 8/99	Use AF_PTOP to get top of weather       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	report
C*
	CHARACTER	pwx*8, tmpwx*6
	LOGICAL		gotvsb, done, wxfnd
C*
	INCLUDE		'ERMISS.FNC'
	INCLUDE		'affnc.fnc'
C-----------------------------------------------------------------------
	iret = 0
	ifptr = 1
C
C*	Break up the input string into groups of "like-type" in order
C*	to facilitate decoding.
C
	CALL AF_BKGP  ( report ( iswxv : iewxv ), ierbgp )
	IF  ( ierbgp .ne. 0 )  THEN
	    RETURN
	END IF
C
C*	Scan no further than the first 3 "like-type" groups in order
C*	to locate the flight visibility data.  This data is optional.
C
	gotvsb = .false.
	ii = ifptr
	maxii = IEDX ( ii, 3, nflds )
C
	DO WHILE  (  ( ( ii + 1 ) .le. maxii ) .and.
     +			( .not. gotvsb )  )
	    IF  (  ( lensf ( ii ) .eq. 2 ) .and.
     +		  ( fields ( ii ) (1:2) .eq. 'FV' ) .and.
     +		  ( itypsf ( ii + 1 ) .eq. NMR )  )  THEN
		gotvsb = .true.
		ifptr = ii + 2
C
C*		Decode and store the flight visibility.
C
		CALL ST_INTG
     +		    ( fields ( ii + 1 ) ( 1 : lensf ( ii + 1 ) ),
     +		      ivsby, ier )
		IF  ( ier .eq. 0 )  THEN
		    rivals ( irvsby ) = FLOAT ( ivsby )
		END IF
C
C*		If the next "like-type" group contains the indicator
C*		"SM" (i.e. "statute miles"), then skip over it.
C
		IF  ( ifptr .le. nflds )  THEN
		    IF  (  ( lensf ( ifptr ) .eq. 2 ) .and.
     +			  ( fields ( ifptr ) (1:2) .eq. 'SM' )  )  THEN
			ifptr = ifptr + 1
		    END IF
		END IF
	    END IF
	    ii = ii + 1
	END DO
C
C*	Locate, decode, and store as many as MXWLYR present weather
C*	observations along with any associated intensity indicators
C*	and/or altitude ranges.  Haze may have already been reported
C*	in the sky cover (/SK) element.
C
	done = .false.
	npwx = MAX ( 0, NINT ( rivals ( irnpwx ) ) ) 
C
	DO WHILE  ( ( npwx .lt. MXWLYR ) .and. ( .not. done ) )
C
C*	    Locate a present weather observation by searching for an
C*	    alphabetic "like-type" group of between 2 and 6 characters
C*	    in length and then verifying that this group contains a
C*	    present weather observation by testing with DC_WTHR.
C
	    ii = ifptr
	    locpwx = IMISSD
C
	    DO WHILE  ( ( ii .le. nflds ) .and. ( locpwx .eq. IMISSD ) )
		IF  (  ( itypsf ( ii ) .eq. ALPHA ) .and.
     +			( lensf ( ii ) .ge. 2 ) .and.
     +			( lensf ( ii ) .le. 6 )  )  THEN
		    CALL DC_WTHR ( fields (ii) ( 1:lensf (ii) ), 
     +				   lensf (ii), wxfnd, tmpwx, ier )
		    IF  ( wxfnd ) THEN
			locpwx = ii
			lenst = INDEX ( tmpwx, ' ' ) - 1
			IF ( lenst .lt. 0 ) lenst = 6
		    END IF
		END IF
		ii = ii + 1
	    END DO
C
	    IF  ( locpwx .eq. IMISSD )  THEN
		done = .true.
	      ELSE
		ispwx = 2
		iepwx = lenst + 1
		pwx ( ispwx : iepwx ) = tmpwx ( 1:lenst )
		IF  ( locpwx .gt. ifptr )  THEN
C
C*		    Check for a "like-type" group containing an
C*		    intensity indicator directly before the "like-type"
C*		    group containing the present weather observation.
C
		    IF  ( ( fields ( locpwx - 1 ) (1:1) .eq. '+' ) .or.
     +			  ( fields ( locpwx - 1 ) (1:1) .eq. '-' ) )
     +			THEN
			ispwx = 1
			pwx ( 1:1 ) = fields ( locpwx - 1 ) (1:1)
		    END IF
		END IF
		ifptr = ii
C
C*		Store the present weather observation.
C
		npwx = npwx + 1
		civals ( icwcod ( npwx ) ) = pwx ( ispwx : iepwx )
C
C*		Check whether an altitude range was reported for this
C*		present weather observation and, if so, decode it.
C
		IF  ( ( ifptr .le. nflds ) .and.
     +		      ( ( itypsf ( ifptr ) .eq. NMR ) .or.
     +			( fields ( ifptr ) (1:4) .eq. 'UNKN' ) ) ) THEN
C
C*		    Get the base of the weather.
C
		    CALL AF_HHFM
     +			( fields ( ifptr ) ( 1 : lensf ( ifptr ) ),
     +			  rhm1, ierhfm )
		    ifptr = ifptr + 1
		  ELSE
		    rhm1 = RMISSD
		END IF
C
C*		Check for the top of the weather.
C
		CALL AF_PTOP ( ifptr, nflds, iptr, rhm2, ier )
		IF  ( ERMISS ( rhm2 ) )  rhm2 = rhm1
		IF  ( ( .not. ERMISS ( rhm1 ) ) .and.
     +		      ( .not. ERMISS ( rhm2 ) ) )  THEN
		    rivals ( irhbwx ( npwx ) ) = AMIN1 ( rhm1, rhm2 )
		    rivals ( irhtwx ( npwx ) ) = AMAX1 ( rhm1, rhm2 )
		  ELSE
		    rivals ( irhbwx ( npwx ) ) = rhm1
		    rivals ( irhtwx ( npwx ) ) = rhm2
	        END IF
		ifptr = iptr
	    END IF
	END DO
C
	rivals ( irnpwx ) = npwx
C*
	RETURN
	END
