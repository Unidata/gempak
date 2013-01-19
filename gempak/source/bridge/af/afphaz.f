	SUBROUTINE AF_PHAZ  ( iret )
C************************************************************************
C* AF_PHAZ								*
C*									*
C* This subroutine looks for one of several forms of the weather string *
C* 'HZ' in an PIREP report element and, if found, decodes it along with *
C* its base and height, if present.  Haze should be reported as weather,*
C* but is sometimes reported in the sky or remark element.              *
C*									*
C* AF_PHAZ  ( IRET )							*
C*									*
C* Output parameters:							*
C*	RIVALS (IRNPWX) REAL		Number of weather levels        *
C*	CIVALS (ICWCOD) CHAR*		Haze weather string             *
C*	RIVALS (IRHBWX) REAL		Base of haze in feet            *
C*	RIVALS (IRHTWX) REAL		Top of haze in feet             *
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/99						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	LOGICAL		done, haze
C*
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
	iret = 0
C
C*	Look for the string 'HZ' by itself.
C
	CALL ST_FIND ( 'HZ', fields, nflds, ipos, ier )
	IF ( ipos .eq. 0 ) THEN
	    done = .false.
	    ii   = 1
C
C*	    Look for embedded string 'HZ' or variations on 'HZ'.
C
	    DO WHILE ( .not. done )
                i0 = INDEX ( fields ( ii ), 'HZ' )
                i1 = INDEX ( fields ( ii ), 'HAZ' )
	        IF ( ( i0 + i1 ) .gt. 0 ) THEN
C
C*		    A haze indicator was found.
C
		    done = .true.
		    ipos = ii
		  ELSE
		    ii = ii + 1
		    IF ( ii .gt. nflds ) done = .true.
		END IF
	    END DO
	END IF
C
	IF ( ipos .gt. 0 ) THEN
C
C*	    A haze field was found.  Check to see if it should be 
C*	    decoded as weather.
C
	    haze = .true.
	    npwx = MAX ( 0, NINT ( rivals ( irnpwx ) ) )
	    DO i = 1, npwx
		IF ( civals ( icwcod ( i ) ) .eq. 'HZ' ) haze = .false.
	    END DO   
	    IF ( npwx .eq. MXWLYR ) haze = .false.
	    IF ( ( ipos .gt. 1 ) .and. 
     +		 ( fields ( ipos - 1 ) .eq. 'NO' ) ) haze = .false.
	  ELSE
	    haze = .false.
	END IF
C
	IF ( haze ) THEN
	    npwx  = npwx + 1
	    civals ( icwcod ( npwx ) ) = 'HZ'
	    ifptr = ipos + 1
C
C*	    Check whether an altitude range was reported for haze
C*	    and, if so, decode it.
C
	    IF ( ( ifptr .le. nflds ) .and.
     +		 ( ( itypsf ( ifptr ) .eq. NMR ) .or.
     +		   ( fields ( ifptr ) (1:4) .eq. 'UNKN' ) ) ) THEN
C
C*	        Get the base of the haze.
C
		CALL AF_HHFM ( fields ( ifptr ) ( : lensf ( ifptr ) ),
     +			       rhm1, ierhfm )
		ifptr = ifptr + 1
	      ELSE
		rhm1  = RMISSD
	    END IF
C
C*	    Check for the top of the haze.
C
	    CALL AF_PTOP ( ifptr, nflds, iptr, rhm2, ier )
	    IF  ( ERMISS ( rhm2 ) )  rhm2 = rhm1
	    IF  ( ( .not. ERMISS ( rhm1 ) ) .and.
     +	          ( .not. ERMISS ( rhm2 ) ) )  THEN
	        rivals ( irhbwx ( npwx ) ) = AMIN1 ( rhm1, rhm2 )
	        rivals ( irhtwx ( npwx ) ) = AMAX1 ( rhm1, rhm2 )
	      ELSE
	        rivals ( irhbwx ( npwx ) ) = rhm1
	        rivals ( irhtwx ( npwx ) ) = rhm2
	    END IF
	    rivals ( irnpwx ) = npwx
	END IF
C*
	RETURN
	END
