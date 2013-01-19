	SUBROUTINE NC_PHEN ( report, lenr, phenom, iptr, iret )
C************************************************************************
C* NC_PHEN 								*
C*									*
C* This subroutine decodes a phenomenon from a non-convective sigmet    *
C* report.                                                              *
C*                                                                      *
C* NC_PHEN ( REPORT, LENR, PHENOM, IPTR, IRET )                         *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*									*
C* Output parameters:							*
C*	PHENOM		CHAR*  		Sigmet phenomenon               *
C*	IPTR		INTEGER		Pointer to location after phenom*
C*	IRET		INTEGER		Return code			*
C*					  1 = no flight levels follow   *
C*					  0 = normal return		*
C*					 -2 = valid phenom not found    *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/00	                                        *
C* D. Kidwell/NCEP	10/00	Added ICGICIP, ICGIC, BLDU, BLSA        *
C* D. Kidwell/NCEP	 4/03	Allowed '.' following phenomenon        *
C* J. Lewis/AWC		 4/05   Added TC, TROPICAL CYCLONE              *
C* L. Hinson/AWC        10/09   Add keywords WDSPR DS, WDSPR SS         *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, phenom
C*
	PARAMETER	( NPHEN = 15 )
C*
	CHARACTER	phen   (NPHEN) * 18, phen2 (NPHEN) * 2, cnext
	INTEGER		lenphn (NPHEN)
C*
	DATA		phen /
     +			' ICE', ' ICING', ' ICGICIP', ' ICGIC',
     +			' TURB',
     +			' DUST', ' SAND', ' BLDU', ' BLSA',
     +                  ' WDSPR DS', ' WDSPR SS', 
     +			' VOLCANIC ASH', ' VA', 
     +			' TROPICAL CYCLONE', ' TC' /
	DATA		phen2 /
     +			'IC', 'IC', 'IC', 'IC',
     +			'TB',
     +			'DU', 'DU', 'DU', 'DU',
     +                  'DU', 'DU',
     +			'VA', 'VA',
     +			'TC', 'TC' /
	DATA 		lenphn /
     +			 4, 6, 8, 6,
     +			 5, 
     +			 5, 5, 5, 5,
     +                   9, 9,
     +			13, 3,
     +			17, 3 /
C------------------------------------------------------------------------
	iret   = 0
	iptr   = 0
	phenom = ' '
C
C*	Look for the first string which describes phenomenon.
C
	len    = MIN ( 120, lenr ) 
	ifirst = len + 1
	DO i = 1, NPHEN
	    iloc = INDEX ( report ( :len ), phen (i) ( :lenphn (i) ) )
	    IF ( iloc .gt. 0 ) THEN
		cnext = report ( iloc + lenphn (i):iloc + lenphn (i) )
		IF ( ( cnext .eq. ' ' ) .or. ( cnext .eq. '.' ) ) THEN
		    IF ( iloc .lt. ifirst ) THEN
		        ifirst = iloc
		        indx   = i
		    END IF
		END IF
	    END IF
	END DO
C
	IF ( ifirst .le. len ) THEN
	    phenom = phen2 ( indx )
	    iptr = ifirst + lenphn ( indx )
	    IF ( report ( iptr: iptr ) .eq. '.' ) iret = 1
	  ELSE
	    iret = -2
	END IF
C*
	RETURN
	END
