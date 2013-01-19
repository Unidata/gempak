	SUBROUTINE IS_PHEN ( report, lenr, lenlm, phenom, iptr, iret )
C************************************************************************
C* IS_PHEN 								*
C*									*
C* This subroutine decodes a phenomenon from an international sigmet    *
C* report.                                                              *
C*                                                                      *
C* IS_PHEN ( REPORT, LENR, LENLM, PHENOM, IPTR, IRET )                  *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Absolute value is length of	*
C*					string. If < 0, then "other" rpt*
C*	LENLM		INTEGER		Length limit for search		*
C*									*
C* Output parameters:							*
C*	PHENOM		CHAR*  		Sigmet phenomenon               *
C*	IPTR		INTEGER		Pointer to location after phenom*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -3 = valid phenom not found    *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Bug fixes for finding first phenomenon  *
C* D. Kidwell/NCEP	11/99	Expanded scope of search; added TD;     *
C*				simplified                              *
C* F. J. Yen/NCEP	 4/00	Modified entry for volcanic ash		*
C* A. Hardy/GSC          7/00   Added phen type TC                      *
C* A. Hardy/GSC         12/00   Added phen type T.C., S.D. & SD   	*
C* F. J. Yen/NCEP	 5/01	Expanded TS to ')TS ' also		*
C* F. J. Yen/NCEP	 7/01	Expanded TS to include ' TS,'		*
C* A. Hardy/SAIC	 9/01	Added TD phenom type			*
C* F. J. Yen/NCEP	10/01	Expanded TS to include ' TS/'		*
C* F. J. Yen/NCEP	11/01	Added phen types DS, SS, & CB. Expanded	*
C*				SQ, IC, and TB.  Changed len for "other"*
C* F. J. Yen/NCEP	12/01	Expanded phenomena with delimeter string*
C*				phdelm.  Increased len for "other".	*
C* F. J. Yen/NCEP	 1/02	Expanded TB, GR, SQ, and TS.		*
C* F. J. Yen/NCEP	 6/02	Expanded phenonmena with prefixes.	*
C* F. J. Yen/NCEP	 9/03	Continue search for phenomenon when not	* 
C*				a true match.  Added 'TYPHOON'.		*
C* F. J. Yen/NCEP	10/03	Added 'CBS', 'TSRA', 'TSRAGR', 'LLWS',	*
C*			        'WNDSHR'; added prefix '+'.  CSC	* 
C* F. J. Yen/NCEP	12/03	Added 'T. D.' from IS_TC.  Added WV/SHR.*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, phenom
C*
	PARAMETER	( NPHEN = 44 )
C*
	CHARACTER	phen   (NPHEN) * 22, phen2 (NPHEN) * 2
	CHARACTER	phdelm (4) * 1, prfx (5) * 1
	LOGICAL		match
	INTEGER		lenphn (NPHEN)
C*
	DATA		phdelm / ' ', '/', ',', 'S' /
	DATA		prfx   / ' ', ')', '/', '-', '+' /
	DATA		phen /
     +			'TS', 'TSTMS', 'THUNDERSTORM', 'HURCN',
     +			'HURRICANE', 'TYPHOON', 'LSQ', 'SQL TS',
     +			'SQUALL', 'TURB', 'TURBC', 'TURBULENCE',
     +			'CAT', 'ICE', 'FZRA', 'ICING',
     +			'ICG', 'GR', 'HAIL', 'TROPICAL CYCLONE',
     +			'TC', 'T.C.', 'TROPICAL DEPRESSION', 'S.D.',
     +			'SD', 'TD', 'T.D.', 'VOLCANIC ASH',
     +			'VA', 'DS', 'SS', 'FRQ CB',
     +			'EMBD CB', 'EMBED CB', 'ISOL CB', 'OCNL CB',
     +			'CBS', 'MTW', 'TROPICAL STORM', 'LLWS',
     +			'WNDSHR', 'WV/SHR', 'TSRA', 'TSRAGR' /
	DATA		phen2 /
     +			'TS', 'TS', 'TS', 'HU',
     +			'HU', 'HU', 'SQ', 'SQ',
     +			'SQ', 'TB', 'TB', 'TB',
     +			'CT', 'IC', 'IC', 'IC',
     +			'IC', 'GR', 'GR', 'TC',
     +			'TC', 'TC', 'TD', 'TD',
     +			'TD', 'TD', 'TD', 'VA',
     +			'VA', 'DS', 'SS', 'CB',
     +			'CB', 'CB', 'CB', 'CB',
     +			'CB', 'MW', 'TR', 'WS',
     +			'WS', 'WS', 'TS', 'TS' /

	DATA 		lenphn /
     +			 2,  5, 12,  5,
     +			 9,  7,  3,  6,
     +			 6,  4,  5, 10,
     +			 3,  3,  4,  5,
     +			 3,  2,  4, 16,
     +			 2,  4, 19,  4,
     +			 2,  2,  4, 12,
     +			 2,  2,  2,  6,
     +			 7,  8,  7,  7,
     +			 3,  3, 14,  4,
     +			 6,  6,  4,  6 /
C------------------------------------------------------------------------
	iret   = 0
	iptr   = 0
	phenom = ' '
C
C*	Look for the first string which describes phenomenon.
C
	IF ( lenr .lt. 0 ) THEN
	    len    = MIN ( 300, -lenr )
	  ELSE
 	    len    = MIN ( lenlm, lenr ) 
	END IF
	ifirst = len + 1
	DO i = 1, NPHEN
	  match = .false.
	  ibg = 1
	  DO WHILE ( ibg .gt. 0 .and. ibg .le. len - 2 .and.
     +			(.not. match) )
	    iloc = INDEX ( report ( ibg:len ), phen (i) ( :lenphn (i) ) )
	    IF ( iloc .gt. 0 ) THEN
		m = 1
		ibg = iloc + ibg - 1
		DO WHILE ( ibg .lt. ifirst. and. (.not. match) .and.
     +			   (m .le. 4) )
		    locdlm = ibg + lenphn(i)
		    IF ( report ( locdlm:locdlm ) .eq. phdelm (m) ) THEN
			n = 1
			il = ibg - 1
			DO WHILE ( .not. match .and. n .le. 5 )
			    IF ( report ( il:il ) .eq.  prfx (n) ) THEN
		    	        ifirst = ibg
		    	        indx   = i
			        match = .true.
			      ELSE
				n = n + 1
			    END IF
			END DO
		    END IF
		    m = m + 1
		END DO
		ibg = ibg + 1
	      ELSE
	        ibg = 0
	    END IF
	  END DO
	END DO
C
	IF ( ifirst .le. len ) THEN
	    phenom = phen2 ( indx )
	    iptr = ifirst + lenphn ( indx )
	  ELSE
	    iret = -3
	END IF
C*
	RETURN
	END
