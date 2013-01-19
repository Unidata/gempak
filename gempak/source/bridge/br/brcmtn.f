	FUNCTION BR_CMTN  ( skyc )
C************************************************************************
C* BR_CMTN							 	*
C*									*
C* This function converts a sky cover group in METAR or TAF format into *
C* a floating point number for storage in GEMPAK format:		*
C*									*
C*                   CMTN = BR_CMTN ( skyc )				*
C*									*
C* REAL BR_CMTN  ( SKYC )						*
C*									*
C* Input parameters:							*
C*	SKYC		CHAR*	Character sky cover group		*
C*									*
C* Output parameters:							*
C*	BR_CMTN		REAL	Sky cover number			*
C**									*
C* Log:									*
C* L. Sager/NCEP         3/96                                           *
C* D. Kidwell/NCEP      11/96   Modified mappings to handle partial     *
C*                              obscurations; renamed PR_CMTN to MT_CMTN*
C* K. Tyle/GSC           1/97   Remapped skcod to match existing symbols*
C* D. Kidwell/NCEP       6/97   Replaced ST_LSTR with INDEX             *
C* D. Kidwell/NCEP       4/98   MT_CMTN type REAL; changed CB indicator *
C* D. Kidwell/NCEP       5/98   Flag valid cloud height of zero with '-'*
C* D. Kidwell/NCEP       6/98   Corrected mappings for partial obscurn  *
C* D. Kidwell/NCEP	 9/02	Renamed from MT_CMTN                    *
C************************************************************************
	CHARACTER*(*)	skyc
C*
 	CHARACTER	input*50, skcod (7)*3
C
C*	The following table converts the sky cover symbols to GEMPAK
C*	code figures.
C
	DATA skcod / 'CLR', 'SCT', 'BKN', 'OVC', 'VV', 'FEW', 'SKC' /
C*
C------------------------------------------------------------------------
C
C*	Move input code to new variable and get length.
C
	input = skyc
	leng = INDEX ( input, ' ' ) - 1
C
C*	Check for no sky cover group 
C
	IF ( leng .le. 0 ) THEN
	    BR_CMTN = 0.
	    RETURN
	END IF	
C
C*	Begin by matching the sky cover code with the skcod table. 
C
	rcod = 0.
	i = 1
	DO WHILE (i .lt. 8)
	    iend = 3 
	    IF ( i .eq. 5 ) iend = 2
	    IF ( input (1:iend) .eq. skcod (i)) THEN
	        rcod = i  
	        i = 10 
	    END IF
	    i = i + 1
	END DO
	IF ( rcod .eq. 0. ) THEN
	    BR_CMTN = 0.
	    RETURN
	END IF
	ist = iend + 1
	iend = ist + 2
C
C*	Equate 'SKC' with 'CLR'.
C
	IF ( rcod .eq. 7. )  rcod = 1.
C 
C*	Convert the cloud height from character to integer format.
C
	IF ( ist .lt. leng ) THEN
	    IF ( input (ist:iend) .eq. '///') THEN
	        rcod = rcod + 3.
		IF ( rcod .le. 6. ) rcod = 9.
	      ELSE
	        CALL ST_INTG ( input (ist:iend), intg, iret )
	        IF ( iret .eq. 0 ) THEN
		    rcod = rcod + intg * 10.
C
C*		    Flag a cloud height of zero with '-'.
C
		    IF ( intg .eq. 0 ) rcod = -rcod
		END IF
	    END IF
C
C* 	    Check to see if cloud type was reported.
C  
	    ist = iend + 1
	    IF ( ist .lt. leng ) THEN
	        IF ( input ( ist:ist+1) .eq. 'CB' ) THEN
		    rcod = ABS ( rcod ) + 30000.
	          ELSE IF ( input ( ist:ist+2) .eq. 'TCU' ) THEN 
		    rcod = ABS ( rcod ) + 20000.
	        END IF
	    END IF
	END IF
C
C*	Load the sky cover code figure for GEMPAK.
C
	BR_CMTN = rcod
C*
	RETURN
	END
