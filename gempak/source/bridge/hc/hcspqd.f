	SUBROUTINE HC_SPQD ( advstr, naut, hvwnd, iret )
C************************************************************************
C* HC_SPQD 								*
C*									*
C* This subroutine separates the nautical distance from the quadrant    *
C* directions and then creates a string either with the wind speeds or  *
C* wave heights with their respective quadrants.			*
C*                                                                      *
C* HC_SPQD ( ADVSTR, NAUT, HVWND, IRET )				*
C*									*
C* Input parameters:							*
C*	ADVSTR 		CHAR*		Array of quadrants		*
C*									*
C* Output parameters:							*
C*	NAUT		CHAR*		Quadrant nautical miles string  *
C*	HVWND		LOGICAL		Flag for non-zero wind value    *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          5/00						*
C* D. Kidwell/NCEP	 7/01	Added check for non-zero winds          *
C* A. Hardy/NCEP	10/03   Added decoding JTWC 100, 50 & 34KT winds*
C* D. Kidwell/NCEP	 6/04	Updated comments for JTWC max wind      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	advstr, naut
	LOGICAL		hvwnd
C*
	CHARACTER	first*3, norest*3, sthest*3, souwst*3, 
     +                  nthwst*3, charr(7)*15
	INTEGER		intarr (5)
        LOGICAL         found
C------------------------------------------------------------------------
	iret  = 0
        inm   = 0
        iquad = 0
        first = ' '
        norest = ' '
        nthwst = ' '
        sthest = ' '
        souwst = ' '
	hvwnd = .false.
        CALL ST_LSTR ( advstr, ilen, ier )
C
        iquad = INDEX(advstr(:ilen), 'QUADRANT')
        inm= INDEX(advstr(:ilen), ' NM')
C
        IF ( (iquad .eq. 0 ) .and. ( inm .eq. 0 ) ) THEN
            CALL ST_CLST (advstr(:ilen), ' ', ' ', 7, charr, nback, ier)
            first = advstr(1:2)
C
C*	    Find the quadrant sections.
C
            ine = INDEX(charr(nback-3), 'NE')
            ise = INDEX(charr(nback-2), 'SE ')
            isw = INDEX(charr(nback-1), 'SW')
            inw = INDEX(charr(nback), 'NW')
C
C*	    Determine the beginning of each quadrant number.
C
            ibck = 0
            found = .false.
            DO  ii = 1,ine
                CALL ST_ALNM (charr( nback-3 )(ii:ii), ityp, ier )
                IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                    ibck =  ii
                    norest  = charr( nback-3 )(ibck:ine-1)
                    found = .true.
                END IF
            END DO
	    IF ( .not. found ) norest = '0'
C
            ibck = 0
            found = .false.
            DO  ii = 1,ise
                CALL ST_ALNM (charr( nback-2 )(ii:ii), ityp, ier )
                IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                    ibck =  ii
                    sthest  = charr( nback-2 )(ibck:ise-1)
                    found = .true.
                END IF
            END DO
	    IF ( .not. found ) sthest = '0'
C
            ibck = 0
            found = .false.
            DO  ii = 1,isw
                CALL ST_ALNM (charr( nback-1 )(ii:ii), ityp, ier )
                IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                    ibck =  ii
                    souwst  = charr( nback-1 )(ibck:isw-1)
                    found = .true.
                END IF
            END DO
	    IF ( .not. found ) souwst = '0'
C
            ibck = 0
            found = .false.
            DO  ii = 1,inw
                CALL ST_ALNM (charr( nback )(ii:ii), ityp, ier )
                IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                    ibck =  ii
                    nthwst  = charr( nback )(ibck:inw-1)
                    found = .true.
                END IF
            END DO
	    IF ( .not. found ) nthwst = '0'
          ELSE
C
C*          Have a JTWC message, may have 100 (before 6/1/04) or 64 (on
C*	    or after 6/1/04), 50 and 34 kt wind radii.
C
            first = advstr(1:3)
            IF ( advstr ( 1:3 ) .ne. '100' ) THEN
                first = advstr(2:3)
              ELSE
                first = advstr(1:2)
            END IF
C
C*          Find the nautical miles for each quadrant and remove
C*	    any leading zeros.
C
C*          Northeast quadrant.
C
            ine = INDEX(advstr(:ilen), 'NORTHEAST')
            IF ( ine .gt. 0 ) THEN
                inm = INDEX(advstr(:ine), 'NM ')
                IF ( inm .gt. 0 ) THEN
                    norest  = advstr(inm-4:inm-1)
		    IF (norest(1:1) .eq. '0' ) norest = norest(2:)
                END IF
                ipos = ine
              ELSE
                inm = INDEX(advstr(:ilen), 'NM ')
                 IF ( inm .gt. 0 ) THEN
                    norest  = advstr(inm-4:inm-1)
		    IF (norest(1:1) .eq. '0' ) norest = norest(2:)
                END IF
            END IF
C
C*          Southeast quadrant.
C
            ise = INDEX(advstr(:ilen), 'SOUTHEAST')
            IF ( ise .gt. 0 ) THEN
                inm = INDEX(advstr(ipos:ise), 'NM ')
                IF ( inm .gt. 0 ) THEN
                    sthest  = advstr(ipos+inm-5:ipos+inm-2)
		    IF (sthest(1:1) .eq. '0' ) sthest = sthest(2:)
                END IF
                    ipos = ise 
              ELSE
                inm = INDEX(advstr(:ilen), 'NM ')
                 IF ( inm .gt. 0 ) THEN
                    sthest  = advstr(inm-4:inm-1)
		    IF (sthest(1:1) .eq. '0' ) sthest = sthest(2:)
                END IF
            END IF
C
C*          Southwest quadrant.
C
            isw = INDEX(advstr(:ilen), 'SOUTHWEST')
            IF ( isw .gt. 0 ) THEN
                inm = INDEX(advstr(ipos:), 'NM ')
                IF ( inm .gt. 0 ) THEN
                    souwst  = advstr(ipos+inm-5:ipos+inm-2)
		    IF (souwst(1:1) .eq. '0' ) souwst = souwst(2:)
                END IF
                ipos = isw 
              ELSE
                inm = INDEX(advstr(:ilen), 'NM ')
                 IF ( inm .gt. 0 ) THEN
                    souwst  = advstr(inm-4:inm-1)
		    IF (souwst(1:1) .eq. '0' ) souwst = souwst(2:)
                END IF
            END IF
C
C*          Northwest quadrant.
C
            inw = INDEX(advstr(:ilen), 'NORTHWEST')
            IF ( inw .gt. 0 ) THEN
                inm = INDEX(advstr(ipos:), 'NM ')
                IF ( inm .gt. 0 ) THEN
                    nthwst  = advstr(ipos+inm-5:ipos+inm-2)
		    IF (nthwst(1:1) .eq. '0' ) nthwst = nthwst(2:)
                END IF
              ELSE
                inm = INDEX(advstr(:ilen), 'NM ')
                 IF ( inm .gt. 0 ) THEN
                    nthwst  = advstr(inm-4:inm-1)
		    IF (nthwst(1:1) .eq. '0' ) nthwst = nthwst(2:)
                END IF
            END IF
        END IF  
C
C*	Create the quadrant string.
C
        naut =   first // ' ' // norest // ' ' // sthest // 
     +                    ' ' // souwst // ' ' // nthwst
C
C*	Check for non-zero winds.
C
	CALL ST_C2I ( naut, 5, intarr, num, ier )
	IF ( ier .eq. 0 ) THEN
	    DO ii = 2, num
		IF ( intarr ( ii ) .gt. 0 ) hvwnd = .true.
	    END DO
	END IF
C
C*      Change 64 kt winds and 100kt winds to Max Winds 'MW'.
C
        IF ( (naut(1:2) .eq. '64') .or. ( naut(1:2) .eq. '10') ) THEN
            naut(1:2) = 'MW'
        END IF
C*
	RETURN
	END
