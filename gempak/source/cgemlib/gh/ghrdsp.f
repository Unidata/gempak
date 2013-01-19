	SUBROUTINE GH_RDSP ( advstr, naut, iret )
C************************************************************************
C* GH_RDSP 								*
C*									*
C* This subroutine separates the nautical distance from the quadrant    *
C* directions and then creates a string either with the winds speed or  *
C* wave heights with their respective quadrants.			*
C*                                                                      *
C* GH_RDSP ( ADVSTR, NAUT, IRET )					*
C*									*
C* Input parameters:							*
C*	ADVSTR 		CHAR*		Array of quadrants		*
C*									*
C* Output parameters:							*
C*	NAUT		CHAR*		Quadrant nautical miles string  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          5/00						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	advstr, naut
C*
	CHARACTER	first*2, norest*3, sthest*3, souwst*3, 
     +                  nthwst*3, charr(7)*15
        LOGICAL         found
C------------------------------------------------------------------------
	iret = 0

        CALL ST_LSTR ( advstr, ilen, iret)
C
        CALL ST_CLST ( advstr(:ilen), ' ', ' ', 7, charr, nback, iret)
        first = advstr(1:2)
C
C*	Find the quadrant sections.
C
        ine = INDEX(charr(nback-3), 'NE')
        ise = INDEX(charr(nback-2), 'SE ')
        isw = INDEX(charr(nback-1), 'SW')
        inw = INDEX(charr(nback), 'NW')
C
C*	Determine the beginning of each quadrant number.
C
        ibck = 0
        found = .false.
        DO  ii = 1,ine
            CALL ST_ALNM (charr( nback-3 )(ii:ii), ityp, ier)
            IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                ibck =  ii
                norest  = charr( nback-3 )(ibck:ine-1)
                found = .true.
            END IF
        END DO
C
        ibck = 0
        found = .false.
        DO  ii = 1,ise
            CALL ST_ALNM (charr( nback-2 )(ii:ii), ityp, ier)
            IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                ibck =  ii
                sthest  = charr( nback-2 )(ibck:ise-1)
                found = .true.
            END IF
        END DO
C
        ibck = 0
        found = .false.
        DO  ii = 1,isw
            CALL ST_ALNM (charr( nback-1 )(ii:ii), ityp, ier)
            IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                ibck =  ii
                souwst  = charr( nback-1 )(ibck:isw-1)
                found = .true.
            END IF
        END DO
C
        ibck = 0
        found = .false.
        DO  ii = 1,inw
            CALL ST_ALNM (charr( nback )(ii:ii), ityp, ier)
            IF ( (ityp .eq. 1 ) .and. ( .not. found ) )THEN
                ibck =  ii
                nthwst  = charr( nback )(ibck:inw-1)
                found = .true.
            END IF
        END DO
C
C*	Create the quadrant string.
C
        naut =   first // ' ' // norest // ' ' // sthest // 
     +                    ' ' // souwst // ' ' // nthwst
C
	RETURN
	END
