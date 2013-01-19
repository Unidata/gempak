	SUBROUTINE RD_GDTE ( segmnt, lens, ispnt, idtarr, idp, iret )
C************************************************************************
C* RD_GDTE								*
C*									*
C* This subroutine gets the first date in the forecast date line,	*
C* decodes it and converts it to an integer time array.			*
C*									*
C* RD_GDTE  ( SEGMNT, LENS, ISPNT, IDTARR, IDP, IRET )			*
C*									*
C* Input parameters:							*
C*      SEGMNT		CHAR*		Bulletin segment		*
C*      LENS		INTEGER		Length of segment		*
C*									*
C* Input and output parameters:						*
C*	ISPNT		INTEGER		Segment pointer			*
C*									*
C* Output parameters:							*
C*      IDTARR (5)	INTEGER		First integer date array	*
C*	IDP		INTEGER		Position of 1st date in dateline*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					  -1 = date/time not found	*
C*					  -2 = invalid month		*
C*					  -3 = invalid day		*
C*					  -4 = invalid year		*
C*					  -5 = invalid date string	*
C*					  -6 = invalid segment		*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02	If no issue time for segment, use issue	*
C*				time for bulletin (negative ispnt)	*
C************************************************************************
	INCLUDE         'GEMPRM.PRM'
C*
	CHARACTER*(*)	segmnt
	INTEGER		idtarr (*) 
C*
        CHARACTER       stlist(7)*5, carr(30)*4, cmon*2, cday*2
        INTEGER         ilens(7) 
        DATA            stlist / ' SUN ', ' MON ', ' TUE ', ' WED ',
     +				 ' THU ', ' FRI ', ' SAT ' /
        DATA            ilens / 7*5 /
C------------------------------------------------------------------------
	iret = 0
	idp = 0
C
C*	Search for date line.  First find day of the week
C
	IF ( ispnt .lt. 0 ) THEN
	    ispnt = -ispnt
	    jserln = 500
	  ELSE
	    jserln = 80
	END IF
	ilpnt = MIN ( ispnt + jserln, lens )
    	CALL ST_NXTS ( segmnt, ispnt, ilpnt, stlist, ilens, 7, isdp,
     +		istrg, iret )
	IF ( iret .ne. 0 ) THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Determine position of date from start of line
C
	jj = isdp
	DO WHILE ( jj .ge. ispnt .and. idp .eq. 0 )
	    IF ( segmnt ( jj:jj ) .eq. CHLF ) THEN
		linst = jj 
		idp = isdp - linst + 1
	    END IF
	    jj = jj - 1
	END DO
	IF ( idp .eq. 0 ) THEN
	    iret = -5
	    RETURN
	END IF
C
C*	Find date string.  First remove any blanks preceding date.
C
	jn = MIN ( isdp + 15, lens )
	jj = isdp + 4
	jst = 0
	DO WHILE ( jj .le. jn .and. jst .eq. 0 )
	    IF ( segmnt ( jj:jj ) .ne. ' ' ) THEN
		jst = jj
	    END IF
	    jj = jj + 1
	END DO
	IF ( jst .eq. 0 ) THEN
	    iret = -5
	    RETURN
	END IF
	jn = MIN ( jst + 20, lens ) 
	jj = jst
	lst = 0
	DO WHILE ( jj .le. jn .and. lst .eq. 0 )
	    IF ( segmnt ( jj:jj ) .eq. ' ' ) THEN
		lst = jj - 1
	    END IF
	    jj = jj + 1
	END DO
	IF ( lst .eq. 0 ) THEN
	    iret = -5
	    RETURN
	END IF
	CALL ST_CLST ( segmnt ( jst:lst ), '/', '99', 3, carr, num, ier )
	IF (  ( ier .ne. 0 ) .or. ( num .lt. 3 ) )  RETURN
C
C*	First string is the month
C
	CALL ST_LSTR ( carr ( 1 ), len, ier )
	IF ( ( len .eq. 1 ) .or. ( len .eq. 2 ) ) THEN
	    cmon = carr ( 1 ) ( :len )
	    CALL ST_NUMB ( cmon, imon, ier )
	    IF ( ier .ne. 0 ) iret = -2
	  ELSE
	    iret = -2
	END IF
	IF ( iret .eq. -2 ) RETURN
C
C*	Second string is the date
C
        CALL ST_LSTR ( carr ( 2 ), len , ier )
        IF ( ( len .eq. 1 ) .or. ( len .eq. 2 ) ) THEN
            cday = carr ( 2 ) ( :len )
            CALL ST_NUMB ( cday, iday, ier )
            IF ( ier .ne. 0 ) iret = -3
          ELSE
            iret = -3
        END IF
        IF ( iret .eq. -3 )  RETURN
C
C*	Third string is the year
C
        CALL ST_LSTR ( carr ( 3 ), len , ier )
        IF ( len .eq. 2 ) THEN
            CALL ST_NUMB ( carr (3) (:2), iyear, ier )
            IF ( ier .ne. 0 ) iret = -4
          ELSE
            iret = -4
        END IF
        IF ( iret .eq. -4 )  RETURN
C
	CALL TI_YY24 ( iyear, iyear, iret )
	idtarr (1) = iyear
	idtarr (2) = imon
	idtarr (3) = iday
	idtarr (4) = 99
	idtarr (5) = 99
	ispn = INDEX ( segmnt ( isdp:lens ),  CHCR )
	IF ( ispn .eq. 0 ) THEN
	    iret = -6
	  ELSE
	    ispnt = ispn + isdp - 1
	END IF
C*
	RETURN
	END
