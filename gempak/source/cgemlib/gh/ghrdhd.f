	SUBROUTINE GH_RDHD  ( record, stype, sname, origc, sadvnum, 
     +                        socnstm, stime, iedtm, iret )
C************************************************************************
C* GH_RDHD								*
C*									*
C* The report header consists of the lines from the date line and above *
C* in the forecast/advisory report. 					*
C*									*
C* GH_RDHD  ( RECORD, STYPE, SNAME, SADVNUM, SOCNSTM, STIME, IEDTM,     *
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	RECORD		CHAR*		Forecast/Advisory Report	*
C*									*
C* Output parameters:							*
C*	STYPE		CHAR*		Storm type			*
C*	SNAME		CHAR*		Storm name			*
C*	ORIGC		CHAR*		Issuing center			*
C*	SADVNUM         CHAR*           Advisory report number          *
C*	SOCNSTM         CHAR*           Ocean storm number          	*
C*	STIME		CHAR*		Valid time (YYMMDD/HHMM)	*
C*      IEDTM		INTEGER		Report index pointer position   *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no time found		*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/01	Modified from HC_GHDR			*
C* D. Kidwell/NCEP	 6/01	Cleaned up prologue             	*
C* A. Hardy/SAIC	 8/01	Added trop. depression check		*
C* A. Hardy/NCEP	 9/02	Added Subtropical check			*
C* D. Kidwell/NCEP	 3/03	Rewrote for new & old header formats    *
C* D. Kidwell/NCEP	 4/03	Allowed for 4-digit year; initialization*
C* A. Hardy/NCEP	 8/03	Added second check for date/time string *
C* S. Gilbert/NCEP	 5/06	Added check for date/time string in new *
C*                              format.  Parse to make GEMPAK date/time *
C* m.gamazayhikov/SAIC  06/06   Allowed storm names beginning with CP   *
C* S. Gilbert/NCEP	 7/06	Decoding the issuing center             *
C* S. Gilbert/NCEP	 8/06	Remove extraneous info from advisory no.*
C* m.gamazaychikov/SAIC	12/07 	Change start point of search for ctime	*
C* A. Krautkramer/NHC   09/11   Decoded 'POST-TROPICAL' and 'REMNATS'   *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	record, stype, sname, sadvnum, socnstm, stime
	CHARACTER*(*)	origc
C*
	CHARACTER	string*128, charr(12)*30, inittm*5, ctime*30
	CHARACTER	zone*3, month*2, mlist(12)*3
        LOGICAL         good
C*
        DATA            mlist / 'JAN', 'FEB', 'MAR', 'APR',
     +                          'MAY', 'JUN', 'JUL', 'AUG',
     +                          'SEP', 'OCT', 'NOV', 'DEC' /
C------------------------------------------------------------------------
	iret    = 0
	stype   = ' '
	sname   = ' '
	sadvnum = ' '
	socnstm = ' '
	stime   = ' '
        origc   = 'TPC'
C
C*	Find the length of the header string.
C
	CALL ST_LSTR ( record, lenrec, ier )
C
C*	Look for the forecast/advisory line.
C
        ifore1 = INDEX ( record ( :lenrec ), 'NWS' ) 
        ifore2 = INDEX ( record ( :lenrec ), 'NATIONAL' ) 
	IF ( ifore1 .eq. 0 ) THEN
	    ifore = ifore2
	  ELSE IF ( ifore2 .eq. 0 ) THEN
	    ifore = ifore1
	  ELSE
	    ifore  = MIN0 ( ifore1, ifore2 )
	END IF
        iddhh  = INDEX ( record(:ifore) , 'SUBTROPICAL' ) 
	IF ( iddhh .eq. 0 ) 
     +          iddhh = INDEX ( record(:ifore) , 'POST-TROPICAL' )
        IF ( iddhh .eq. 0 ) 
     +          iddhh = INDEX ( record(:ifore) , 'TROPICAL' ) 
        IF ( iddhh .eq. 0 ) 
     +          iddhh = INDEX ( record(:ifore) , 'HURRICANE' )
        IF ( iddhh .eq. 0 ) 
     +          iddhh = INDEX ( record(:ifore) , 'REMNANTS' )
        string = record(iddhh:ifore-1)
 	CALL ST_LSTR ( string, lens, ier )
        CALL ST_CLST  (string(:lens), ' ', ' ', 7, charr, n, ier )
C
C*	Break the forecast/advisory string.
C
	IF  (charr(1) .eq. 'REMNANTS') THEN
		stype = 'DB'
		sname = charr(3)
		sadvnum = charr(6)
		IF ( charr(4) .eq. 'SPECIAL' ) sadvnum = charr(7)
	END IF
	IF  (charr(1) .eq. 'POST-TROPICAL') THEN
		stype = 'PT'
		sname = charr(3)
		sadvnum = charr(6)
		IF ( charr(4) .eq. 'SPECIAL' ) sadvnum = charr(7)
	END IF

        IF ( charr(1) .eq. 'HURRICANE' ) THEN
             stype   = 'HU'
             sname   = charr(2)
             sadvnum  = charr(5)
           IF ( charr(3) .eq. 'SPECIAL' ) sadvnum  = charr(6)
	END IF
C
        IF ( charr(1) .eq. 'TROPICAL' ) THEN
            IF ( charr(2) .eq. 'STORM' ) THEN
                stype   = 'TS'
              ELSE
                stype   = 'TD'
            END IF
            sname   = charr(3)
            sadvnum  = charr(6)
            IF ( charr(4) .eq. 'SPECIAL' ) sadvnum  = charr(7)
        END IF
C
        IF ( charr(1) .eq. 'SUBTROPICAL' ) THEN
            IF ( charr(2) .eq. 'STORM' ) THEN
                stype   = 'SS'
              ELSE
                stype   = 'SD'
            END IF
            sname   = charr(3)
            sadvnum  = charr(6)
            IF ( charr(4) .eq. 'SPECIAL' ) sadvnum  = charr(7)
        END IF
C
C*      Check if advisory number is followed by extraneous text
C*      as would occur with a Corrected advisory
C
        CALL ST_LSTR( sadvnum, lenadv, ier )
        ii = 1
        DO  WHILE ( ii .le. lenadv )
            CALL ST_ALNM ( sadvnum(ii:ii), ityp, ier )
            IF ( ityp .ne. 1 ) THEN
                IF ( sadvnum(ii:ii) .ne. 'a' .AND.
     +               sadvnum(ii:ii) .ne. 'b' .AND.
     +               sadvnum(ii:ii) .ne. 'A' .AND.
     +               sadvnum(ii:ii) .ne. 'B' ) THEN

                    sadvnum(ii:lenadv) =  ' '
                    ii = lenadv

                END IF
            END IF
            ii = ii + 1
        END DO
C
C*	Look for the ocean location.  Advisory can be issued by Miami
C*	(Atlantic storm or Pacific storm) or Honolulu (Pacific storm).
C
	inittm  = ' '
	string = record ( ifore:ifore + 79 )
        CALL ST_CLST  ( string, ' ', ' ', 12, charr, n, ier )
	DO ii = 1, n - 1
	    IF ( ( charr ( ii ) ( :2 ) .eq. 'AL' ) .or. 
     +           ( charr ( ii ) ( :2 ) .eq. 'CP' ) .or. 
     +		 ( charr ( ii ) ( :2 ) .eq. 'EP' ) ) THEN
		inittm  = charr ( ii + 1 ) 
		zone  = charr ( ii + 2 ) 
		CALL ST_LSTR ( charr (ii), lens, ier )
		IF ( lens .eq. 6 ) THEN
		    socnstm = charr ( ii )
		  ELSE IF ( lens .eq. 8 ) THEN
		    socnstm = charr (ii) ( 1:4 ) // charr (ii) ( 7:8 )
		END IF
                IF ( (charr (ii - 1) .EQ. 'HI' ) .AND.
     +               (charr (ii - 2) .EQ. 'HONOLULU' ) ) origc = 'CPHC' 
	    END IF
	END DO
C
C*      Save the time string. First, check if time is valid. 
C
	IF ( inittm .ne. ' ' ) THEN
            IF ( inittm(5:5) .eq. 'Z' ) THEN
                ii = 4
                lent = 20
                good = .true.
                DO WHILE ( ( ii .ne. 0 ) .and. ( good ) )
                    CALL ST_ALNM ( inittm (ii:ii), itype, ier ) 
                    IF ( itype .ne. 1 ) good = .false.
                    ii = ii - 1
                END DO
            ELSE IF ( inittm(5:5) .eq. ' ' .AND. zone .eq. 'UTC' ) THEN
                ii = 4
                lent = 23
                good = .true.
                DO WHILE ( ( ii .ne. 0 ) .and. ( good ) )
                    CALL ST_ALNM ( inittm (ii:ii), itype, ier ) 
                    IF ( itype .ne. 1 ) good = .false.
                    ii = ii - 1
                END DO
            ELSE
                good = .false.
            END IF
C
C*	    If time is not valid, search for it farther in the string.
C
            IF ( good ) THEN
	        ipos  = INDEX ( record ( ifore:lenrec ), inittm )
                ctime = record(ifore-1+ipos:ifore+ipos+lent)
                iedtm = ipos+lent
              ELSE
	        iss  = INDEX ( record ( iddhh:lenrec ), inittm )
	        ihvz  = INDEX ( record ( iss:lenrec ), 'Z ')
                inittm = record ( iss+ihvz-5:iss+ihvz)
                IF ( ihvz .gt. 0 ) THEN
                    ii = 2
                    good = .true.
                    DO WHILE ( ( ii .ne. 6 ) .and. ( good ) )
                        CALL ST_ALNM ( record(ihvz+iss-ii:ihvz+iss-ii), 
     +					   itype, ier ) 
                        IF ( itype .ne. 1 ) good = .false.
                        ii = ii + 1
                        IF ( ii .eq. 6 ) THEN
                            IF ( record(ihvz+iss-ii:ihvz+iss-ii) 
     +                            .ne. ' ' ) THEN
                                good = .false.
                            END IF
                        END IF
                    END DO
                END IF
C
C*		Found 'Z ' later in string, set times.
C
                IF ( good ) THEN
	            ipos  = INDEX ( record ( ifore:lenrec ), inittm )
                    ctime = record(ifore-1+ipos:ifore+ipos+20)
                    iedtm = ipos+20
                  ELSE
                    iret = -1
	            iedtm = ifore + 80
                END IF
            END IF
C
C*          Convert the time string to a GEMPAK time.
C
            IF ( good ) THEN
               CALL ST_CLST  ( ctime, ' ', ' ', 6, charr, n, ier )
               stime = ' '
               stime ( 1:2 )  = charr ( n ) ( 3:4 )
               stime ( 5:6 )  = charr ( n-1 ) ( 1:2 )
               stime ( 7:7 )  = '/'
               stime ( 8:11 ) = charr ( 1 ) ( 1:4 )
               CALL ST_FIND ( charr ( n-2 ) ( 1:3 ), mlist, 12, ipos,
     +                         ier )
               CALL ST_INCH ( ipos, month, ier )
               IF ( ipos .lt. 10 ) month = '0' // month
               stime ( 3:4 )  = month
            END IF
	  ELSE    
            iret = -1
	    iedtm = ifore + 80
	END IF
C*
	RETURN
	END
