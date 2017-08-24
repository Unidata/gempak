	SUBROUTINE GH_PUHD  ( record, stype, sname, sadvnum, 
     +                        stime, iret )
C************************************************************************
C* GH_PUHD								*
C*									*
C* The report header consists of the lines from the date line and above *
C* in the public advisory report.  Returns the storm name, type,        *
C* advisory number and valid time from the header.			*
C*									*
C* GH_PUHD  ( RECORD, STYPE, SNAME, SADVNUM, STIME, IRET )              *
C*									*
C* Input parameters:							*
C*	RECORD		CHAR*		Public /Advisory Report		*
C*									*
C* Output parameters:							*
C*	STYPE		CHAR*		Storm type			*
C*	SNAME		CHAR*		Storm name			*
C*	SADVNUM         CHAR*           Advisory report number          *
C*	STIME		CHAR*		Valid time			*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = no time found		*
C**									*
C* Log:									*
C* S. Gilbert/NCEP	 1/06	MOdified from GH_RDHD                   *
C* m.gamazaychikov/SAIC	04/06	Skipped storm ID in the advisory header	*
C* S. Gilbert/NCEP	 7/06	Added check for Honolulu HI             *
C* S. Gilbert/NCEP       8/06   Remove extraneous info from advisory no.*
C* M. Sardi/NHC          9/16   Decoded 'POST-TROPICAL' and 'REMNANTS'  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	record, stype, sname, sadvnum, stime
C*
	CHARACTER	string*128, charr(12)*30, timlcl*60
        CHARACTER       chhmm*30, cmerid*30, ctz*30, cdow*30, cmon*30
        CHARACTER       cdom*30, cyear*30
C------------------------------------------------------------------------
	iret    = 0
	stype   = ' '
	sname   = ' '
	sadvnum = ' '
	stime   = ' '
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
        IF ( charr(1) .eq. 'HURRICANE' ) THEN
             stype   = 'HU'
             sname   = charr(2)
             sadvnum  = charr(5)
           IF ( charr(3) .eq. 'INTERMEDIATE' ) sadvnum  = charr(6)
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
            IF ( charr(4) .eq. 'INTERMEDIATE' ) sadvnum  = charr(7)
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
            IF ( charr(4) .eq. 'INTERMEDIATE' ) sadvnum  = charr(7)
        END IF
C
        IF ( charr(1) .eq. 'POST-TROPICAL' ) THEN
             stype   = 'PT'
             sname   = charr(3)
             sadvnum  = charr(6)
           IF ( charr(4) .eq. 'INTERMEDIATE' ) sadvnum  = charr(7)
        END IF
C
        IF ( charr(1) .eq. 'REMNANTS' ) THEN
             stype   = 'DB'
             sname   = charr(3)
             sadvnum  = charr(6)
           IF ( charr(4) .eq. 'INTERMEDIATE' ) sadvnum  = charr(7)
        END IF
C
C*	Check if advisory number is followed by extraneous text
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
C*	Look for Date/Time info
C
        ipos  = INDEX ( record(ifore:lenrec) , 'MIAMI FL' ) 
        IF ( ipos .eq. 0 ) THEN
           ipos  = INDEX ( record(ifore:lenrec) , 'HONOLULU HI' ) 
        ENDIF
	ifore = ifore + ipos + 8
	string = record ( ifore:ifore + 79 )
        CALL ST_CLST  ( string, ' ', ' ', 12, charr, n, ier )
C
C*      Check if the first word is the storm identifier
C
        i1st = 1
        CALL ST_LSTR( charr(1), lenid, ier )
        IF ( ( charr(1)(1:2) .eq. 'AL' .OR. 
     +         charr(1)(1:2) .eq. 'EP' .OR.
     +         charr(1)(1:2) .eq. 'CP' ) .AND. lenid .eq. 8 ) 
     +     i1st = 2
C
C*      Save the time string. , check if time is valid. 
C
        IF ( charr(i1st) .eq. 'NOON' .OR. 
     +       charr(i1st) .eq. 'MIDNIGHT' ) THEN
            chhmm = '1200'
            IF ( charr(i1st) .eq. 'NOON' ) cmerid = 'PM'
            IF ( charr(i1st) .eq. 'MIDNIGHT' ) cmerid = 'AM'
            ctz =   charr(i1st+1)
            cdow =  charr(i1st+2)
            cmon =  charr(i1st+3)
            cdom =  charr(i1st+4)
            cyear = charr(i1st+5)
        ELSE
            CALL ST_LSTR( charr(i1st), lenc, ier )
            IF ( lenc .le. 2 ) THEN
               chhmm = charr(i1st)(1:lenc) // '00'
            ELSE 
               chhmm = charr(i1st)
            ENDIF 
            cmerid = charr(i1st+1)
            ctz =    charr(i1st+2)
            cdow =   charr(i1st+3)
            cmon =   charr(i1st+4)
            cdom =   charr(i1st+5)
            cyear =  charr(i1st+6)
        ENDIF
C
C*      Create local date/time string and convert to GEMPAK 
C*      date/time in UTC.
C
	CALL ST_LSTR ( chhmm, len1, ier )
	CALL ST_LSTR ( cmerid, len2, ier )
	CALL ST_LSTR ( ctz, len3, ier )
	CALL ST_LSTR ( cdow, len4, ier )
	CALL ST_LSTR ( cmon, len5, ier )
	CALL ST_LSTR ( cdom, len6, ier )
	CALL ST_LSTR ( cyear, len7, ier )
        timlcl = chhmm(1:len1) // ' ' // cmerid(1:len2) // ' ' // 
     +           ctz(1:len3) // ' ' // cdow(1:len4) // ' ' // 
     +           cmon(1:len5) // ' ' // cdom(1:len6) // ' ' // 
     +           cyear(1:len7)
        CALL TI_LOCL ( timlcl, stime, ier )
        IF ( ier .ne. 0 )  iret = -1
C
	RETURN
	END
