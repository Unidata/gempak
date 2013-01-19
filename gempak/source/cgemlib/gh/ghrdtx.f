	SUBROUTINE GH_RDTX  ( record, stype, adnum, sadvnm, iedtm, 
     +                        iwflag, iret )
C************************************************************************
C* GH_RDTX								*
C*									*
C* This subroutine finds if 'WATCH' or 'WARN' is in the forecast     	*
C* advisory text paragraph.  The current text paragraph is compared	*
C* to the previous advisory paragraph, if one existed, to see if 	*
C* changes were made to the watch/warning area(s).			*
C*									*
C* GH_RDTX  ( RECORD, STYPE, ADNUM, SADVNM, IEDTM, IWFLAG, IRET )	*
C*									*
C* Input parameters:							*
C*	RECORD		CHAR*		Forecast/advisory report	*
C*	STYPE		CHAR*		Storm type			*
C*      ADNUM		CHAR*		Number of requested advisory    *
C*      SADVNM		CHAR*		Number of this advisory         *
C*      IEDTM		INTEGER		Report index pointer position   *
C*									*
C* Output parameters:							*
C*      IWFLAG		INTEGER		Flag for watch/warning wording	*
C*					  1 = different from prev. one  *
C*					  0 = no watch/warning found	*
C*					 -1 = same as previous one  	*
C*					 -2 = earlier advisory; unused  *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* A. Hardy/GSC		 5/01						*
C* A. Hardy/GSC		 6/01	Added second check for 'CENTER'	        *
C* D. Kidwell/NCEP	 4/02	String size 500 -> 2000; documentation  *
C* A. Hardy/NCEP         9/02   Added 'SS' & 'SD' 			*
C* D. Kidwell/NCEP	 3/03	Removed ST_UNPR call                    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	record, stype, adnum, sadvnm 
C*
	CHARACTER	txt*2000, partxt*2000, advise*5000
        SAVE		partxt
C------------------------------------------------------------------------
	iret  = 0
        ifnd = 0
        iwflag = -2
C
C*	Find the length of the rest of the record.
C
	CALL ST_LSTR ( record(iedtm:), lenrec, ier )
	advise = record ( iedtm:lenrec )
	CALL ST_LSTR ( advise, lenw, ier )
C
C*	Determine the end of the watch/warn paragraph.
C
        ipos = INDEX( advise(:lenw), 'CENTER LOCATED')
        IF (ipos .eq. 0 )
     +        ipos = INDEX( advise(:lenw), 'CENTER')
        ipos2 = ipos
	CALL ST_LSTR ( stype, len, ier )
        IF ( stype(:len) .eq. 'HU' ) THEN
            ipos = ipos - 12
          ELSE IF ( stype(:len) .eq. 'TS' ) THEN
            ipos = ipos - 17
          ELSE IF ( stype(:len) .eq. 'TD' ) THEN
            ipos = ipos - 22
          ELSE IF ( stype(:len) .eq. 'SS' ) THEN
            ipos = ipos - 20
          ELSE IF ( stype(:len) .eq. 'SD' ) THEN
            ipos = ipos - 25
        END IF
        IF ( ipos .lt. 2 ) THEN
            partxt = ' '
            iwflag = 0
            RETURN
        END IF	
        txt = advise(2:ipos)
	CALL ST_LSTR ( txt, len, ier )
C
C*	See if this advisory is the one just before the current one.
C
        CALL ST_NUMB (adnum, iadv, iret )
        CALL ST_NUMB (sadvnm, isnm, iret )
C
C*      Save paragraph if it is the one for the advisory just before 
C*	the current (requested) advisory.
C
        IF ( isnm .eq. (iadv-1) ) THEN
            partxt = txt(:len)
        END IF
C
C*	If this is the paragraph for the current (requested) advisory,
C*	search for WATCH or WARN in text and, if found, compare to the 
C*	paragraph for the immediately preceding advisory.
C
        IF ( isnm .eq. iadv ) THEN
            ifnd = INDEX( txt(:len), 'WATCH')
            IF ( ifnd .eq. 0 )  ifnd = INDEX( txt(:len), 'WARN')
            IF ( ifnd .ne. 0 ) THEN
                IF ( txt .eq. partxt ) THEN
                    iwflag = -1
                  ELSE
                    iwflag = 1
                END IF
              ELSE
                iwflag = 0
            END IF
        END IF
C*
	RETURN
	END
