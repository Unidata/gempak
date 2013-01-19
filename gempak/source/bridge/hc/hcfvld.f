	SUBROUTINE HC_FVLD ( advise, nfcst, flat, flon, fstype, f34kt,
     +			     f50kt, f64kt, iret)
C************************************************************************
C* HC_FVLD 								*
C*									*
C* This subroutine finds the forecasted latitudes and longitudes, and   *
C* the 34, 50 and the 64 knot or 100 kt forecast wind radii.  It also   *
C* determines the forecast storm type.  If they are not found, the 	*
C* forecast position of the storm and the 34, 50 and 64 kt or 100kt	*
C* strings are set to missing, and the forecast storm type is set blank.*
C*                                                                      *
C* HC_FVLD ( ADVISE, NFCST, FLAT, FLON, FSTYPE, F34KT, F50KT, F64KT,    *
C*           IRET )	                                                *
C*									*
C* Input parameters:							*
C*	ADVISE		CHAR*		Advisory wind string            *
C*	NFCST		INTEGER		Number of forecast times        *
C*									*
C* Output parameters:							*
C*	FLAT(*)		REAL   		Array of forecast latitudes     *
C*	FLON(*)		REAL 		Array of forecast longitudes    *
C*	FSTYPE(*)	CHAR*		Forecast storm type             *
C*	F34KT(*)	CHAR*		34kt string at all fcst times   *
C*	F50KT(*)	CHAR*		50kt string at all fcst times   *
C*	F64KT(*)	CHAR*		64kt string at all fcst times   *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          5/00						*
C* D. Kidwell/NCEP       7/01	Added fcst storm type and 34kt fcst     *
C*				radii at 24, 48, 72 hrs; cleaned up     *
C* A. Hardy/SAIC	 8/01   Added 34kt fcst radii at 12 and 36 hrs  *
C* D. Kidwell/NCEP       2/02	Added check for REMNANT LOW             *
C* D. Kidwell/NCEP       3/02	Added f50kt & f64kt strings             *
C* A. Hardy/NCEP         9/02   Corrected long. check for forecast pos. *
C* A. Hardy/NCEP        10/02   Added space for 'inlat' index checks	*
C* D. Kidwell/NCEP       2/03	Added argument nfcst                    *
C* A. Hardy/NCEP	10/03	Modified to decode JTWC messages	*
C* D. Kidwell/NCEP       6/04	Updated comments for JTWC max wind      *
C* m.gamazaychikov/SAIC	07/05	Added check to get the correct JTWC lats*
C* m.gamazaychikov/SAIC	04/08	Add code to decode EXTRATROPICAL string *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'BRIDGE.PRM'
C*
	CHARACTER*(*)	advise, fstype (*), f34kt (*), f50kt (*),
     +			f64kt (*)
        REAL            flat(*), flon(*)
C*
	CHARACTER       advstr*(DCMXBF), t34*50, t50*50, t64*50, dum*1
	LOGICAL		west
C------------------------------------------------------------------------
	iret = 0
	itcm = 0
        iext = 0 
        advstr = advise
        CALL ST_LSTR( advstr, ilen, iret) 
C
C*	Find the forecast latitude and longitude, storm type and wind 
C*	radii at all forecast times.
C
        DO i = 1, nfcst
C
            CALL ST_NOCC ( advise(:ilen), 'VALID AT:', 
     +                     i, ipos, iret)
            IF ( ipos .eq. 0 ) THEN
                CALL ST_NOCC ( advise(:ilen), 'VALID', 
     +                        i, ipos, iret)
            END IF
            ityp  = 0 
	    inlat = 0
	    idiss = 0
            idash = 0
	    IF ( ipos .gt. 0 ) THEN
	        ifvd = INDEX( advise( ipos:ilen), 'Z')
                idash = INDEX( advise( ipos:ilen), '---')
                IF ( idash .gt. 0 ) THEN
                    CALL ST_ALNM ( advise (ipos+ifvd:ipos+ifvd), 
     +                             ityp, ier )
                    IF ( ityp .eq. 1 ) THEN
                       idash = 6
                     ELSE
                       idash = 5
                    END IF
                END IF
		IF ( ifvd .gt. 0 ) THEN
C
C*                  When searching for the latitude and longitude
C*		    designators, a check for the next character of it
C*	 	    is necessary to skip over the check sum position
C*                  found in the old format JTWC reports.
C
                    ipos = ipos+ifvd + idash
	            innn = INDEX( advise( ipos:ipos+6), 'N')
                    IF ( innn .ne. 0 ) THEN
                        IF (advise(ipos+innn:ipos+innn) 
     +                                            .eq. ' ' ) THEN
                            inlat = innn
                          ELSE
                            CALL ST_ALNM (advise(ipos+7:ipos+7), 
     +                                                    ityp, ier)
                            inlat = innn
                        END IF
                      ELSE
                        innn = INDEX(advise(ipos:ipos+6), 'S')
                        IF ( innn .ne. 0 ) THEN
                            CALL ST_ALNM (advise(ipos+6:ipos+6), 
     +                                                     ityp, ier)
                            IF ( (advise(ipos+6:ipos+6) .eq. ' ' ) .or.
     +                           (ityp .eq. 1 ) )THEN
                                inlat = innn
                            END IF
                        END IF
                    END IF
                    idiss = INDEX( advise( ipos:ipos+6), 'DIS')
	            ireml = MIN ( ipos+40, ilen )
                    irem  = INDEX (advise ( ipos:ireml ), 'REMNANT LOW')
                    iext=INDEX(advise ( ipos:ireml+210),'EXTRATROPICAL')
	        END IF
	    END IF
C
C*          Found either north or south latitude and not the word
C*          'DISSIPATING', so continue.  Otherwise, set lat. long.
C*          and 34, 50 and 64kt wind radii to missing, and storm type
C*	    to blank.
C
            IF ( ( inlat .ne. 0 ) .and. ( idiss .eq. 0 ) ) THEN
                CALL ST_C2R ( advise(ipos:ipos+inlat-2), 1, flat(i), 
     +                        num, iret )
                IF ( advise(ipos+inlat:ipos+inlat) .eq. 'S' ) 
     +               flat(i) = -flat(i)
                ipos = ipos + inlat
                IF ( ityp .eq. 1 ) THEN
                    ipos = ipos + 2
                END IF
C
C*	        Locate the longitude.
C
                west = .true.
	        iwlon = INDEX( advise( ipos:ipos+14), 'W')
                IF ( iwlon .eq. 0 ) west = .false.
                ielon = INDEX( advise( ipos:ipos+14), 'E')
                IF ( ( ielon .ne. 0 ) .and. ((ielon .lt. iwlon ) .or.
     +                ( .not. west ) ) ) THEN
                    iwlon = ielon
                END IF
                CALL ST_C2R ( advise(ipos:ipos+iwlon-2), 1, 
     +                        flon(i), num, iret )
                IF ( advise(ipos+iwlon-1:ipos+iwlon-1) .eq. 'W' )
     +               flon(i) = -flon(i)
		ipos = ipos + iwlon
		ivalid = INDEX ( advise ( ipos:ilen ), 'VALID' )
		IF ( ivalid .eq. 0 ) ivalid = ilen - ipos
C
C*		Get the forecast wind radii.
C
		CALL HC_QUAD ( advise ( ipos:ipos + ivalid ), t64,
     +			       t50, t34, dum, fstype ( i ), ier )
		ikt = INDEX ( advise ( ipos:ilen ), 'KT...GUSTS' )
                IF ( ikt .eq. 0 ) 
     +		ikt = INDEX ( advise ( ipos:ilen ), 'KT, GUSTS' )
                IF  (ikt .gt. 0 ) THEN
C
C*		    Determine forecast storm type based on gust speed.
C
                    CALL ST_NUMB ( advise (ipos+ikt-5:ipos+ikt-2),
     +                             iwnd, ier )
                    IF ( iwnd .ge. 64 ) THEN
                        fstype ( i ) = 'HU'
                      ELSE IF ( (iwnd .lt. 64 ) .and. 
     +                               ( iwnd .ge. 34 ) ) THEN
                        fstype ( i ) = 'TS'
                      ELSE IF ( iwnd .lt. 34 ) THEN
                        fstype ( i ) = 'TD'
                    END IF
                END IF
		f34kt ( i ) = t34
		f50kt ( i ) = t50
		f64kt ( i ) = t64
		IF ( ( fstype ( i ) .eq. 'TD' ) .and. ( irem .gt. 0 ) )
     +		       fstype ( i ) = 'RL'
		IF ( iext .gt. 0 ) fstype ( i ) = 'EX'
              ELSE
C
C*              Use 'MW' to signify Maximum Wind. This represents either
C*		the 64 kt winds (for TPC, and JTWC as of 6/1/04) or 
C*		100 kt winds (for JTWC before 6/1/04). 
C
                flat(i)   = -9999.
                flon(i)   = -9999.
	        fstype(i) = ' '
		f34kt (i) = '34 -9999 -9999 -9999 -9999'
		f50kt (i) = '50 -9999 -9999 -9999 -9999'
		f64kt (i) = 'MW -9999 -9999 -9999 -9999'
            END IF  
        END DO
C
	RETURN
	END
