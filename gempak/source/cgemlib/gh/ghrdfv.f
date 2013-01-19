	SUBROUTINE GH_RDFV ( advise, nfcst, flat, flon, adate, awind,
     +                       agust, aldex, nknt, iret )
C************************************************************************
C* GH_RDFV 								*
C*									*
C* This subroutine finds the forecasted latitudes and longitudes.  If   *
C* they are not found, the forecast position of the storm is set to     *
C* missing.								*
C*                                                                      *
C* GH_RDFV ( ADVISE, NFCST, FLAT, FLON, ADATE, AWIND, AGUST, ALDEX, 	*
C*	     NKNT, IRET)						*
C*									*
C* Input parameters:							*
C*	ADVISE		CHAR*		Advisory wind string            *
C*	NFCST		INTEGER		Number of fcst times allowed    *
C*									*
C* Output parameters:							*
C*	FLAT(*)		REAL   		Array of forecast latitudes     *
C*	FLON(*)		REAL 		Array of forecast longitudes    *
C*	ADATE (*)	CHAR*		Array of forecast dates		*
C*	AWIND (*)	CHAR*		Array of forecast winds		*
C*	AGUST (*)	CHAR*		Array of forecast wind gusts	*
C*	ALDEX (*)	CHAR*		Array of forecast Post-Tropical	*
C*	NKNT		INTEGER		Number of time periods		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          5/01						*
C* A. Hardy/GSC          6/01	Cleaned up check for 'DIS'	        *
C* A. Hardy/SAIC        10/01	Added check for iwlon		        *
C* A. Hardy/NCEP        10/02   Added space for 'inlat' index checks    *
C* D. Kidwell/NCEP	 3/03	Added argument nfcst; cleaned up        *
C* m.gamazaychikov/SAIC	06/07	Add argument agust and decoded it	*
C* m.gamazaychikov/SAIC	04/08	Add argument aremn and decoded it	*
C* X. Guo/CWS		02/10   Read POST-TROPICAL instead of EXT and   *
C*                                   REMNANT                            *
C* X. Guo/CWS           03/10   Removed argument aremn                  *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'BRIDGE.PRM'
C*
	CHARACTER*(*)	advise, adate(*), awind(*), aldex(*), agust(*)
        REAL            flat(*), flon(*)
C*
	CHARACTER	msg*5
C------------------------------------------------------------------------
	iret = 0
        nknt = 1
C
        CALL ST_LSTR ( advise, ilen, ier ) 
	CALL ST_INCH ( IMISSD, msg, ier )
C
C*	Find the forecast date/time string.
C
        DO i = 1, nfcst
            flat ( i )  = RMISSD
            flon ( i )  = RMISSD
            awind ( i ) = msg
            adate ( i ) = ' '
            aldex ( i ) = ' '
            CALL ST_NOCC ( advise(:ilen), 'VALID', 
     +                     i, ipos, ier )
            IF ( ipos .gt. 0 ) THEN
	        ifvd = INDEX( advise( ipos:ilen), 'Z')
		IF ( ifvd .gt. 0 ) THEN
                    ipos = ipos + ifvd
                    adate(i) = advise( ipos-8:ipos-1)
C
C*	            Find the forecasted latitude and longitude.
C
	            inlat = INDEX( advise( ipos:ipos+6), 'N ')
                    idiss = INDEX( advise( ipos:ipos+6), 'DIS')
                    IF ( idiss .eq. 0 ) THEN
                        IF ( inlat .eq. 0 ) 
     +                       inlat = INDEX( advise( ipos:ipos+6), 'S ')
                    END IF
		  ELSE
		    inlat = 0
		    idiss = 0
		END IF
C
C*              Found either north or south latitude and not the word
C*              'DISSIPATING', so continue. Otherwise, set lat. long.
C*              to missing.
C
                IF ( ( inlat .ne. 0 ) .and. ( idiss .eq. 0 ) ) THEN
                    CALL ST_C2R ( advise(ipos:ipos+inlat-2), 1, 
     +                            flat(i), num, ier )
                    IF ( ( advise(ipos+inlat:ipos+inlat) .eq. 'S' )
     +			 .and. ( ier .ge. 0 ) ) 
     +                   flat(i) = -flat(i)
                    ipos = ipos + inlat
C
C*	            Locate the longitude.
C
	            iwlon = INDEX( advise( ipos:ipos+9), 'W')
                    IF ( iwlon .eq. 0 ) 
     +                  iwlon = INDEX( advise( ipos:ipos+9), 'E')
                    IF ( iwlon .ne. 0 ) THEN
                        CALL ST_C2R ( advise(ipos:ipos+iwlon-2), 1, 
     +                                flon(i), num, ier )
                        IF ((advise(ipos+iwlon-1:ipos+iwlon-1) .eq. 'W')
     +			     .and. ( ier .ge. 0 ) ) 
     +                       flon(i) = -flon(i) 
                        nknt = nknt + 1
                    END IF
		  ELSE
		    iwlon = idiss
                END IF  
C
C*	        Find the forecasted inland/extratropical and remnant.
C
	        inld = INDEX( advise( ipos+iwlon:ipos+iwlon+10), 
     +                                                        'INLAND')
	        iext = INDEX( advise( ipos+iwlon:ipos+iwlon+10),
     +                                                    'POST')
C
                IF ( inld .ne. 0 ) THEN
                    aldex(i) = 'I'
                  ELSE IF ( iext .ne. 0 ) THEN
                    aldex(i) = 'P'
                END IF
C
C*	        Find the forecasted max wind.
C
                ipos2 = ipos+iwlon
	        ikt = INDEX( advise( ipos2:ilen), 'KT')
                IF ( ikt .ne. 0 ) THEN
                    awind (i) = advise(ipos2+ikt-5:ipos2+ikt-2)
                END IF
C
C*	        Find the forecasted wind gusts
C
                ipos2 = ipos+iwlon+ikt
                igust = INDEX( advise( ipos2:ilen), '...GUSTS')
                IF ( igust .ne. 0 ) THEN
                    agust (i) = advise(ipos2+igust+8:ipos2+igust+10)
                END IF
            END IF
        END DO
C*
	RETURN
	END
