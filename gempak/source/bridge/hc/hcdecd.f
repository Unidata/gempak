	SUBROUTINE HC_DECD ( bultin, lenbul, nfcst, temptim, rlat, rlon, 
     +                       posnm, direc, speed, presmin, sixty, 
     +                       fifty, thirty, seaft, flat, flon, icor,
     +                       fstype, f34kt, f50kt, f64kt, iexflg, iret )
C************************************************************************
C* HC_DECD 								*
C*									*
C* This subroutine decodes a single WTNT or WTPZ forecast/advisory      *
C* report.      							*
C*                                                                      *
C* HC_DECD ( BULTIN, LENBUL, NFCST, TEMPTIM, RLAT, RLON, POSNM, DIREC,  *
C*           SPEED, PRESMIN, SIXTY, FIFTY, THIRTY, SEAFT, FLAT, FLON,  	*
C*           ICOR, FSTYPE, F34KT, F50KT, F64KT, IEXFLG, IRET )		*
C*									*
C* Input parameters:							*
C*	BULTIN		CHAR*		WTNT bulletin			*
C*	LENBUL		INTEGER		Length of bulletin              *
C*	NFCST		INTEGER		Number of forecast times        *
C*									*
C* Output parameters:							*
C*	TEMPTIM		CHAR*		Valid location time		*
C*	RLAT 		REAL		Latitude of points              *
C*	RLON 		REAL		Longitude of points             *
C*	POSNM		CHAR*		Position accuracy in NM		*
C*	DIREC		CHAR*		Direction of storm movement(deg)*
C*	SPEED 		CHAR*		Speed of movement (knots)	*
C*	PRESMIN		CHAR*		Minimum central pressure (mb)   *
C*	SIXTY		CHAR*		64 kt current quadrant string	*
C*	FIFTY		CHAR*		50 kt current quadrant string	*
C*	THIRTY		CHAR*		34 kt current quadrant string	*
C*	SEAFT		CHAR*		12 ft seas quadrant string	*
C*	FLAT(*) 	REAL		Array of forecasted latitudes   *
C*	FLON(*)		REAL		Array of forecasted longitudes  *
C*	ICOR		INTEGER		Correction flag			*
C*	FSTYPE(*)	CHAR*		Forecasted storm type           *
C*	F34KT(*)	CHAR*		34kt string at all fcst times   *
C*	F50KT(*)	CHAR*		50kt string at all fcst times   *
C*	F64KT(*)	CHAR*		64kt string at all fcst times   *
C*      IEXFLG		INTEGER		Flag for extratropical storm	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 9/99						*
C* A. Hardy/GSC		10/99	Fixed for stationary dir/speed		*
C* A. Hardy/GSC		10/99	Check for missing mb & accuracy 	*
C* A. Hardy/GSC          5/00 	Get current/forecast positions		*
C* A. Hardy/GSC          7/00 	Changed key word for finding init. lat. *
C* A. Hardy/GSC          8/00 	Added search for correction 		*
C* D. Kidwell/NCEP       7/01 	Added fcst storm type and 34kt fcst     *
C*			        radii at 24, 48, 72 hrs; cleaned up     *
C* A. Hardy/SAIC         8/01 	Added search for extratropical storm	*
C* D. Kidwell/NCEP	 3/02	Added f50kt & f64kt strings             *
C* D. Kidwell/NCEP	 2/03	Added argument nfcst                    *
C* A. Hardy/NCEP 	10/03	Modified to decode JTWC hurricane msgs	*
C* A. Hardy/NCEP 	11/03	Modified locating the current long.	*
C* m.gamazaychikov/SAIC 03/05	Modified locating the CENTER in bulletin*
C*				Added check for leading R w/1 digit wdir*
C* m.gamazaychikov/SAIC 07/05	Added check on iwp for JTWC bulletins	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
        INCLUDE         'BRIDGE.PRM'
C*
	CHARACTER*(*)	bultin, temptim, posnm, direc, speed, presmin,
     +                  sixty, fifty, thirty, seaft, fstype(*),
     +			f34kt(*), f50kt(*), f64kt(*)
	REAL   		flat(*), flon(*)
C*
	CHARACTER	advise*(DCMXBF), dum*1
        LOGICAL         done
C------------------------------------------------------------------------
	iret = -2
        iexflg = 0
C
C*	Remove unprintables and control characters from bulletin and
C*	ensure upper case.
C
	CALL ST_UNPR ( bultin ( :lenbul ), lenbul, advise, lenw, ier )
	CALL ST_LCUC ( advise, advise, ier )
C
C*	Search for correction indication.
C
	icind = INDEX( advise( :lenw), '..COR') 
        IF ( icind .ne. 0 ) THEN
	    icor = 1
	  ELSE
	    icor = 0
	END IF
C
C*	Search for the center location of storm.
C
        iwp = INDEX( advise(:lenw), 'WARNING POSITION:')
        IF ( iwp .eq. 0) THEN
C
C*         Finding ipos for NHC/CPHC bulletins
C
           done  = .false.
           inear = 1
           ipnear = 0
           DO WHILE ( .not. done )
              CALL ST_NOCC (advise( :lenw), 'NEAR', inear, ipnear, ier)
              IF ( ier .ge. 0 ) THEN
                 CALL ST_NOCC ( advise( ipnear-25:ipnear), 'CENTER',
     +                          1, ipcnt, ier2 )
                 IF ( ier2 .ge. 0 ) THEN
                    done = .true.
                  ELSE
                    inear = inear + 1
                 END IF
              END IF
              IF ( ipnear .eq. 0 ) THEN
                 done = .true.
                 ipnear = 1
              END IF
           END DO
           ipos = ipnear
         ELSE 
C
C*         Finding ipos for JTWC bulletins
C
           ipos = INDEX ( advise (:lenw), 'NEAR')
           IF ( ipos .eq. 0) ipos = 1
        END IF
C
C*	Locate the latitude of the center.
C
        CALL ST_NOCC ( advise(ipos:lenw), '.', 1, ipos1, ier)
        CALL ST_C2R ( advise(ipos+5:ipos+ipos1), 1, rlat, num, iret )
        IF ( num .lt. 0 ) rlat = RMISSD
        IF ( advise(ipos+ipos1+1:ipos+ipos1+1) .eq. 'S' ) 
     +      rlat = -rlat
        ipos = ipos+ipos1+1
C
C*      Check for the check sum number after the lat/lon designator.
C*      This is in the old format JTWC messages.
C
	CALL ST_ALNM ( advise(ipos+ipos1+1:ipos+ipos1+1), itype, ier )
        IF ( itype .eq. 1 ) ipos = ipos + 2
C
C*	Locate the longitude of the center. Find the space before the
C*      longitude then find the decimal point in the longitude.
C
        CALL ST_NOCC ( advise(ipos:lenw), '.', 1, ipos2, ier)
        CALL ST_NOCC ( advise(ipos:lenw), ' ', 1, ipos3, ier)
        CALL ST_C2R ( advise(ipos+ipos3:ipos+ipos2), 1, rlon, num, iret)
C
        IF ( num .lt. 0 ) rlon = RMISSD
        IF ( advise(ipos+ipos2+1:ipos+ipos2+1) .eq. 'W' ) 
     +       rlon = -rlon
        CALL ST_ALNM ( advise(ipos+ipos2+1:ipos+ipos2+1), itype, ier )
        IF ( itype .eq. 1 ) ipos = ipos + 2
        
C
C*	Search for the date time string. If a JTWC, insert '/' between
C*      the day and time.
C
        IF ( iwp .ne. 0 ) THEN
           temptim = advise(iwp+18:iwp+19) // '/' // 
     +					advise(iwp+20:iwp+23)
	  ELSE
	    iposdat = INDEX( advise(ipos:lenw), 'AT') 
            CALL ST_NOCC ( advise(ipos:lenw), 'Z', 1, ipos3, ier)
            temptim = advise(ipos+iposdat+2:ipos+ipos3-2)
        END IF
C
C*	Search for the accuracy position.
C
	iposnm = INDEX( advise(:lenw), 'ACCURATE WITHIN') 
        IF ( iposnm .ne. 0 ) THEN
            posnm = advise(iposnm+16:iposnm+17)
          ELSE 
	    iposnm = INDEX( advise(:lenw), 'ACCURATE TO WITHIN') 
            IF ( iposnm .ne. 0 ) THEN
                posnm = advise(iposnm+20:iposnm+22)
              ELSE
                posnm = ' '
            END IF
        END IF
C
C*	Search for the movement and speed.
C
        CALL ST_NOCC ( advise(ipos:lenw), 'DEGREES', 1, ipos1, ier)
        IF ( ipos1 .ne. 0 ) THEN
            direc = advise(ipos+ipos1-5:ipos+ipos1)
            IF ( direc(1:1) .eq. 'R' ) direc (1:1) = ' '
	    iposnm = INDEX( advise(:lenw), 'DEGREES') 
            CALL ST_NOCC ( advise(iposnm:lenw), 'KT', 1, ipos1, ier)
            speed = advise(iposnm+ipos1-4:iposnm+ipos1)
          ELSE   
            CALL ST_NOCC ( advise(ipos:lenw), 'STATIONARY', 1, 
     +                     ipos1, ier)
            IF ( ipos1 .ne. 0 ) THEN
                direc = ' '
                speed = '0'
            END IF
        END IF
C
C*	Search for the central pressure.
C
	irmk = INDEX( advise(iposnm:lenw), 'REMARKS')
	iposprs = INDEX( advise(iposnm:lenw), 'PRESSURE')
        islp = INDEX( advise(iposnm:lenw), 'MB ')
C
        IF ( (iposprs .ne. 0 ) .and. ( irmk .eq. 0 ) )THEN
            ipres = iposnm+iposprs+7
            presmin = advise(ipres:ipres+5)
C
C*        Search for sea level pressure information (if any) in the
C*        JTWC 'REMARKS' section.
C
          ELSE IF ( (iposprs .ne. 0 ) .and. ( irmk .ne. 0 ) )THEN
            IF ( ( islp .lt. iposprs ) .and. ( islp .ne. 0  ) ) THEN
	        imb = iposnm + islp
                presmin = advise(imb-6:imb-3)
              ELSE
	        imb = iposnm+iposprs
                presmin = advise(imb+11:imb+14)
            END IF
            CALL ST_RMBL ( presmin, presmin, lenp, ier )
            DO ii = 1, lenp 
                CALL ST_ALNM ( presmin(ii:ii), ityp, ier )
                IF ( ityp .ne. 1 ) THEN
                    presmin = 'XXX'
                END IF
            END DO
          ELSE 
            presmin = 'XXX'
        END IF
        CALL ST_RMBL ( presmin, presmin, lenp, ier )
C
C*	Search for the current quadrant information.
C
	igust = INDEX( advise(ipos:lenw), 'GUSTS') 
        ipos2 = ipos+igust
	irep = INDEX( advise(ipos2:lenw), 'REPEAT') 
        CALL ST_LSTR ( advise(ipos2:ipos2+irep), ilen, ier)
        CALL HC_QUAD ( advise(ipos2:ipos2+irep), sixty, fifty,
     +                 thirty, seaft, dum, ier )
C
C*      Search for 'EXTRATROPICAL' before forecast information. 
C*      If it is found, set a flag.
C
        iext = INDEX( advise( :ipos1+ipos2+irep), 'EXTRATROPICAL')
        IF ( iext .gt. 0 ) iexflg = 1
C
C*	Search for valid forecast positions, storm type, and 34kt, 50kt,
C*	and 64kt forecast wind radii at forecast times.
C
        CALL HC_FVLD ( advise(ipos2+irep:lenbul), nfcst, flat, flon,
     +		       fstype, f34kt, f50kt, f64kt, ier)
C*
	RETURN
	END
