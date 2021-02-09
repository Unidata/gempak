        SUBROUTINE GG_WWCR ( date, alias, mndif, iuwtch, iunum, tzone, 
     +                       timiss, tmstrt, tmstp, iret )
C************************************************************************
C* GG_WWCR                                                              *
C*                                                                      *
C* This subroutine returns the array of active thunderstorm and tornados*
C* for either the current WOUs or WCNs                                  *
C*                                                                      *
C* GG_WWCR ( DATE, ALIAS, MNDIF, IUWTCH, IUNUM, TZONE, TIMISS, TMSTRT,  *
C*           TMSTP, IRET )                                              *
C*                                                                      *
C* Input parameters:                                                    *
C*   DATE	    CHAR*          Ending time for watches              *
C*   ALIAS          CHAR*          Alias for WOU or WCN                 *
C*   MNDIF          INTEGER        Minutes difference from end time	*
C*                                                                      *
C* Output parameters:                                                   *
C*   IUWTCH (IUNUM) INTEGER        Unique watch numbers                 *
C*   IUNUM          INTEGER        Number of Unique watchs         	*
C*   TZONE (IUNUM)  CHAR*          Time zones                        	*
C*   TIMISS(IUNUM)  CHAR*          Issue times for each watch      	*
C*   TMSTRT(IUNUM)  CHAR*          Start times for each watch      	*
C*   TMSTP(IUNUM)   CHAR*          Stop times for each watch       	*
C*   IRET           INTEGER                Return code                  *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/NCEP    2/03        Modified from GG_WCUR                   *
C* A. Hardy/NCEP    3/03        Modified for reverse search             *
C* A. Hardy/NCEP    6/03        Increased array size 100 -> NW          *
C* A. Hardy/NCEP    7/03        Added MNDIF to call sequence            *
C* A. Hardy/NCEP    8/03        Added MNDIF to unique watch IF          *
C* A. Hardy/NCEP    1/04        Added tmstp to call sequence            *
C* B. Yin/SAIC      3/04        Changed SS_GTIM to CSS_GTIM             *
C* T. Lee/SAIC      9/04        Replaced FL_TMPL with CTB_DTGET         *
C* A. Hardy/NCEP    11/04       Added calls to ST_RNUL                  *
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* A. Hardy/NCEP    04/05       Added check for cancel messages		*
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* F. J. Yen/NCEP   02/06	Increase length of buffer to max decoded*
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* F. J. Yen/NCEP	 7/08	Increased look back from 6 hrs to 48 hrs*
C************************************************************************
        INCLUDE                'GEMPRM.PRM'
        INCLUDE                'ggcmn.cmn'
C*
        CHARACTER*(*)   date, alias, tzone(*), timiss(*), tmstrt(*),
     +                  tmstp(*)
        INTEGER                iuwtch(*)
C*
        CHARACTER        path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +                   buffer*120, tfile*128, tmstr4*20,
     +                   tmstp4*20, carr(11)*20, tnum*4, 
     +                   temp4*20
C*
        CHARACTER        stime*20, flstrt*160, zone(NW)*3, 
     +                   chiss(NW)*20, fnull*(MXFLSZ)
        INTEGER          itarr(5), jtarr(5), idtarr(5), itype 
        LOGICAL          done, found, finis, rnew, iuflg, gdtim
C-----------------------------------------------------------------------
        iret = 0
        dattim = date
C
C*        Scan the directory for all of the watch data files.
C
        CALL ST_LCUC ( alias, alias, ier )
        CALL ST_NULL ( alias, fnull, nf, ier )
        path  = ' '
        templ = ' '
	CALL CTB_DTGET ( fnull, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
        CALL ST_RNUL ( path, path, lens, ier )
        CALL ST_RNUL ( templ, templ, lens, ier )
        CALL ST_LSTR ( path, lenp, ier )
        nexp   = MXNMFL
        iorder =  1
        CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*        Check for the last file requested by the user.
C
        CALL ST_LCUC ( dattim, dattim, ier )
        itype = 1
        IF  ( ( dattim .eq. 'LAST' ) .or.
     +              ( dattim .eq. 'ALL' ) )  THEN
            CALL CSS_GTIM ( itype, dattm2, ier )
            CALL FL_MNAM ( dattm2, templ, filnam, ier )
          ELSE
            CALL CSS_GTIM ( itype, cdttm, ier )
            CALL TI_STAN ( dattim, cdttm, dattm2, ier )
            IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'TI', ier, dattim, ierr )
                    iret = ier
                    RETURN
            ENDIF                                
            CALL FL_MNAM ( dattm2, templ, filnam, ier )
        END IF
C
C*        Find the earliest file to start searching. For ALL times
C*        go back 10 days, for any other entry for dattim subtract
C*        1 day from the time given.
C
        IF  ( dattim .eq. 'ALL' )  THEN
            minuts = 14400
          ELSE
            minuts = 1440
        END IF
        CALL TI_CTOI ( dattm2, itarr, ier )
        CALL TI_SUBM ( itarr, minuts, jtarr, ier )
        CALL TI_ITOC ( jtarr, stime, ier )
        CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*        Get 4-digit year to compare dates.
C
        CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*        Decode each file until the end time is reached.
C
        nwtch = 0
        wnum ( 1 ) = ' '
        done  = .false.
        ifl = 1
        DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
           IF  ( files(ifl) .gt. filnam )  THEN
              done = .true.
            ELSE
              IF  ( files(ifl) .ge. flstrt )  THEN
                tfile = path(:lenp) // '/' // files(ifl)
                CALL FL_SOPN ( tfile, lunf, ier )
C
                iostat = 0
                DO WHILE  ( iostat .eq. 0 )
                  READ ( lunf, 2, IOSTAT = iostat ) buffer
2                  FORMAT ( A )
                  IF  ( iostat .eq. 0 )  THEN
                    IF  ( buffer(1:1) .eq. '|' )  THEN
                        CALL ST_CLST ( buffer, '|', ' ', 11,
     +                                       carr, num, ier )
                        CALL ST_NUMB ( carr ( 6 ), nmwtch, ier )
                        CALL ST_INCH ( nmwtch, tnum, ier )
                        carr ( 6 ) = tnum
                        CALL ST_NUMB ( carr ( 10 ), jflag, ier )
                        icorr = MOD ( jflag, 4 )
                        jtest = jflag / 4
                        CALL ST_NUMB ( carr ( 11 ), icancl, ier )
                        found = .false.
                        finis = .false.
                        rnew  = .false.
                        ii    = 1
                        jw    = 0
C
                              IF ( ( carr ( 2 )(:3) .eq. 'TOR' ) .or. 
     +                       ( carr ( 2 )(:3) .eq. 'TSM' ) .or.
     +                       ( carr ( 2 )(:3) .eq. 'SVR' ) ) THEN
                            IF ( .not. found ) THEN
C
C*                                Add new watch to the data arrays.
C
                                rnew  = .true.
                                temp4 = carr ( 5 )
                            END IF
                        END IF

C
C*                        See if this may actually be a correction
C*                        to a watch number.
                        IF ( icorr .eq. 1 ) THEN
                            DO ii = 1, nwtch
                                IF ( ( carr(2)(:2) .eq. wtype(ii))  
     +                            .and. ( carr(4) .eq. timstr(ii)) 
     +                            .and. ( carr(5) .eq. timstp(ii)) )
     +                                 icnum = ii
                            END DO
                         END IF 
                        IF ( rnew ) THEN
C
C*                            Add new watch information.
C                          
                            nwtch = nwtch + 1
                            jw    = nwtch
                            wtype  (jw) = carr (2)(:2)
                            chiss  (jw) = carr (3)
                            timstr (jw) = carr (4)
                            timstp (jw) = temp4
                            wnum   (jw) = carr (6)(:4)
                            zone   (jw) = carr (9)(:3)
			    npt    (jw) = 0
                            itest  (jw) = jtest
                        END IF
                      END IF
                  END IF
                END DO
C
                CALL FL_CLOS ( lunf, ier )
C
              END IF
C
          END IF
          ifl = ifl + 1
        END DO
C
        CALL TI_CTOI ( dattm4, idtarr, ier )
        CALL TI_ADDM ( idtarr, mndif, idtarr, ier )
        CALL TI_ITOC ( idtarr, dattm4, ier )
C
        ij = 1
        iunum = 0
        iuflg = .false.
        DO ii = nwtch, 1, -1
           gdtim = .false.
C
           
           CALL TI_DTM4 ( timstp ( ii ), tmstp4, ier )
           CALL TI_DTM4 ( timstr ( ii ), tmstr4, ier )
C
            CALL TI_CTOI ( tmstr4, idtarr, ier )
            CALL TI_ADDM ( idtarr, mndif, idtarr, ier )
            CALL TI_ITOC ( idtarr, tmstr4, ier )
            CALL TI_CTOI ( tmstp4, idtarr, ier )
            CALL TI_ITOC ( idtarr, tmstp4, ier )
C
            IF ( (dattim .eq. 'ALL' ) .or.
     +          ( ( tmstp4 .ge. dattm4 ) .and.
     +            ( tmstr4 .le. dattm4 ) ) ) THEN
C
C*             Check time differences.
C
               CALL TI_DIFF ( dattim, chiss(ii), nmis, ier )
               CALL TI_DIFF ( dattim, timstr(ii), nmst, ier )
               CALL TI_DIFF ( dattim, timstp(ii), nmsp, ier )
C
               IF ( nmis .ge. 0 ) THEN
                   IF ( nmst .ge. 0 ) THEN
                       IF ( nmsp .le. mndif )  THEN
                           gdtim = .true.
                         ELSE
                           CALL TI_DIFF ( timstr(ii), timstp(ii),
     +                                    nm, ier )
                           IF ( nm .eq. 0 ) THEN
                               gdtim = .true.
C
C*			       This check sets a limit on how far
C*				to go back to check for cancel messages.
C*                              Currently it is set for 48 hours.
C
                               IF ( (mndif .eq. 0  ) .and.
     +                            ( nmsp .le. 2880  ) ) THEN
                                     nmsp = mndif
                               END IF
                             ELSE
                               gdtim = .false.
                           END IF
                       END IF
                     ELSE
                       gdtim = .false.
                   END IF
                 ELSE
                   gdtim = .false.
               END IF 
C
C*	       Find unique watch numbers.
C
               IF ( ( ( wtype (ii) .eq. 'TS') .or.
     +              ( wtype (ii) .eq. 'TO') .or.
     +              ( wtype (ii) .eq. 'SV') ) .and. ( gdtim) 
     +              .and. ( nmsp .le. mndif ) ) THEN
                   CALL ST_NUMB (wnum (ii), iwnm, ier )
                   DO jj = 1, iunum
                       if ( iuwtch (jj) .eq. iwnm ) THEN
                           iuflg = .true.
                       END IF
                   END DO
C
                   IF ( .not. iuflg ) THEN
                       iunum = iunum + 1
                       iuwtch ( iunum ) = iwnm 
                       tzone(iunum)  = zone(ii)
                       timiss(iunum) = chiss(ii)
                       tmstrt(iunum) = timstr(ii)
                       tmstp(iunum)  = timstp(ii)
                   END IF 
                   iuflg = .false.
               END IF
               ij = ij + 1
            END IF
        END DO
C*
	RETURN
	END
