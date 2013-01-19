        SUBROUTINE GH_GBKL ( ibkpts, iarea, icnt, lntp, 
     +			     nparts, npln, lat, lon, iret )
C************************************************************************
C* GH_GBKL                                                              *
C*                                                                      *
C* This subroutine returns arrays of breakpoints lats and lons given	*
C* the ENDpoints of breakpoint segment.					*
C*                                                                      *
C* GH_GBKL ( IBKPTS, IAREA, ICNT, LNTP, NPARTS, NPLN, LAT, LON, IRET )	*
C*                                                                      *
C* Input parameters:                                                    *
C*      IBKPTS (*)	INTEGER         Array of breakpoint sequence 	*
C*					numbers 			*
C*      IAREA(*)      	INTEGER         Array of geographic area 	*
C*					designators      		*
C*      ICNT(*)      	INTEGER         Number of breakpoints 		*
C*      LNTP            CHAR*		Breakpoint type			*
C* Output parameters:                                                   *
C*      NPARTS      	INTEGER         Number of parts (lntp=list only)*
C*      NPLN      	INTEGER         Number of points in line	* 
C*	LAT (*)		FLOAT		Array of lats in line segment	*
C*	LON (*)		FLOAT		Array of lons in line segment	*
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/SAIC	05/07	Created					*
C* m.gamazaychikov/SAIC	07/07	Clean up prolog				*
C************************************************************************
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'ghcmn.cmn'
C*
        INTEGER         ibkpts (*), iarea(*), npln(20)
        REAL            lat (20,*), lon(20,*)
        CHARACTER*(*)   lntp
C*
        REAL		rlat (MAXBK), rlon (MAXBK)
        LOGICAL         done
C------------------------------------------------------------------------
        iret = 0
        jj = 1
        inpri = 0
        
        DO jj = 1, 20
           npln(jj) = 1
        END DO
        DO ii = 1, MAXBK
         rlat (ii) = RMISSD
         rlon (ii) = RMISSD
        END DO
        IF ( icnt .eq. 2 .and. lntp .eq. 'pair' ) THEN
        itot = 0
C
C*      For pairs of points
C
           IF ( iarea (1) .eq. iarea (2) ) THEN
             ipass = 1
            ELSE
             ipass = 2
           END IF
C
           IF ( ipass .eq. 2 ) THEN
            IF ( ( ( iarea ( 1 ) .eq. IUSGEC ) .and.
     +             ( iarea ( 2 ) .eq. IMXCSA ) ) .or.
     +           ( ( iarea ( 1 ) .eq. IMXCSA ) .and.
     +             ( iarea ( 2 ) .eq. IUSGEC ) ) ) THEN
             ELSE
                RETURN
            END IF
           END IF
C
           IF ( iarea ( 1 ) .eq. IKEYS ) THEN
             isupp = 99
            ELSE
             isupp = 19
           END IF
C
           DO ip = 1, ipass
            ibeg = indxbk ( iarea (ip) )
            iend = indxbk ( iarea (ip)  + 1 ) - 1
            IF ( ipass .eq. 1) THEN
                ilo  = MIN0 ( ibkpts (1), ibkpts (2) )
                ihi  = MAX0 ( ibkpts (1), ibkpts (2) )
                inpri = 1
                incrmt = 1
              ELSE
                itotl = ibkpts (2) + ibkpts (1)
C
C*              line is drawn from mexico to us 
C
                IF ( iarea (1) .eq. 2 ) THEN
                  IF ( ip .eq. 1 ) THEN
C
C*                  First point-set:
C*                    starting bkpnt - given mexico pnt
C*                    ending   bkpnt - us/mex brdr  pnt
C
                    ilo  = ibkpts (1)
                    ihi  = 1
                    inpri = 1
                    incrmt = 1
                   ELSE IF ( ip .eq. 2 ) THEN
C
C*                  Second point-set:
C*                    starting bkpnt - first us pnt
C*                    ending   bkpnt - given us pnt
C
                    ilo  = 1
                    ihi  = ibkpts (2)
c                    inpri = inpri + ihi1 - ilo1 + 2
c                    incrmt = -1
c                    inpri = inpri +1
                    incrmt = 1
                  END IF
C
C*                line is drawn form us to mexico
C
                 ELSE IF ( iarea (1) .eq. 1 ) THEN
                  IF ( ip .eq. 1 ) THEN
C
C*                  First point-set:
C*                    starting bkpnt - given us pnt
C*                    ending   bkpnt - first us pnt
C
                    ilo  = ibkpts (1)
                    ihi  = 1
                    inpri = itotl
                    incrmt = -1
                   ELSE IF ( ip .eq. 2 ) THEN
C
C*                  Second point-set:
C*                    starting bkpnt - us/mex brdr  pnt
C*                    ending   bkpnt - given mexico pnt
C
                    ilo  = 1
                    ihi  = ibkpts (2)
                    incrmt = -1
                  END IF
                END IF
            END IF
C
C*          Find the beginning and ending indices in the breakpoint
C*          sequence array.
C
            indxb = 0
            indxe = 0
            DO kk = ibeg, iend
                IF ( ibkseq ( kk ) .eq. ilo ) indxb = kk
                IF ( ibkseq ( kk ) .eq. ihi ) indxe = kk
            END DO
            IF ( ( indxb * indxe ) .gt. 0 ) THEN
C
C*              Check the priority number of a breakpoint.
C
                istep = 1
                IF ( indxe .lt. indxb) istep = -1
                DO inp = indxb, indxe, istep
                    IF ( ( ibkpri ( inp ) .ne. isupp ) .or.
     +                   ( ( ibkpri ( inp ) .eq. isupp ) .and.
     +                     ( ( inp .eq. indxb ) .or.
     +                       ( inp .eq. indxe ) ) ) ) THEN
                        rlat ( inpri ) = bklat ( inp )
                        rlon ( inpri ) = bklon ( inp )
                        inpri = inpri + incrmt
                        itot = itot + 1
                    END IF
                END DO
                npri = inpri - 1
            END IF
C
C*          Check for inclusion of Matagorda Island for U.S.
C*          These two points MUST be the last entries in the "U. S.
C*          sounds, bays, etc" section of the breakpoint plotting table.
C
        END DO
C
C*      For ordered pairs of points
C
         ELSE IF ( icnt .eq. 2 .and. lntp .eq. 'ordr' ) THEN
           ibeg = indxbk ( iarea(1) )
           iend = indxbk ( iarea(1) + 1 ) - 1
           last = nbkpts ( iarea(1) ) + 1
C
C*         Get the breakpoint pairs for the current type.
C
           ilo  = ibkpts ( 1 )
           ihi  = ibkpts ( 2 )
           IF ( ilo .eq. ihi ) THEN
C
C*           This is the convention used to define the entire island
C
             ilo = 1
             ihi = last
           END IF
           ipass = 1
           itot = 0
           IF ( ilo .gt. ihi ) THEN
             isav = ihi
             ihi  = last
             ipass = 2
           END IF
           DO ii = 1, ipass
C
C*           Find the beginning and ending indices in the
C*           breakpoint sequence array.
C
             indxb = 0
             indxe = 0
             DO kk = ibeg, iend
                IF ( ibkseq ( kk ) .eq. ilo ) indxb = kk
                IF ( ibkseq ( kk ) .eq. ihi ) indxe = kk
             END DO
             IF ( ihi .eq. last ) indxe = iend
             IF ( ( indxb * indxe ) .gt. 0 ) THEN
                np = indxe - indxb + 1
C
C*              Draw the coastline connecting the points in the
C*              pair.
C
                DO inp = indxb, indxe
                   rlat ( itot+1 ) = bklat ( inp )
                   rlon ( itot+1 ) = bklon ( inp )
                   itot = itot + 1
                END DO
             END IF
C
             IF ( ipass .eq. 2 ) THEN
                 ilo = 1
                 ihi = isav
             END IF
            END DO
C
C*      For lists of points
C
         ELSE IF ( icnt .ne. 2 .and. lntp .eq. 'list' ) THEN
           ibeg = indxbk ( iarea (1) )
           iend = indxbk ( iarea (1) + 1 ) - 1
           itot = 0
           isegmn = 1
C
C*      Loop over the breakpoint list for the current type.
C
        DO ii = 1, icnt
            larea = ibkpts ( ii )
C
C*          Find the beginning and ending indices in the
C*          breakpoint sequence array.  Breakpoints in the list
C*          may have more than one line segment associated with
C*          them.
C
            DO kk = ibeg, iend
                IF ( ibkseq ( kk ) .eq. larea ) THEN
                    indxb = kk + 1
                    indxe = 0
                    ll    = indxb + 1
                    done  = .false.
                    DO WHILE ( .not. done )
                        ipen = MOD ( ibkpri ( ll ), 10 )
                        IF ( ( ipen .ne. 5 ) .or.
     +                       ( ll .eq. iend ) ) THEN
                            IF ( ( indxe .ne. 0 ) .or.
     +                           ( ll .eq. iend ) ) THEN
                                IF ( indxe .ne. 0 ) THEN
                                    np = indxe - indxb + 1
                                ELSE
                                    np = iend - indxb
                                END IF
                                IF ( ( ll .eq. iend ) .and.
     +                               ( ipen .eq. 5 ) )  np = np + 1
C
C*                              Draw the coastline for this list
C*                              item.
C                              
                                iinp = 1
                                DO inp = indxb, indxb+np-1
                                  lat (isegmn,iinp) = bklat ( inp )
                                  lon (isegmn, iinp) = bklon ( inp )
                                  npln(isegmn) = npln(isegmn) + 1
                                  iinp = iinp + 1
                                  itot = itot + 1
                                END DO
                                isegmn = isegmn +1
                                IF ( ipen .eq. 1 ) THEN
                                    indxb = ll
                                ELSE
                                    done = .true.
                                END IF
                            END IF
                        ELSE
                            indxe = ll
                        END IF
                        ll = ll + 1
                        IF ( ll .gt. iend ) done = .true.
                    END DO
                END IF
            END DO
        END DO
        nparts = isegmn-1
        END IF
      
        IF ( lntp .eq. 'pair' .OR. lntp .eq. 'ordr') THEN
          npline = 1
          DO inp = 1, itot
           IF ( ( rlat (inp)  .ne. RMISSD )  .and.
     +          ( rlat (inp)  .ne. RMISSD ) ) THEN
                lat (1, npline) = rlat (inp)
                lon (1, npline) = rlon (inp)
                npline= npline + 1
           END IF
          END DO
          npln (1)  = npline-1
          nparts = 1
         ELSE IF ( lntp .eq. 'list') THEN
            DO ii = 1, nparts
              npln(ii) = npln(ii)-1
            END DO
        END IF
C*
        RETURN
        END
