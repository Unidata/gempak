	SUBROUTINE CS_CBND ( string, mxp, type, intsy, dist, npt, 
     +                       rlat, rlon, iptr, iret ) 
C************************************************************************
C* CS_CBND  								*
C*									*
C* This subroutine gets the type, intensity, distance and bounds for a  *
C* convective sigmet report.  						*
C*                                                                      *
C* CS_CBND ( STRING, MXP, TYPE, INTSY, DIST, NPT, RLAT, RLON, IPTR,     *
C*           IRET )    							*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*	MXP		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	TYPE		CHAR*		Report type                     *
C*	INTSY		CHAR*		Level of storm intensity	*
C*	DIST		CHAR*		Storm distance/diameter		*
C*	NPT		INTEGER		Number of bounds points         *
C*	RLAT(*)		REAL		Latitudes of bounds points      *
C*	RLON(*)		REAL		Longitudes of bounds points     *
C*	IPTR		INTEGER		Pointer following last bounds pt*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = no valid convec. sig. type*
C*					 -6 = no bounds given           *
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02	Created					*
C* A. Hardy/NCEP         8/02   Fixed insty, IS single station decoding *
C* A. Hardy/NCEP         5/04   Corrected DCCIG -> DCCSIG		*
C* J. Lewis/AWC		 8/07   Change search for 'TS' to ' TS '	*
C* L. Hinson/AWC         9/13   Fix to correctly Process VCNTY remarks  *
C*                               for ISOL TS                            *
C* L. Hinson/AWC         5/15   Fixed decoding issues with ISOL TS when *
C*                                movement is 'MOV LTL' or states line  *
C*                                contains 'CSTL WTRS' or               *
C*                                'AND CSTL WTRS'                       *
C*                              Fixed decoding issue with LINE TS when  *
C*                                when movement is 'MOV LTL'            *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, type, intsy, dist
	REAL		rlat (*), rlon (*)
C*
	PARAMETER	( NWDMAX = 120, IFMT = 5112 )
C*
	CHARACTER	carr (NWDMAX)*10, sep*1, tarr (NWDMAX)*10
        CHARACTER	loc*3, ddbb*6, tmpstr*20, locnam*10, tmpch*20
        LOGICAL		done, found
C------------------------------------------------------------------------
	iret    = 0
        iend    = 0
        icon    = 0
        ikts    = 0
	npt     = 0
        ifrom   = 0
        type    = ' '
        dist    = ' '
        intsy   = ' '
        sep     = '-'
        done    = .false.
        found   = .false.
        nwdmx   = NWDMAX
C
	DO ii = 1, mxp
	    rlat ( ii ) = RMISSD
	    rlon ( ii ) = RMISSD
	END DO
C
C*	Find the beginning of the 'FROM' line.
C 
        ifrom = INDEX ( string, 'FROM' ) 
        istrt = ifrom + 5 
C
C*      Roughly find the end of the convective sigmet.
C
        icon = INDEX ( string , 'TS ' ) 
        IF ( icon .eq. 0 ) THEN
            CALL ST_LSTR ( string , lens, ier ) 
            icon = lens
        END IF 
C
C*	Determine type of convective sigmet.
C        
        iend = INDEX ( string (:icon ),'AREA') 
        IF ( iend .ne. 0 ) THEN
            type = 'AR'
          ELSE
            iend = INDEX ( string (:icon),'LINE') 
            IF ( iend .ne. 0 ) THEN
                type = 'LN'
              ELSE
                iend = INDEX(string(:icon),'ISOL') 
                IF ( iend .ne. 0 ) THEN
                    type = 'IS'
                  ELSE
                    type = '  '
                END IF
             END IF 
        END IF
C
C*	Determine if there is a level of intensity.
C        
        ilvl = INDEX ( string (:iend),'DMSHG') 
        IF ( ilvl .ne. 0 ) THEN
            intsy = 'DMSHG'
          ELSE
            ilvl = INDEX ( string (:iend),'DSIPTG') 
            IF ( ilvl .ne. 0 ) THEN
                intsy = 'DSIPTG'
              ELSE
                ilvl = INDEX(string(:iend),'INTSF') 
                IF ( ilvl .ne. 0 ) THEN
                    intsy = 'INTSFYG'
                  ELSE
                    ilvl = INDEX(string(:iend),'DVLPG') 
                    IF ( ilvl .ne. 0 ) THEN
                        intsy = 'DVLPG'
                      ELSE
                        intsy = '  '
                    END IF
                END IF
             END IF 
        END IF
C
        IF ( ilvl .ne. 0 ) THEN
             ioff = iend - ilvl
             iend = iend - ioff
        END IF 
C
C*	If a line area, find the distance.
C        
        IF ( type .eq. 'LN' ) THEN
            ikts = INDEX ( string ,'KT') 
            IF ( ikts .eq. 0 ) THEN
              ikts = INDEX ( string, 'LTL')
            END IF
            its = INDEX ( string (:ikts),' TS ') 
            idst = INDEX ( string (:ikts),'NM WIDE') 
            IF ( its .lt. idst ) THEN
                dist = string (its+4:idst-2)
              ELSE IF ( its .gt. idst ) THEN
                dist = string (idst-3:idst-1)
            END IF
        END IF
C
C*	If a isolated area, find the diameter, then find the
C*      location.
C        
        IF ( type .eq. 'IS' ) THEN
            ikts = INDEX ( string ,'KT') 
            IF ( ikts .eq. 0 ) THEN
              ikts = INDEX ( string, 'LTL')
            END IF
            its = INDEX ( string (:ikts),'TS D') 
            idst = INDEX ( string (:ikts),'MOV') 
            IF ( its .ne. 0 ) THEN
                IF ( its .lt. idst ) THEN
                    dist = string (its+4:idst)
                  ELSE IF ( its .gt. idst ) THEN
                    dist = string (idst-3:idst-1)
                END IF
            END IF
C           
 	    CALL ST_CLSL ( string(:iend), ' ', ' ', nwdmx, tarr, 
     +                 numwds, ier )
            ii = numwds
            DO WHILE ( ( .not. found ) .and. ( ii .ge. 1) )
C
C*              Search for the 2 character states.
C
                CALL ST_LSTR ( tarr(ii), lens, ier )
                IF ( ( lens .eq. 2 ) .and.  (ii .lt. numwds ) ) THEN
C
C*                  Check for CSTL WTRS wording after 2-Letter State ID.
C
                    IF (tarr(ii+1) .eq. "CSTL") THEN
                      ii = ii + 2
                    END IF
C
C*                  Check for AND CSTL WTRS wording after 2-Letter State
C*                  ID.
C
                    IF (tarr(ii+1) .eq. "AND") THEN
                      ii = ii + 3
                    END IF
                    CALL ST_ALNM ( tarr(ii+1)(1:1), ikind, ier)
C
C*                  Check to see if the next array is the distance
C*                  and direction from the station ID.
C
                    IF ( ikind .eq. 1 ) THEN
                        numwds = 1
                        tmpch = tarr(ii+1) // tarr(ii+2)
                        CALL ST_RXBL ( tmpch, tmpch, lent, ier )
                        carr(1) = tmpch
                        found = .true.
                        ii = 0
                      ELSE IF ( ikind .eq. 2 ) THEN
C
C*                      Don't have a distance/direction, only station ID.
C
                        IF ((INDEX(tarr(ii+1),'VCNTY') .gt. 0) .and.
     +                       (ii+2 .le. numwds)) THEN
                          tmpch = tarr(ii+2)
                        ELSE
                          tmpch = tarr(ii+1)
                        END IF
                        numwds = 1
                        CALL ST_RXBL ( tmpch, tmpch, lent, ier )
                        carr(1) = tmpch
                        found = .true.
                        ii = 0
                    END IF
                 END IF
                 ii = ii - 1
            END DO
        END IF
C
C*	Put the individual words of the bounds string in an array.
C
        IF ( ( type .eq. 'AR' ) .or. ( type .eq. 'LN' ) ) THEN
 	    CALL ST_CLSL ( string(istrt:iend-1), '-', '-', nwdmx, carr, 
     +                     numwds, ier )
        END IF
C
        iptr = iend
C
C*	Get the latitude and longitude pairs.
C
        ii = 1
        loc  = ' '
        ddbb = ' '
        DO WHILE (.not. done )
            CALL ST_LSTR ( carr(ii), lenc, ier )
            IF ( lenc .eq. 3 ) THEN
C 
C*              Only a location is given.
C
                loc  = carr ( ii )
                ddbb = ' '
              ELSE IF ( lenc .gt. 3 ) THEN
C 
C*              A distance, bearing and location are given.
C
 	        CALL ST_CLSL ( carr(ii), ' ', ' ', nwdmx, tarr, 
     +                 numc, ier )
                ddbb = tarr ( 1 )
                loc  = tarr ( 2 )
            END IF
C
            tmpstr = loc
            IF ( .not. done ) THEN
                IF ( ddbb .ne. ' ' ) THEN
C
C*                  Insert a blank between the distance and bearing.
C
                    ibb = 1
                    CALL ST_LSTR ( ddbb, lendb, ier )
                    DO jj = 1, lendb
                        CALL ST_ALNM ( ddbb ( jj:jj ), ityp, ier )
                        IF ( ( ityp .eq. 2 ) .and. ( ibb .eq. 1 ) ) THEN
                            ibb = jj
                            END IF
                    END DO
                    tmpstr = ddbb ( ibb:lendb ) // ' ' // tmpstr
                    IF ( ibb .gt. 1 )
     +                   tmpstr = ddbb ( :ibb - 1 ) // ' ' // tmpstr
                END IF
C
                CALL ST_NULL ( tmpstr, tmpstr, ll, ier )
                locnam = 'VOR'
                CALL ST_NULL ( locnam, locnam, ll, ier )
                CALL CLO_DDDEC ( locnam, IFMT, tmpstr, 1, xlat, xlon,
     +                           nstn, ier )
                IF ( ier .eq. 0 ) THEN
                    IF ( npt .lt. mxp ) THEN
                        npt = npt + 1
                        rlat ( npt ) = xlat
                        rlon ( npt ) = xlon
                    END IF
                  ELSE IF ( ier .ne. ( -2 ) ) THEN
                    CALL DC_WLOG ( 2, 'DCCSIG', -8, tmpstr, ierr )
                  ELSE
                    IF ( loc .ne. ' ' ) THEN
                       CALL DC_WLOG ( 0, 'DCCSIG', -7, loc, ierr )
                    END IF
                END IF
            END IF
            ii = ii + 1
            IF ( ii .gt. numwds ) done = .true.
        END DO
C
        IF ( ( npt .gt. 0 ) .and. ( type .eq. 'AR' ) ) THEN
            IF ( ( rlat ( 1 ) .ne. rlat ( npt ) ) .or.
     +           ( rlon ( 1 ) .ne. rlon ( npt ) ) ) THEN
                IF ( npt .lt. mxp )  npt = npt + 1
                rlat ( npt ) = rlat ( 1 )
                rlon ( npt ) = rlon ( 1 )
            END IF
        END IF
C*
	RETURN
	END
