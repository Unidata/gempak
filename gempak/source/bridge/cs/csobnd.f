	SUBROUTINE CS_OBND ( string, mxp, npt, rlat, rlon, inxt, iret ) 
C************************************************************************
C* CS_OBND  								*
C*									*
C* This subroutine gets the bounds for a convective outlook report.  	*
C*                                                                      *
C* CS_OBND ( STRING, MXP, NPT, RLAT, RLON, INXT, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*	MXP		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	NPT		INTEGER		Number of bounds points         *
C*	RLAT(*)		REAL		Latitudes of bounds points      *
C*	RLON(*)		REAL		Longitudes of bounds points     *
C*	INXT		INTEGER		Pointer following last bounds pt*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -6 = no bounds given           *
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02	Created					*
C* A. Hardy/NCEP	 8/02	Modified error checking for loc		*
C* A. Hardy/NCEP	10/02	Changed tarr from NWDMAX to 2		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string 
	REAL		rlat (*), rlon (*)
C*
	PARAMETER	( NWDMAX = 120, IFMT = 5112 )
C*
	CHARACTER	carr (NWDMAX)*10, sep*1, tarr (2)*10
        CHARACTER	loc*3, ddbb*6, tmpstr*20, locnam*10
        LOGICAL		done
C------------------------------------------------------------------------
	iret    = 0
        iend    = 0
        icon    = 0
	npt     = 0
        ifrom   = 0
        sep     = '-'
        done    = .false.
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
C
C*	Put the individual words of the bounds string in an array.
C
 	CALL ST_CLSL ( string(ifrom+5:), '-', '-', nwdmx, carr, 
     +                 numwds, ier )
C
        inxt = iend
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
 	        CALL ST_CLSL ( carr(ii), ' ', ' ', 2, tarr, 
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
        IF ( npt .gt. 0 ) THEN
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
