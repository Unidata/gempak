	SUBROUTINE IS_CBND ( string, mxp, npt, rlat, rlon, iptr, iret ) 
C************************************************************************
C* IS_CBND  								*
C*									*
C* This subroutine gets the lat/lon coordinates of geographical points  *
C* in international sigmet reports.  					*
C*                                                                      *
C* IS_CBND ( STRING, MXP, NPT, RLAT, RLON, IPTR, IRET )			*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*	MXP		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	NPT		INTEGER		Number of bounds points         *
C*	RLAT(*)		REAL		Latitudes of bounds points      *
C*	RLON(*)		REAL		Longitudes of bounds points     *
C*	IPTR		INTEGER		Pointer following last bounds pt*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = "FROM" or "FM" not found	*
C*					 -2 = No points found		*
C*					 -7 = Location not in vors.tbl	*
C*					-11 = Bad sigmet location	*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/03	Created from CS_CBND			*
C* F. J. Yen/NCEP	 2/04	Allowed for "." before "FROM" or "FM".	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string
	REAL		rlat (*), rlon (*)
C*
C*      IFMT format code interpretation for location string -
C*      (See locator.tbl for further information)
C*      5 = nearest 5 miles
C*      1 = nautical miles (NM)
C*      1 = 16 point compass direction
C*      2 = 3 character station id
C*
	PARAMETER	( NWDMAX = 120, IFMT = 5112)
C*
	CHARACTER	carr (NWDMAX)*15, sep*1, tarr (NWDMAX)*15
        CHARACTER	loc*3, ddbb*6, tmpstr*30, locnam*10, strwk*1250
        LOGICAL		done
C------------------------------------------------------------------------
	iret    = 0
        iend    = 0
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
        istrt = ifrom + 5 
	IF ( ifrom .eq. 0 ) THEN
	    ifrom = INDEX ( string, 'FM' )
	    istrt = ifrom + 3
	    IF ( ifrom .eq. 0 ) THEN
C
C*		Check for isolated point, such as "60NM OF NUD"
C
		inm = INDEX ( string, 'NM ' )
		IF ( inm .ne. 0 ) THEN
		    iof = INDEX ( string ( inm+2:inm+7 ), ' OF ')
		    IF ( iof .ne. 0 ) THEN
			istrt = inm + iof + 5
		      ELSE
		        iret = -1
		        RETURN
		    END IF
		  ELSE
		    iret = -1
		    RETURN
		END IF
	    END IF
	END IF
C
C*      Roughly find the end of the area definition.
C
        iend = INDEX ( string ( istrt: ) , '.' ) 
        IF ( iend .eq. 0 ) THEN
            CALL ST_LSTR ( string , lens, ier ) 
            iend = lens
	  ELSE
	    iend = istrt + iend - 1
        END IF 
C           
	CALL ST_CLSL ( string(istrt:iend), '-', '-', nwdmx, carr, 
     +                     numwds, ier )
C
	IF ( numwds .eq. 0 ) THEN
	    iret = -2
	    RETURN
	END IF
	IF ( numwds .eq. 1 ) THEN
C
C*	    Search for " TO " separating geographical positions
C*	    and replace with "-"
C
	    strwk = string ( istrt:iend )
	    ito = INDEX ( strwk, ' TO ' )
	    IF ( ito .ne. 0 ) THEN
		ipos = -1
		DO WHILE ( ipos .ne. 0 )
    	            CALL ST_RPSL ( strwk, ' TO ', 4, '-', 1, ipos,
     +			       strwk, ier )
		END DO
		CALL ST_LSTR ( strwk, lns, ier )
		CALL ST_CLSL ( strwk( :lns ), '-', '-', nwdmx, carr, 
     +                     numwds, ier )
	    END IF	
	END IF
        iptr = iend
C
C*	Get the latitude and longitude pairs.
C
        ii = 1
        loc  = ' '
        ddbb = ' '
        DO WHILE (.not. done )
	    CALL ST_RXBL ( carr(ii), carr(ii), lenc, ier )
            IF ( lenc .eq. 3 ) THEN
C 
C*              Only a location is given.
C
		numc = 1
                loc  = carr ( ii ) (1:3)
                ddbb = ' '
              ELSE IF ( lenc .gt. 3 ) THEN
C 
C*              A distance, bearing and location are given.
C
 	        CALL ST_CLSL ( carr(ii), ' ', ' ', nwdmx, tarr, 
     +                 numc, ier )
                ddbb = tarr ( 1 ) (1:6)
                loc  = tarr ( numc ) (1:3)
            END IF
C
            tmpstr = loc
            IF ( .not. done ) THEN
                IF ( ddbb .ne. ' ' ) THEN
		    IF ( numc .eq. 2 ) THEN
C
C*                      Insert a blank between the distance and bearing
C
                        ibb = 1
                        CALL ST_LSTR ( ddbb, lendb, ier )
                        DO jj = 1, lendb
                            CALL ST_ALNM ( ddbb ( jj:jj ), ityp, ier )
                            IF ( ibb .eq. 1 ) THEN
                                ibb = jj
                            END IF
                        END DO
                        tmpstr = ddbb ( ibb:lendb ) // ' ' // tmpstr
                        IF ( ibb .gt. 1 )
     +                       tmpstr = ddbb ( :ibb - 1 ) // ' ' // tmpstr
		      ELSE
			IF ( numc .eq. 4 ) THEN
			    IF ( tarr(2) .eq. 'NM' ) THEN
				numc = 3
				CALL ST_LSTR ( tarr(1), ln1, ier )
				tarr (1) = tarr (1) (:ln1) // 'NM'
				tarr (2) = tarr (3)
			    END IF
			END IF
			IF ( numc .eq. 3 ) THEN
			    CALL ST_LSTR ( tarr(1), ln1, ier )
			    CALL ST_LSTR ( tarr(2), ln2, ier )
			    CALL ST_LSTR ( tmpstr, lnp, ier )
			    tmpstr = tarr ( 1 ) (:ln1) // " " //
     +				    tarr ( 2 ) (:ln2) // " " // tmpstr (:lnp)
			END IF
		    END IF
               	END IF
C
                CALL ST_NULL ( tmpstr, tmpstr, ll, ier )
                locnam = 'VOR'
                CALL ST_NULL ( locnam, locnam, ll, ier )
		jnm = INDEX ( tmpstr, 'NM ' )
		IF ( jnm .ne. 0 ) THEN
		    IF ( tmpstr ( jnm-1:jnm-1 ) .eq. ' ' ) THEN
			tmpstr = tmpstr (1:jnm-2) // tmpstr ( jnm: )
		    END IF
		  ELSE
		    CALL ST_ALNM ( tmpstr ( 1:1 ), ityp, ier )
		    IF ( ityp .eq. 1 ) THEN
		        jspc = INDEX ( tmpstr, ' ' )
			tmpstr = tmpstr (1:jspc-1) // 'NM' // tmpstr (jspc: )
		    END IF
		END IF
                CALL CLO_DDDEC ( locnam, IFMT, tmpstr, 1, xlat, xlon,
     +                           nstn, ier )
                IF ( ier .eq. 0 ) THEN
                    IF ( npt .lt. mxp ) THEN
                        npt = npt + 1
                        rlat ( npt ) = xlat
                        rlon ( npt ) = xlon
                    END IF
                  ELSE IF ( ier .ne. -2 ) THEN
                    CALL DC_WLOG ( 2, 'DCISIG', -11, tmpstr, ierr )
                  ELSE
                    IF ( loc .ne. ' ' ) THEN
                       CALL DC_WLOG ( 0, 'DCISIG', -7, loc, ierr )
                    END IF
                END IF
            END IF
            ii = ii + 1
            IF ( ii .gt. numwds ) done = .true.
        END DO
C*
	RETURN
	END
