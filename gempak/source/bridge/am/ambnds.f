	SUBROUTINE AM_BNDS ( string, type, mxp, stype, npt, rlat, rlon,
     +			     iptr, iret ) 
C************************************************************************
C* AM_BNDS  								*
C*									*
C* This subroutine gets the airmet subtype and bounds for an airmet     *
C* report.                                                              *
C*                                                                      *
C* AM_BNDS ( STRING, TYPE, MXP, STYPE, NPT, RLAT, RLON, IPTR, IRET )    *
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*	TYPE		CHAR*		Report type                     *
C*	MXP		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	STYPE		CHAR*		Airmet subtype                  *
C*	NPT		INTEGER		Number of bounds points         *
C*	RLAT(*)		REAL		Latitudes of bounds points      *
C*	RLON(*)		REAL		Longitudes of bounds points     *
C*	IPTR		INTEGER		Pointer following last bounds pt*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -5 = no valid airmet subtype   *
C*					 -6 = no bounds given           *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/00	                                        *
C* D. Kidwell/NCEP	 7/00	Added check on max points               *
C* D. Kidwell/NCEP	 9/00	Replaced AM_GTPT with CLO_DDDEC         *
C* D. Kidwell/NCEP	 1/02	Allowed for string > 160 characters     *
C* A. Hardy/NCEP         8/02   Modified to use br_bnds			*
C* m.gamazaychikov/SAIC	01/04	Added check for type SW			*
C* J. Lewis/AWC		02/07   Added check for sub-type LLWS		*
C* J. Lewis/AWC		09/07	Change sub-type check to "LLWSPOT"	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, type, stype
	REAL		rlat (*), rlon (*)
C*
	PARAMETER	( NWDMAX = 120 )
C*
	CHARACTER	carr (NWDMAX)*10, strbuf*160, tmpbuf*160,
     +  		tmpbuf2*160, sep*2
	LOGICAL		done, blank
C------------------------------------------------------------------------
        iret  = 0
	npt   = 0
	iptr  = 1
	stype = type
C
        DO ii = 1, mxp
	    rlat ( ii ) = RMISSD
	    rlon ( ii ) = RMISSD
	END DO
C
C*	Get the airmet subtype.
C
	CALL ST_LSTR ( string, lens, ier )
	iend = INDEX ( string ( :lens ), '...' ) - 1
	CALL ST_RMBL ( string ( :iend ), strbuf, lenbuf, ier )
	IF ( strbuf ( :lenbuf ) .eq. 'IFR' ) THEN
	  ELSE IF ( strbuf ( :lenbuf ) .eq. 'MTNOBSCN' ) THEN
	    stype = 'MO'
	  ELSE IF ( strbuf ( :lenbuf ) .eq. 'TURB' ) THEN
	  ELSE IF ( strbuf ( :lenbuf ) .eq. 'ICE' ) THEN
	  ELSE IF ( strbuf ( :7 )      .eq. 'STGSFCW' ) THEN
	    stype = 'SW'
	  ELSE IF ( strbuf ( :7 )      .eq. 'LLWSPOT' ) THEN
            stype = 'WS'
	  ELSE
	    iret = -5
	    RETURN
	END IF
C
	IF ( stype .eq. 'WS' ) THEN
	    ibound = INDEX ( string ( :lens ), ' BOUNDED BY ' )
	    IF ( ibound .eq. 0 ) THEN
	        iret = -6
	        RETURN
	    ELSE
		sep = '-'
	    END IF
	  ELSE
	    ibound = INDEX ( string ( :lens ), ' FROM ' )
	    IF ( ibound .eq. 0 ) THEN
                iret = -6
                RETURN
            ELSE
                sep = 'TO'
            END IF
	END IF
	
C
C*	Put the individual words of the bounds string in an array.
C
	done   = .false.
        numc   = 0
        istart = ibound
        nwdmx  = NWDMAX
        iwd    = 1
C
	DO WHILE ( .not. done )
	    iend = MIN ( istart + 159, lens )
            IF ( iend .lt. lens ) THEN
		IF ( ( string ( iend:iend ) .ne. ' ' ) .and.
     +		     ( string ( iend + 1:iend + 1 ) .ne. ' ' ) ) THEN
		    blank = .false.
		    ic    = iend - 1
		    DO WHILE ( .not. blank )
			IF ( string ( ic:ic ) .eq. ' ' ) THEN
			    blank = .true.
			    iend  = ic - 1
			    IF ( iend .lt. istart ) iend = istart
			  ELSE
			    ic = ic - 1
			    IF ( ic .lt. istart ) blank = .true.
			END IF
		    END DO
		END IF
	      ELSE
		done = .true.
	    END IF

	    strbuf = string ( istart:iend )
	    IF ( stype .eq. 'WS' ) THEN
C
C*		Put a space before and after each delimiter "-" so the string of points
C*		can be parsed correctly.
C
		ipos    = 1
		ibeg    = 1
		iptr    = 0
		nchars  = 0
		DO WHILE ( ipos .gt. 0 )
		    CALL ST_RPSL ( strbuf(ibeg:), '-', 1, ' - ' , 3,
     +                             ipos, tmpbuf, iret)

		    IF ( ipos .gt. 0 ) THEN
			nchars = ipos + 2
			tmpbuf2 ( iptr + 1:iptr + nchars ) = tmpbuf ( 1:nchars )
			ibeg = ibeg + ipos
		        iptr = iptr + nchars
		      ELSE
			tmpbuf2 ( iptr + 1: ) = strbuf ( ibeg: )
     			CALL ST_LSTR ( tmpbuf2, lenstr, ier )
        		strbuf ( 1:lenstr ) = tmpbuf2
		    END IF   
		END DO
	    END IF

	    CALL ST_CLST ( strbuf, ' ', ' ', nwdmx, carr ( iwd ),
     +	                   numwds, ier )
	    numc  = numc + numwds
	    iwd   = numc + 1
	    nwdmx = nwdmx - numwds
	    istart = iend + 1
	    IF ( ( istart .gt. lens ) .or. ( ier .eq. 1 )) done = .true.
        END DO

C
C*	Get the latitude and longitude pairs.
C
        CALL BR_BNDS ( string, mxp, carr, numc, sep, npt, rlat, rlon, 
     +                 iptr, ier )
C*
	RETURN
	END

