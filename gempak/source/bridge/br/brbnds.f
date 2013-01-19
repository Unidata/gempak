	SUBROUTINE BR_BNDS ( string, mxp, carr, numc, sep, npt, 
     +                       rlat, rlon, iptr, iret ) 
C************************************************************************
C* BR_BNDS  								*
C*									*
C* This subroutine gets the latitude and longitude points array from    *
C* bound lines.								*
C*                                                                      *
C* BR_BNDS ( STRING, MXP, CARR, NUMC, SEP, NPT, RLAT, RLON, IPTR, IRET )*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		Report string                   *
C*	MXP		INTEGER		Maximum number of points        *
C*	CARR(*)		CHAR*		Array of bound points		*
C*	NUMC		INTEGER		Number of strings		*
C* 	SEP		CHAR*		Separation delimiter		*
C*									*
C* Output parameters:							*
C*	NPT		INTEGER		Number of bounds points         *
C*	RLAT(*)		REAL		Latitudes of bounds points      *
C*	RLON(*)		REAL		Longitudes of bounds points     *
C*	IPTR		INTEGER		Pointer following last bounds pt*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	 8/02 	Modified from nc_bnds			*
C* J. Lewis/AWC		02/07	Add check for separator      		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	string, carr (*), sep
	REAL		rlat (*), rlon (*)
C*
C*	IFMT format code interpretation for location string - 
C*	(See locator.tbl for further information)
C*	5 = nearest 5 miles
C*	1 = nautical miles (NM)
C*	1 = 16 point compass direction
C*	2 = 3 character station id
C*
	PARAMETER	( IFMT = 5112 )
C*
	CHARACTER	strbuf*160, start (2)*6,
     +			loc*3, ddbb*6, tmpstr*20, locnam*10
	LOGICAL		done, alpha
C------------------------------------------------------------------------
	iret      = 0
	DO ii = 1, mxp
	    rlat ( ii ) = RMISSD
	    rlon ( ii ) = RMISSD
	END DO
	npt   = 0
	iptr  = 1
	CALL ST_LSTR ( string, lens, ier )
	CALL ST_LSTR ( sep, lsep, ier )

	IF ( sep .eq. 'TO' ) THEN
	    ibounds = INDEX ( string ( :lens ), 'FROM ' )
	    incr = 4
	    ii = 2
	  ELSE IF ( sep .eq. '-' ) THEN
	    ibounds = INDEX ( string ( :lens ), 'BOUNDED BY ' )
	    incr = 10
	    ii = 3
	END IF
	    
	done = .false.
C
C*	Save the first location, to match with the last.
C
	start ( 1 ) = carr ( ii )

	IF ( carr ( ii + 1 ) .ne. sep(:lsep) ) THEN
	    start ( 2 ) = carr ( ii + 1 )
	  ELSE
	    start ( 2 ) = start ( 1 )
	    start ( 1 ) = ' '
	END IF
	loc  = ' '
	ddbb = ' '
C
C*	Loop over all locations, separated by input parameter, sep.
C
	DO WHILE ( .not. done ) 
	    IF ( carr ( ii + 1 ) .eq. sep(:lsep) ) THEN
C  
C*		Only a location is given.
C
		loc  = carr ( ii )
		ddbb = ' '
		ii   = ii + 2
	      ELSE IF ( carr ( ii + 2 ) .eq. sep(:lsep) ) THEN
C  
C*		A distance, bearing and location are given.
C
		loc  = carr ( ii + 1 )
		ddbb = carr ( ii )     
		ii   = ii + 3
	      ELSE
C
C*		This should be the last point.
C
		CALL ST_LSTR ( carr ( ii ), len1, ier )
		alpha = .true.
		DO jj = 1, len1
		    CALL ST_ALNM ( carr ( ii ) ( jj:jj ), ityp, ier )
		    IF ( ityp .ne. 2 ) alpha = .false.
		END DO
		IF ( alpha .and. ( len1 .eq. 3 ) ) THEN
C  
C*		    Only a location is given.
C
		    loc  = carr ( ii )
		    ddbb = ' '
		  ELSE
		    CALL ST_LSTR ( carr ( ii + 1 ), len2, ier )
		    alpha = .true.
		    DO jj = 1, len2
		        CALL ST_ALNM ( carr (ii+1) (jj:jj), ityp, ier )
		        IF ( ityp .ne. 2 ) alpha = .false.
		    END DO
		    ialpha = 0
		    inum   = 0
		    DO jj = 1, len1
		        CALL ST_ALNM ( carr ( ii ) (jj:jj), ityp, ier )
			IF ( ityp .eq. 1 ) THEN
			    inum = jj
			  ELSE IF ( ityp .eq. 2 ) THEN
			    ialpha = jj
			END IF
		    END DO
 		    IF ( ( alpha .and. ( len2 .eq. 3 ) ) .and.
     +			 ( ( len1 .le. 6 ) .and. ( ialpha .gt. 0 ) .and.
     +			   ( inum .gt. 0 ) .and. ( ialpha .gt. inum ) ) )
     +			   THEN
C  
C*		        A distance, bearing and location are given.
C
			loc  = carr ( ii + 1 )
			ddbb = carr ( ii )
		      ELSE
C
C*			The last point was not found. 
C
			done = .true.
		    END IF
		END IF
C
C*		Compare the first and last points.
C
		IF ( .not. done ) THEN
		    IF ( (  loc .eq. start ( 2 ) ) .and.
     +		         ( ddbb .eq. start ( 1 ) ) ) THEN
			done = .true.
		      ELSE
			ii = numc
		    END IF
		END IF
	    END IF
C
	    IF ( .not. done ) THEN
		tmpstr = loc
		IF ( ddbb .ne. ' ' ) THEN
C
C*		    Insert a blank between the distance and bearing.
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
     +			 tmpstr = ddbb ( :ibb - 1 ) // ' ' // tmpstr
		END IF
C
		CALL ST_NULL ( tmpstr, tmpstr, ll, ier )
		locnam = 'VOR'
		CALL ST_NULL ( locnam, locnam, ll, ier )
		CALL CLO_DDDEC ( locnam, IFMT, tmpstr, 1, xlat, xlon, 
     +				 nstn, ier )
		IF ( ier .eq. 0 ) THEN
		    IF ( npt .lt. mxp ) THEN
			npt = npt + 1
			rlat ( npt ) = xlat
			rlon ( npt ) = xlon
		    END IF
		  ELSE IF ( ier .ne. ( -2 ) ) THEN
		    CALL DC_WLOG ( 2, 'BR', -1, tmpstr, ierr ) 
		  ELSE
		    CALL DC_WLOG ( 0, 'BR', -2, loc, ierr ) 
		END IF
	    END IF
	    IF ( ii .ge. numc ) done = .true.
	END DO
C
	IF ( npt .gt. 0 ) THEN
	    IF ( ( rlat ( 1 ) .ne. rlat ( npt ) ) .or.
     +		 ( rlon ( 1 ) .ne. rlon ( npt ) ) ) THEN
		IF ( npt .lt. mxp )  npt = npt + 1
		rlat ( npt ) = rlat ( 1 )
		rlon ( npt ) = rlon ( 1 )
	    END IF
	END IF
C
	IF ( start ( 1 ) .eq. ' ' ) start ( 1 ) = 'TO'
	strbuf = start ( 1 ) // ' ' // start ( 2 )
	CALL ST_RXBL ( strbuf, strbuf, lenbuf, ier ) 
	ibounds = ibounds + incr
	iptr = INDEX ( string (ibounds + lenbuf:lens), strbuf (:lenbuf) )
     +	       + ibounds + 2 * lenbuf - 1
C*
	RETURN
	END
