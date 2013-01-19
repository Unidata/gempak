	SUBROUTINE LC_DIST ( stntbl, rlat, rlon, maxstn, maxdst,
     +			     stid, istn, dist, nstn, iret )
C************************************************************************
C* LC_DIST								*
C*									*
C* This subroutine computes the distance from a point to each station	*
C* in a station table.  The distances to the stations are returned in	*
C* the ascending order.							*
C*									*
C* LC_DIST  ( STNTBL, RLAT, RLON, MAXSTN, MAXDST, STID, ISTN, DIST, 	*
C*							NSTN, IRET )	*
C*									*
C* Input parameters:							*
C*	STNTBL		CHAR*		Station table name		*
C*	RLAT		REAL		Latitude of a point		*
C*	RLON		REAL		Longitude of a point		*
C*	MAXSTN		INTEGER		Maximum no of stations		*
C*	MAXDST		INTEGER		Maximum search distance		*
C*									*
C* Output parameters:							*
C*	STID (NSTN)	CHAR* 		Station identifier		*
C*	ISTN (NSTN)	INTEGER		Station number			*
C*	DIST (NSTN)	REAL		Distance between the pt and stns*
C*	NSTN		INTEGER		Number of stations		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return 		*
C*					  -3 = station table not opened	*
C**									*
C* Log:									*
C* T. Lee/GSC		 8/99	Created					*
C* T. Lee/GSC		11/99	Called CLO_DIST; Used insertion sort	*
C* T. Lee/GSC		 4/01	Returned station number			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	stid (*), stntbl
	REAL		dist (*)
	INTEGER		istn (*)
C*
        INTEGER         ispri (LLSTFL), nnn (LLSTFL)
        CHARACTER       stnam (LLSTFL)*32, stat (LLSTFL)*2, 
     +			coun (LLSTFL)*2, tbchrs (LLSTFL)*20, 
     +			sid (LLSTFL)*8, sss (LLSTFL)*8
        REAL            slat (LLSTFL), slon (LLSTFL), selv (LLSTFL),
     +			rdis (LLSTFL), ddd (LLSTFL)
	LOGICAL		found, done
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret = 0
C
C*	Open the table file.
C
	CALL FL_TBOP  ( stntbl, 'stns', lun, ierr )
	IF  ( ierr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ierr, stntbl, ier )
	    iret = -3
	    RETURN
	END IF
C
C*	Read in all the stations.
C
	CALL TB_ASTN  ( lun, LLSTFL, nst, sid, stnam, istn, stat,
     +                  coun, slat, slon, selv, ispri, tbchrs, iret )
C
C*	Close the table file.
C
	CALL FL_CLOS  ( lun, ier )
C
C*      Compute the distance between the input point and stations.
C
	CALL CLO_DIST ( rlat, rlon, nst, slat, slon, rdis, ier )
C
C*	Assign non-missing data to buffer.
C
	ik = 1
	nn = 0
	found = .false.
C
	DO WHILE  ( ( .not. found ) .and. ( ik .le. nst ) )
	    IF  ( .not. ERMISS ( rdis ( ik ) ) .and.
     +		  rdis ( ik ) .le. maxdst )  THEN
		ddd ( 1 ) = rdis  ( ik )
		sss ( 1 ) = sid   ( ik )
		nnn ( 1 ) = istn  ( ik )
		nn = nn + 1
		found = .true.
	    END IF
	    ik = ik + 1
	END DO
C
C*      Sort station based on distance.
C
	DO  i = ik, nst
	    IF  ( .not. ERMISS ( rdis ( i ) ) .and. 
     +		( rdis ( i ) .le. maxdst ) )  THEN
		j = 1
		done = .false.
C
		DO  WHILE  ( .not. done )
C
C*		    Inserting data into the output array.
C
		    IF  ( rdis ( i ) .le. ddd ( j ) )  THEN
			DO  k = nn, j, -1
			    ddd ( k+1 ) = ddd ( k )
			    sss ( k+1 ) = sss ( k )
			    nnn ( k+1 ) = nnn ( k )
			END DO
		        ddd ( j ) = rdis ( i )
		        sss ( j ) = sid  ( i )
		        nnn ( j ) = istn ( i )
			done = .true.
		      ELSE IF  ( j .eq. nn )  THEN
			ddd ( nn + 1 ) = rdis ( i )
			sss ( nn + 1 ) = sid  ( i )
			nnn ( nn + 1 ) = istn ( i )
			done = .true.
		    END IF
		    j = j + 1
C
		END DO
		nn = nn + 1
	    END IF
	END DO
C
C*	Return requested number of stations.
C
	nstn = MIN ( maxstn, nn )
	DO  i = 1, nstn
	    stid (i) = sss (i)
	    dist (i) = ddd (i)
	    istn (i) = nnn (i)
	END DO
C*
	RETURN
	END
