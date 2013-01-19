	SUBROUTINE IS_EGAO ( report, lenr, mxp, npt, rlat, rlon, irad,
     +			     iptr, kdir, iret )
C************************************************************************
C* IS_EGAO 								*
C*									*
C* This subroutine gets the boundary points of the open area covered by *
C* an EGGY international sigmet phenomenon.				*
C*                                                                      *
C* IS_EGAO ( REPORT, LENR, MXP, NPT, RLAT, RLON, IRAD, IPTR, KDIR,	*
C*	     							IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	LENR		INTEGER		Length of string                *
C*	MXP		INTEGER		Maximum number of points        *
C*									*
C* Output parameters:							*
C*	NPT 		INTEGER		Number of boundary points       *
C*      RLAT(*)		REAL		Latitudes of boundary points    *
C*	RLON(*)		REAL		Longitudes of boundary points   *
C*	IRAD		INTEGER		Radius if area is a circle      *
C*	IPTR		INTEGER		Pointer to location after area  *
C*	KDIR		CHAR*3	 	Direction from open area	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 3/00	Created					*
C* D. Kidwell/NCEP	10/01   Used ST_WORD to check for lat/lon       *
C* F. J. Yen/NCEP	 2/02	Put initializations in DATA statement	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	REAL		rlat (*), rlon (*)
C*
	CHARACTER	idir (8)*3
	CHARACTER	kdir*3, kdir1*3, kdir2*3
	CHARACTER	carr (20)*12, work*20
	INTEGER		ndr (2), iof (2) 
	LOGICAL		done, twoof
C*
	DATA		idir /' N ', ' S ', ' E ', ' W ',
     +  		      ' NE', ' NW', ' SE', ' SW'/
C------------------------------------------------------------------------
	iret = 0
	iptr = 0
	npt  = 0
	irad = IMISSD
	DO i = 1, mxp
	    rlat ( i ) = RMISSD
	    rlon ( i ) = RMISSD
	END DO 
C
C*	Look for an open area definition.
C*	Examples:  N OF 56N	              SE OF A LINE 53N02E 50N03W
C*		   S OF 49N AND W OF 27W      NE OF LINE 55N00W TO 61N06W
C
	len  = MIN ( 100, lenr ) 
	iof(1) = INDEX ( report ( :len ), ' OF ' )
	IF ( iof(1) .eq. 0 ) THEN
	    iret = -4
	    RETURN
	END IF
C
C* 	Check for direction in front of keyword 'OF'
C
	jdir = 0
	i = 1
	DO WHILE ( jdir .eq. 0 .and. i .le. 8 )
	    jdir = INDEX ( report (iof(1)- 3 :iof(1) ), idir(i) )
	    IF ( jdir .ne. 0 ) THEN
		kdir1 = idir(i) ( 2:2 + (i-1)/4 )
		kdir = kdir1
		ndr(1) = i
	    END IF 
	    i = i + 1
	END DO
	IF ( jdir .eq. 0 ) THEN 
	    iret = -4
	    RETURN
        END IF
C
C*	Find the beginning and end of the point definition string.
C
	ii   = iof(1) + 4
	done = .false.
	DO WHILE ( .not. done )
	    CALL ST_ALNM ( report ( ii:ii ), ityp, ier )
	    IF ( ityp .eq. 1 ) THEN
		done = .true.
		ibeg = ii - 1
	      ELSE
		ii = ii + 1
		IF ( ii .gt. lenr ) THEN
		    iret = -4
		    RETURN
		END IF
	    END IF
	END DO
	iend = INDEX ( report ( ii:len ), '=' )
	IF ( iend .eq. 0 ) THEN
	    iend = INDEX ( report ( ii:len ), '.' )
	    IF ( iend .eq. 0 ) THEN
		iend = lenr - ii + 1
	    END IF
	END IF
	iend = ii + iend - 1

C
C*	Check if another possible direction " OF "
C
	twoof = .false.
	ii = iof(1) + 5
	IF (ndr(1) .lt. 5) THEN
	    iof(2) = INDEX ( report ( ii:ii + 16 ), ' OF ' )
	    IF ( iof(2) .ne. 0 ) THEN
C
C*	        Second 'OF' found; Possible two open ended directions;
C
     		iof(2) = ii + iof(2) - 1
		jdir2 = 0
		i = 1
		DO WHILE ( jdir2 .eq. 0 .and. i .le. 4 )
	    	    jdir2 = INDEX (report(iof(2)-3:iof(2)+12),idir(i))
	    	    IF ( jdir2 .ne. 0 ) THEN
		        kdir2 = idir(i) (2:2)
			ndr (2) = i
	    	    END IF 
	    	    i = i + 1
	        END DO
		IF ( jdir2 .ne. 0 ) THEN 
		    twoof = .true.
        	END IF
	    END IF
	END IF
C
C*	Get the points.
C
	iptr = iend + 3
	iloc = ibeg
	CALL ST_CLST ( report ( iloc:iend ), ' ', ' ', mxp, carr, 
     +	 	       npts, ier )
	ii = 1
	DO WHILE ( ii .le. npts )
	    work = carr ( ii )
	    CALL ST_LSTR ( work, lenw, ier )
	    IF ( lenw .le. 5 ) THEN
		CALL ST_WORD ( work ( :lenw ), ityp, ier )
		IF ( ityp .eq. 1 ) THEN 
		    ii   = ii + 1
		    work = work ( :lenw ) // carr ( ii )
		END IF
	    END IF
	    CALL IS_LTLN ( work, xlat, xlon, ier )
	    IF ( ier .eq. 0 ) THEN
		    npt = npt + 1
		    rlat ( npt ) = xlat
		    rlon ( npt ) = xlon
	    END IF
	    ii = ii + 1
	END DO
	IF ( twoof ) THEN
	    CALL IS_EG2D ( report, iof, ndr, kdir1, kdir2, kdir,
     +			   npt, rlat, rlon, iret ) 
	END IF
	IF ( npt .eq. 0 ) THEN
	    iret = -4
	END IF
C*
	RETURN
	END
