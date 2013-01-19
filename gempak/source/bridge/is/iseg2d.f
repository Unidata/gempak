	SUBROUTINE IS_EG2D ( report, iof, ndr, kdir1, kdir2, kdir,
     +			     npt, rlat, rlon, iret )
C************************************************************************
C* IS_EG2D 								*
C*									*
C* This subroutine gets the boundary points of the open area defined by *
C* two open ended directions covered by an EGGY international sigmet	*
C* phenomenon.  Example:  S OF 49N AND W of 27W.			*
C*                                                                      *
C* IS_EG2D ( REPORT, IOF, NDR, KDIR1, KDIR2, KDIR, NPT, RLAT, RLON,	*
C*	     							IRET )	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Partial sigmet report string    *
C*	IOF(*)		INTEGER		Position of 1st and 2nd 'OF's	*
C*	NDR(*)		INTEGER		Indices of Direction		*
C*	KDIR1		CHAR*		Direction from first 'OF'	*
C*	KDIR2		CHAR*		Direction from second 'OF'	*
C*									*
C* Output parameters:							*
C*	KDIR		CHAR*		Combined direction		*
C*	NPT 		INTEGER		Number of boundary points       *
C*      RLAT(*)		REAL		Latitudes of boundary points    *
C*	RLON(*)		REAL		Longitudes of boundary points   *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = bad area definition       *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 3/00	Created					*
C* F. J. Yen/NCEP	 4/00	Fixed bug for decoding 1 digit lat/lon. *
C* F. J. Yen/NCEP	 6/02	Handled new lat/lon format.		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report
	REAL		rlat (*), rlon (*)
C*
	CHARACTER*(*)	kdir, kdir1, kdir2, carr (2)*12
	INTEGER		iof (*), ndr (*)
C------------------------------------------------------------------------
	iret = 0
	npt  = 0
	xlat = RMISSD 
	xlon = RMISSD
	DO i = 1, 2
	    ii = iof(i) + 4
	    CALL ST_CLST ( report ( ii:ii + 12 ) ,' ', ' ', 2, carr,
     +		    npts, ier )
	    IF ( npts .ge. 1 ) THEN	    
C
C*		Get latitude or longitude.
C
		CALL IS_LORL ( carr(1), xlatt, xlont, ier )
		IF ( ier .eq. 0 ) THEN
		    IF ( xlatt .ne. RMISSD ) THEN
			xlat = xlatt
		      ELSE
		        IF ( xlont .ne. RMISSD ) THEN
			    xlon = xlont
			END IF
		    END IF
		END IF
	    END IF
 	END DO
C
	IF ( (INDEX (kdir1,'N') .ne. 0) .or.
     +	         (INDEX (kdir1,'S') .ne. 0) ) THEN
	    kdir = kdir1(1:1) // kdir2(1:1)
	  ELSE
	    kdir = kdir2(1:1) // kdir1(1:1)
	END IF
C
C*	Determine lat long values 300nm from intersection
C
	dist = PR_HGNM ( 300. )
	IF ( INDEX (kdir(1:1), 'N' ) .ne. 0) THEN
	    ang = 0.
	ELSE
	    IF ( INDEX (kdir(1:1), 'S') .ne. 0 ) THEN
		ang = 180.
	    END IF
	END IF
	CALL CLO_DLTLN ( xlat, xlon, dist, ang, rlat (1),
     +		 	 rlon (1), ier)
	IF ( ier .ne. 0 ) THEN
	    iret = -4
	    RETURN
	END IF 
	IF ( INDEX (kdir(2:2), 'E' ) .ne. 0) THEN
	    ang = 90.
	ELSE
	    IF ( INDEX (kdir(2:2), 'W') .ne. 0 ) THEN
		ang = 270.
	    END IF
	END IF
	CALL CLO_DLTLN ( xlat, xlon, dist, ang, rlat (3),
     +		 	 rlon (3), ier)
	IF ( ier .ne. 0 ) THEN
	    iret = -4
	    RETURN
	END IF 
	npt = 3
	rlon (1) = xlon
	rlat (2) = xlat
	rlon (2) = xlon
	rlat (3) = xlat
C*
	RETURN
	END
