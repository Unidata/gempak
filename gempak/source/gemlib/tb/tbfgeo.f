	SUBROUTINE TB_FGEO  ( geog, rlatll, rlonll, rlatur, rlonur,
     +			      cdproj, cenlat, cenlon, iret )
C************************************************************************
C* TB_FGEO								*
C*									*
C* This subroutine searches the geographic name table for an area and	*
C* returns the lower-left and upper-right latitude/longitude pairs.	*
C*									*
C* The input geographic name may be either the abbreviated or full 	*
C* name as it appears in the geographic table.  It must be in 		*
C* upper-case.								*
C*									*
C* The subroutine also returns the default projection and the location 	*
C* of the centroid in terms of latitude and longitude.			*
C*									*
C* If the geographic table cannot be opened, an error message is	*
C* written.								*
C*									*
C* TB_FGEO  ( GEOG, RLATLL, RLONLL, RLATUR, RLONUR, CDPROJ, CENLAT,	*
C*	      CENLON, IRET )						*
C*									*
C* Input parameters:							*
C*	GEOG		CHAR*		Geographic name			*
C*									*
C* Output parameters:							*
C*	RLATLL		REAL		Lower left latitude		*
C*	RLONLL		REAL		Lower left longitude		*
C*	RLATUR		REAL		Upper right latitude		*
C*	RLONUR		REAL		Upper right longitude		*
C*	CDPROJ		CHAR*		Default projection		*
C*	CENLAT		REAL		Centroid latitude		*
C*	CENLON		REAL		Centroid longitude		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return 		*
C*					  -4 = area not in table	*
C*					 -10 = invalid format in table	*
C*					 -11 = area invalid in table	*
C*					 -12 = cannot open geog. table	*
C**									*
C* Log:									*
C* M. desJardins	 4/84	LC_FEGO					*
C* I. Graffman		 6/88	Added # for personal files		*
C* M. desJardins/GSFC	 6/88	Eliminate TB modules; renamed		*
C* D. Keiser/GSC	12/95	FL_TOPN to FL_TBOP, removed #GEOG	*
C* G. Krueger/EAI	 6/96	Add corner points, default projection	*
C* K. Tyle/GSC		 2/97	Search for full geog. name; 		*
C*				change IRETs; update header 		*
C* T. Piper/SAIC	12/01	Changed geoare to 8 bytes from 10	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*) 	geog, cdproj
C*
	LOGICAL 	datfnd
	REAL		tval (6)
	CHARACTER 	geoare*8, geonam*20, tbline*136
C------------------------------------------------------------------------
	iret   = -4
	
	rlatll = 0.0
	rlonll = 0.0
	rlatur = 0.0
	rlonur = 0.0
	cenlat = 0.0
	cenlon = 0.0
	cdproj = ' '
C
C*	Open the file and check for errors.
C
	CALL FL_TBOP  ( 'geog.tbl', 'stns', lun, ier )
	IF  ( ier .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'FL', ier, 'geog.tbl', ierr )
	    iret = -12
	    RETURN
	END IF
C
C*	Read the table searching for the area.
C
	datfnd = .false.
	DO WHILE  ( .not. datfnd )
C
C*	    Read the next record.
C
	    READ ( lun, 1000, IOSTAT = iostat ) tbline
1000	    FORMAT ( A )
	    IF ( iostat .ne. 0 ) THEN
		CALL FL_CLOS ( lun, ier )
		RETURN
	    END IF
	    IF ( tbline (1:1) .eq. '!' ) THEN
		datfnd = .false.
	    ELSE
		CALL ST_RLST ( tbline (27:75), ' ', RMISSD, 6, tval,
     +			       nvals, iern )
		geoare = tbline (1:8)
		geonam = tbline (9:26)
		IF ( nvals .eq. 4 ) THEN
		    iret = -10
		    CALL ER_WMSG  ( 'TB', iret, 'geog.tbl', ierr )
		    CALL FL_CLOS  ( lun, ier )
		    RETURN
		ELSE IF ( geoare .eq. geog .or. geonam .eq. geog ) THEN
		    IF ( (iern .eq. 0) .and. (nvals .eq. 6) ) THEN
			datfnd    = .true.
			cenlat = tval (1)
			cenlon = tval (2)
			rlatll = tval (3)
			rlonll = tval (4)
			rlatur = tval (5)
			rlonur = tval (6)
			cdproj = tbline (76:)
			iret = 0
		    ELSE
			iret = -11
			CALL ER_WMSG ( 'TB', iret, geog, ierr )
			CALL FL_CLOS  ( lun, ier )
			RETURN
		    END IF
		END IF
	    END IF
	END DO
C
C*	Close the table file.
C
	CALL FL_CLOS  ( lun, ier )
C*
	RETURN
	END
