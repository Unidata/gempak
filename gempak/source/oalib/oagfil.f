	SUBROUTINE OA_GFIL ( gdfile, guess, gdattm, area, gsflag,
     +	              gsdttm, prjnam, prjang, deltan, gltln, 
     +                kex, key, iextnd, iret )
C************************************************************************
C* OA_GFIL								*
C*									*
C* This subroutine processes the grid file inputs of OA programs, and	*
C* returns the information needed to perform a Barnes objective		*
C* analysis.								*
C*									*
C* OA_GFIL  ( GDFILE, GUESS, GDATTM, AREA, GSFLAG, GSDTTM, PRJNAM,	*
C*            PRJANG, DELTAN, GLTLN, KEX, KEY, IEXTND, IRET )		*
C*									*
C* Input parameters:							*
C* 	GDFILE		CHAR*		Output grid file		*
C*	GUESS		CHAR*		First guess grid file		*
C*      GDATTM          CHAR*           Analysis GEMPAK time		*
C*									*
C* Input and output parameters:						*
C*	AREA		CHAR*		User input for AREA		*
C*									*
C* Output parameters:							*
C*	GSFLAG		LOGICAL		Guess file existence flag	*
C*      GSDTTM          CHAR*           Guess GEMPAK time               *
C*	PRJNAM		CHAR*		Projection name			*
C*	PRJANG (3)	REAL		Projection angles		*
C*	DELTAN		REAL		Station spacing			*
C*	GLTLN (4)	REAL		Grid area			*
C*	KEX		INTEGER		Number of x points in ELTLN	*
C*	KEY		INTEGER		Number of y points in ELTLN	*
C*	IEXTND (4)	INTEGER		Grid extension size		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*				 	 -1 = grid file process error	*
C*				 	 -2 = invalid area		*
C*				 	 -3 = invalid grid time		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. desJardins/GSFC	 7/87	Added new GD calls for GEMPAK4		*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   First guess file			*
C* K. Brill/NMC		02/92	Use LLNANL, LLNNAV			*
C* S. Jacobs/NCEP	10/95	Check GDFILE before GUESS; Check for 	*
C*				  previously opened files		*
C* T. Piper/GSC		11/98	Updated prolog				*
C* R. Tian/SAIC		 3/05	Major recoding				*
C* J. Wu/SAIC		 4/05	Output the projection name and angles	*
C* R. Tian/SAIC          4/05   Modified time search, added ER_WMSG     *
C* R. Tian/SAIC          4/06   Modified DG_INXT argument		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	gdfile, guess, gdattm, area, gsdttm, prjnam
	REAL		gltln  (*), prjang (*)
	INTEGER		iextnd (*)
	LOGICAL		gsflag
C*
	CHARACTER	tmpare*72, proj*72, prjout*72
	CHARACTER	firstm*20, tmlist(LLMXGT)*20,
     +                  trange*36, time (2)*20
	REAL		anlblk (LLNANL), rnvblk (LLNNAV),
     +	                eltln (4), dltln (4), grltln (4)
	LOGICAL		gottm
C------------------------------------------------------------------------
	iret = 0
C
C*	Process grid file inputs 'gdfile' and 'guess'. 'guess' is input
C*	grid file, and 'gdfile' is output grid file. If no guess file,
C*	use 'gdfile' as input grid file.
C
	IF ( guess .ne. ' ' ) THEN
	    CALL DG_NFIL ( guess, gdfile, ier )
	    gsflag = .true.
	  ELSE
	    CALL DG_NFIL ( gdfile, gdfile, ier )
	    gsflag = .false.
	END IF
	IF ( ier .ne. 0 ) THEN
	    iret = -1
	    CALL ER_WMSG ( 'OA', iret, gdfile, ier )
	    RETURN
	END IF
C
C*	Process grid time.
C
	gsdttm = ' '
	IF ( gsflag ) THEN
	    CALL DG_NDTM ( 'FIRST-LAST', ier )
	    CALL DG_QTMS ( LLMXGT, .true., tmlist, ntimes, trange, ier )
	    IF ( ntimes .lt. 1 ) THEN
	        iret = -3
	        CALL ER_WMSG ( 'OA', iret, ' ', ier )
	        RETURN
	      ELSE IF ( ntimes .eq. 1 ) THEN
	        gsdttm = tmlist (ntimes)
	      ELSE
C
C*		First check if the analysis time exists.
C
	        nt = 1
	        gottm = .false.
	        DO WHILE ( ( nt .le. ntimes ) .and. ( .not. gottm ) )
	            IF ( gdattm .eq. tmlist (nt) ) THEN
		        gsdttm = gdattm
		        gottm = .true.
		    END IF
	            nt = nt + 1
	        END DO
C
C*		If the analysis time does not exist, compute a forecast
C*		time that is valid at the anylysis time.
C
		IF ( .not. gottm ) THEN
		    firstm = tmlist (ntimes)(:11)
	            CALL TG_VI2F ( gdattm, firstm, gsdttm, len, ier )
		    IF ( ier .ne. 0 ) THEN
	                iret = -7
	                CALL ER_WMSG ( 'OA', iret, gdattm, ier )
	                RETURN
		    END IF
		    gsdttm = gsdttm (:15)
		END IF
	    END IF
	END IF
C
C*	Initialize grid diagostics package.
C
	time (1) = ' '
	time (2) = ' '
	CALL DG_INXT ( .true., .false., time, ier )
C
C*	Replace GRID, EXTEND and DATA in area name.
C
	proj = ' '
	IF ( area .eq. ' ' ) THEN
	    tmpare = 'DATA'
	  ELSE
	    tmpare = area
	END IF
	CALL DG_FIXA ( tmpare, proj, area, prjout, ier )
	IF ( ier .ne. 0 ) THEN
	    iret = -2
	    CALL ER_WMSG ( 'OA', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Query reference grid navigation and analysis block.
C
	CALL DG_QREF ( LLNANL, LLNNAV, anlblk, rnvblk, mxgrd, ier )
 	CALL ST_ITOC  ( rnvblk (2), 1, prjnam, ier )
        prjang (1) = rnvblk (11)
        prjang (2) = rnvblk (12)
        prjang (3) = rnvblk (13)
	gltln (1) = rnvblk (7)
	gltln (2) = rnvblk (8)
	gltln (3) = rnvblk (9)
	gltln (4) = rnvblk (10)
	kx = rnvblk (5)
	ky = rnvblk (6)
	CALL GR_RBAN  ( anlblk, deltan, deltax, deltay, grltln, 
     +		        eltln,  dltln,  iextnd, ier )
	kex = kx + iextnd (1) + iextnd (3)
	key = ky + iextnd (2) + iextnd (4)
C*
	RETURN
	END
