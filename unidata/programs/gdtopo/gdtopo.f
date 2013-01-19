	PROGRAM  GDTOPO      
C************************************************************************
C* PROGRAM GDTOPO							*
C*									*
C* This program will read topography data from a 5-minute database, 	*
C* create a GEMPAK grid file, and write the data to the grid file.	*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	11/91						*
C* S. Jacobs/SSAI	12/91	Removed option to interpolate topo	*
C*				data to user's grid file.		*
C* J. Whistler/SSAI	 6/92	Changed file access			*
C* P. Bruehl/Unidata	 1/95	Fixed calls to FL_INQR for 5.2		*
C* S. Chiswell/Unidata	 3/04	Updated for GEMLIB changes		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gdattim*48, garea*48, 
     +			topofl*(LLMXLN), dummy*(LLMXLN),
     +			terrain(3)*(LLMXLN), ijskip*(LLMXLN),
     +			gvcord*(LLMXLN), gfunc*(LLMXLN)
C*
	REAL		gltln(4)
	CHARACTER	time(2)*20, tpfile*(LLMXLN), gvc*4, vparm*4
	LOGICAL		respnd, done, proces, exist
C
C
C*	Known data files DEM5, DEM30, LAND1
C
	DATA		terrain(1) / 'world_topo.5min' /
	DATA		terrain(2) / 'world_topo.30s' /
	DATA		terrain(3) / 'gl-latlong-1km-landcover' /
C-----------------------------------------------------------------------
C
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'GDTOPO', ier )
	IF  ( iperr .eq. 0 )  THEN
	    mode = 1
	    CALL GG_INIT  ( mode, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and compute diagnostics.
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the TAE.
C
	    CALL GDTINP ( gdfile, garea, gdattim, gvcord, gfunc, topofl,
     +			  ijskip, iperr )
C
C*	    Exit if there is an error.
C
	    IF  ( iperr .ne. 0 )  THEN
		done = .true.
	    ELSE
C
		time(1) = gdattim(:20)
		time(2) = ' '
C
C*		Set the topography file number.
C
		CALL ST_LCUC ( topofl, dummy, ier )
C
		IF ( dummy(1:5) .eq. 'DEM30' ) THEN
		   tpfile = terrain(2)
		ELSE IF ( dummy(1:4) .eq. 'DEM5' ) then
		   tpfile = terrain(1)
		ELSE IF ( dummy(1:5) .eq. 'LAND1' ) then
		   tpfile = terrain(3)
		ELSE
		   tpfile = topofl
		END IF
C
		CALL FL_TINQ ( tpfile, 'unidata', exist, dummy, ier )
		IF ( .not. exist ) THEN
		    ier1 = -12
		    CALL ER_WMSG ( 'GDTOPO', ier1, ' ', ier )
		    proces = .false.
		ELSE
		    CALL FL_INQR ( dummy, exist, tpfile, ier )
		END IF
C
		IF  ( proces )  THEN
C
C
C*		    If the grid file does not exist then read the
C*		    topography data and create the grid file...
C
		    CALL ST_NULL ( ijskip, ijskip, lens, ier)
		    CALL ST_NULL ( tpfile, tpfile, lens, ier)
		    CALL read_dem ( tpfile, ijskip, garea, ikx, iky,
     +				gltln, rmin, rmax, ier1 ) 
C
		    IF  ( ier1 .ne. 0 )  THEN
			CALL ER_WMSG ( 'GDTOPO', ier1, ' ', ier )
		    ELSE
C
C
C*			Give the user a chance to exit.
C
			CALL GDTDSP ( gdfile, tpfile, garea, gltln,
     +				      ikx, iky, rmin, rmax, ier1 )
C
		    END IF
			IF  ( ier1 .ne. 0 )  proces = .false.
C
		ELSE
C
C*		    ...otherwise, do nothing at present.
C
		    CALL ER_WMSG ( 'GDTOPO', -13, ' ', ier )
C
		END IF
C
C*		Write the grid to the file.
C
		IF  ( proces )  THEN
C
C*		    Compute packing bits
C
		    rdif = rmax - rmin
		    rbits = ABS ( LOG (rdif) ) / LOG (2.0)
		    nbits = INT ( rbits + 1 )
		    IF (nbits .gt. 32) THEN
			nbits = 32
		    ELSE IF ( nbits .lt. 2 ) THEN
			nbits = 2
		    END IF
C
C*		    Write grid to file / Create if necessary
C
		    CALL ST_NULL ( time(1), time(1), lens, ier1)
		    CALL ST_NULL ( time(2), time(2), lens, ier1)
		    CALL ST_NULL ( gfunc, gfunc, lens, ier1)
		    CALL ST_LCUC ( gvcord(1:4), gvc, ier1 )
		    CALL LV_CORD ( gvc, vparm, ivcord, ier1)
		    CALL GDTFIL ( gdfile, ikx, iky, gfunc, time(1), 
     +			time(2), ivcord, nbits, gltln, ier1 )
		    IF  ( ier1 .ne. 0 )  proces = .false.
		END IF
C
C
	    END IF
C
C*	    Prompt for next diagnostic to be done.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'GDTOPO', iperr, ' ', ier )
	CALL IP_EXIT  ( ier )
C*
	STOP
	END
