	PROGRAM  GDPVSF      
C************************************************************************
C* PROGRAM GDPVSF							*
C*									*
C* This program interpolates grid data to a functional surface (such as *
C* a potential vorticity surface.					*
C*									*
C* Log:									*
C*	J. Nielsen/SUNYA   12/90	from GDDIAG and GDPROF		*
C*	D. Knight	    9/94	modified for gempak5.2          *
C*	J. N-G/TAMU	   12/97	Cleaned up, fixed bugs		*
C*	J. N-G/TAMU	    9/98	Allow flex vert coord		*
C*	D. Knight/UAlbany   3/00        CALL ST_LCUC before LV_CORD     *
C*      K.Tyle/UAlbany      4/03	Remove "GEMINC:" in INCLUDE	*
C*      K.Tyle/UAlbany      2/08	DG_OFIL --> DG_NFIL        	*
C*      K.Tyle/UAlbany      2/08	DG_OFIL --> DG_NFIL        	*
C*      M. James/Unidata    1/13	Added gottm             	*
C*                              	Call GD_INIT and DG_INTL      	*
C*                                      Call DG_NDTM and DG_NTIM       	*
C*                                      GD_NGRD --> DG_QDTM         	*
C*                                      iflinp --> igdfln for GDPVLV    *
C*                                      Added calls to GD_OPEN/GD_CLOS  *
C*                                      Fixed search on GVCORD boundary *
C*                                      Fixed search on GVCORD boundary *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gfunc*(LLMXLN),gdatim*(LLMXLN),
     +			gvcord*(LLMXLN), gdoutf*(LLMXLN),gpack*(LLMXLN),
     +			cstop*24, cdesir*24, firstm*20, lastim*20,
     +                  time(2)*20, gname*12, cglist*(LLMXLN), cpmax*12,
     +			ovcord*(LLMXLN), cgdoul*24, cstart*24
	CHARACTER	glist(12)*12
C*
	INTEGER		ighdr(10), gdoutl (2)
	REAL		ogrids (LLMXGD,13), desire,
     +			startl, stopl
	LOGICAL		respnd, done, proces, gottm
	DATA		ighdr /10*0/
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDPVSF', iperr )
C
C*      Initialize GEMPLT
C
            mode = 1
            CALL GG_INIT ( mode, ier )
	    IF  ( ier .eq. 0 ) THEN
	        CALL GD_INIT ( ier )
	        CALL DG_INTL ( ier ) 
	        done = .false. 
	    ELSE
	        done = .true.
	    END IF
	ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters and compute diagnostics.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL GDPVIN  ( gdfile, gdoutf, gdatim, gvcord, gfunc, cdesir, 
     +			 cstart, cstop, cgdoul, gpack, cglist, cpmax, 
     +			 ovcord, iperr )
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'LV', iperr, ' ', ier )
	        done = .true.
	  ELSE
C
C*	    Convert to numbers.
C
	    CALL ST_RLST  ( cdesir, '/', 1., 1, desire, n, ier )
	    CALL ST_RLST  ( cstart, '/', 100., 1, startl, n, ier )
	    CALL ST_RLST  ( cstop, '/', 500., 1, stopl, n, ier )
	    CALL ST_ILST  ( cgdoul, '/', 1, 1, gdoutl(1), n, ier )
	    CALL ST_ILST  ( cpmax, '/', 0, 1, ipmax, n, ier )
	    CALL ST_CLST  ( cglist, ';', ' ', 12, glist, nlist, ier )
	    gdoutl(2) = -1
	    CALL ST_LCUC  ( ovcord, ovcord, ier)
	    CALL LV_CORD  ( ovcord, ovcord, icord, iverr )
	    IF  ( iverr .ne. 0 ) THEN
		CALL ER_WMSG  ( 'LV', iverr, ' ', ier )
		proces = .false.
		iret = ier
	    END IF
C
C*	    Open the grid files.
C
	    CALL DG_NFIL ( gdfile, gdoutf, iret )
	    IF  ( iret .ne. 0 ) THEN 
		CALL ER_WMSG  ( 'DG_NFIL', iret, ' ', ier )
	        proces = .false.
	    END IF 
C
C*          Processe GDATTIM
C
	    CALL DG_NDTM ( gdatim, iret )
	    IF  ( iret .ne. 0 ) THEN 
		CALL ER_WMSG  ( 'DG_NDTM', iret, ' ', ier )
	        proces = .false.
	    END IF 
C
C                make a call to DG_NTIM to advance to the next
C                (in this case first) time in the grid file...
C
            IF (proces) THEN
                CALL DG_NTIM ( .false., .false., time, gottm, ier )
                IF ( ier .ne. 0 ) THEN
                  CALL ER_WMSG ( 'DG', ier, ' ', irr )
                  proces = .false.
                END IF
            END IF
C
C*          Scan GFUNC for a file number.  Getting the file number here
C*          will assure that the time obtained by GDPVTD is present in
C*          the file, unless the user input is erroneous.
C
            CALL DG_FLNO  ( gfunc, igdfln, iret )
            IF  ( iret .ne. 0 )  proces = .false.
            CALL DG_QDTM  ( igdfln, firstm, lastim, ier )
            IF  ( ier .ne. 0 )  proces = .false.
C
C*	    Convert grid stuff to something useful.
C
	    IF  ( proces )  THEN
		CALL GDPVDT  ( gdatim, gvcord, gfunc, firstm, lastim,
     +				time, ivcord, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'GDPVDT', iret, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Compute the new grid.
C
	    IF  ( proces )  THEN
C
C*		Compute the pv surface grids.
C
		CALL GDPVLV  ( igdfln, gdatim, gvcord, gfunc, glist, nlist, 
     +				time, ivcord, startl, stopl, desire, ipmax,
     +				ogrids, igx, igy, igrids, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'DG', iret, 'gdpvlv', ier )
		    proces = .false.
		END IF
C
C*		Write the grids to the file.
C
		IF  ( proces )  THEN
		  CALL GR_PACK  ( gpack, ipktyp, nbits, ier ) 
		  IF  ( ivcord .eq. 1 )  THEN
		     gname = 'PRES'
		  ELSE IF  ( ivcord .eq. 2 )  THEN
		     gname = 'THTA'
		  ELSE
		     gname = 'HGHT'
		  END IF

		  CALL GD_OPEN ( gdoutf, .true., 0, 0, iflout, 
     +			bkanl, bknav, mxgrd, iret )
	 	  IF  ( igrids .ge. 1 ) THEN
C
C*		    Output the grids.
C
		    DO  i = 1, igrids
		      gname=glist(i)
		      CALL GD_WPGD  ( iflout, ogrids ( 1, i+1 ), igx, 
     +			igy, ighdr, time, gdoutl, icord, gname,
     +			.true., ipktyp, nbits, iret ) 
		      IF  ( iret .ne. 0 )  THEN
		        CALL ER_WMSG  ( 'DG', iret, 'gdpvlv', ier )
		        proces = .false.
		      END IF    
		    END DO
		    CALL GD_CLOS ( iflout, ier )
		  END IF
	    END IF
	END IF
C
C*	    Prompt for next diagnostic to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'GDPVSF-IP_EXIT', 
     +		iperr, ' ', ier ) 
	CALL IP_EXIT  ( iret )
C*
	END
