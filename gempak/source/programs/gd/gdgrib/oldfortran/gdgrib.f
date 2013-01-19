	PROGRAM  GDGRIB      
C************************************************************************
C* PROGRAM GDGRIB							*
C*									*
C* This programs computes a grid diagnostic and adds it to a GRIB	*
C* file.								*
C*									*
C**									*
C* Log:									*
C* K. Brill/HPC          8/99   Created from GDDIAG			*
C* K. Brill/HPC		11/99	Fix final error message output		*
C* K. Brill/HPC		 2/00	Added "GRIB message written." output	*
C* K. Brill/HPC		 2/00	Added CPYFIL				*
C* K. Brill/HPC		 3/00	Pass NCNTR to GDGWMO; add NAVCHG	*
C* K. Brill/HPC		10/02	LLMXGD*15 -> LLMXGD*6 for grdo		*
C* T. Lee/SAIC		 7/03	Bypass diagnosis for existing grids	*
C* R. Tian/SAIC		 1/04	Added nuflg to DG_INTL call		*
C* R. Tian/SAIC		 2/04	Removed nuflg from DG_INTL call		*
C* R. Tian/SAIC		 4/05	Changed for time/file mgmt		*
C* S. Gilbert/NCEP	 6/05	Added call to GD_RDAT before DG_GRID	*
C*				to be able to read grids > LLMXGD	*
C* T. Lee/SAIC		12/05	Optimized arrays dimension		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( MXLPDS = 64 )
C!					Maximum PDS length
	PARAMETER	( MXLGDS = 128 )
C!					Maximum GDS length
	PARAMETER	( MXLBMS = LLMXTG / 8 + 8  )
C!					Maximum Bit Map length
	PARAMETER	( MXLBDS = LLMXTG * 2 + 14 )
C!					Maximum BDS length
C*
	CHARACTER	gdfile*(LLMXLN), gfunc*(LLMXLN),
     +			gdatim*(LLMXLN), glevel*(LLMXLN),
     +			gvcord*(LLMXLN), gbtbls*(LLMXLN),
     +			gbfile*(LLMXLN), vercen*(LLMXLN),
     +			pdsval*(LLMXLN), precsn*(LLMXLN),
     +			wmohdr*(LLMXLN), proj*(LLMXLN),
     +			gdarea*(LLMXLN), kxky*(LLMXLN),
     +			cpyfil*(LLMXLN)
C*
	CHARACTER	pfunc*80, filnam*256, firstm*20, lasttm*20
	CHARACTER	gbfcur*(LLMXLN)
	CHARACTER*1	cpds (MXLPDS), cgds (MXLGDS), cbms (MXLBMS),
     +			cbds (MXLBDS), csc0 (8)
	CHARACTER*4	csc5
C*
	INTEGER		ibyts (3), level (2), ighdr (LLGDHD)
	REAL		grid (LLMXTG), rnavo (LLNNAV), dum
	REAL		grdo (LLMXTG), rnvblk (LLNNAV)
	CHARACTER       gvc*4, vparm*4
	CHARACTER	time (2)*20, parm*12, chdr*22, cdd*2, chhmm*4
	LOGICAL		respnd, done, proces, makbms, navchg, nxtok
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'GDGRIB', ier )
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
C*	Initialize the DG library.
C
	CALL DG_INTL ( ier )
C
C*	Main loop to read in user parameters and compute diagnostics.
C
	gbfcur = ' '
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the user interface.
C
	    CALL GDGUIN  ( gdfile, gfunc, gdatim, glevel, gvcord,
     +			   gbtbls,
     +			   gbfile, vercen, pdsval, precsn, wmohdr,
     +			   cpyfil, proj, gdarea, kxky, iperr )
C
C*	    Exit if there is an error.
C
	    IF  ( iperr .ne. 0 )  THEN
		done = .true.
	    ELSE
C
C*		Open the output GRIB file if it is a new file.
C
		IF ( gbfile .ne. gbfcur ) THEN
		    IF ( gbfcur .ne. ' ' ) CALL GBF_CLOS ( ier )
		    CALL ST_LSTR ( gbfile, lng, ier )
		    gbfile (lng+1:lng+1) = CHAR (0)
		    CALL GBF_AOPN ( gbfile, ier )
		    IF ( ier .eq. 0 ) THEN
			gbfcur = gbfile
		      ELSE
			proces = .false.
		    END IF
		END IF
C
C*		Process GDFILE.
C
		IF ( proces ) THEN
                    CALL DG_NFIL ( gdfile, ' ', iret )
                    IF ( iret .ne. 0 ) THEN
                        CALL ER_WMSG ( 'DG', iret, gdfile, ier )
                        proces = .false.
                    END IF
		END IF
C
C*              Process GDATIM.
C
		IF ( proces ) THEN
                    CALL DG_NDTM ( gdatim, iret )
                    IF ( iret .ne. 0 ) THEN
                        CALL ER_WMSG ( 'DG', iret, gdatim, ier )
                        proces = .false.
                    END IF
		END IF
C
C*              Get time.
C
                IF ( proces ) THEN
                    CALL DG_NTIM ( .true., .false., time, nxtok, iret )
                    IF ( iret .ne. 0 ) THEN
                        CALL ER_WMSG ( 'DG', iret, ' ', ier )
                    END IF
                    proces = ( nxtok .and. iret .eq. 0 )
                    CALL TG_DUAL ( time, gdatim, iret )
                END IF
                CALL DG_QDTM ( 1, firstm, lasttm, iret )
                CALL DG_QREF ( 0, LLNNAV, adum, rnvblk, maxgrd, iret )
C
C*		Read the grid directly from the file.
C
		IF  ( proces )  THEN
                    CALL GR_LEVL ( glevel, level (1), level (2), ier )
                    CALL ST_LCUC ( gvcord, vparm, ier )
                    CALL LV_CORD ( vparm, gvc, ivcord, ier ) 
                    CALL GD_NGRD ( igdfln, numgrd, firstm, lasttm, ier )
                    CALL GR_GTIM ( gdatim, firstm, lasttm, time (1),
     +                             time (2), ier )
                    CALL ST_LCUC ( gfunc, parm, ier )
                    CALL FL_MFIL ( gdfile, ' ', filnam, iret )
                    CALL GD_OPEN ( filnam, .false., 0, 0, igdfln, dum,
     +                             dum, mxgrd, iret )
                    CALL GD_RDAT ( igdfln, time, level, ivcord, parm,
     +                             grid, igx, igy, ighdr, iret )

                    IF  ( iret .ne. 0 )
     +		        CALL DG_GRID  ( gdatim, glevel, gvcord, gfunc,
     +				        pfunc, grid, igx, igy, time,
     +				        level, ivcord, parm, iret )
C
C*		    Interpolate to output grid.
C
		    IF ( iret .eq. 0 ) THEN
			CALL GDGINT ( grid, igx, igy, rnvblk,
     +				      cpyfil, proj, gdarea, kxky,
     +				      grdo, nxo, nyo, rnavo, igpds,
     +				      navchg, ier )
			IF ( ier .ne. 0 ) THEN
			    CALL ER_WMSG ( 'GDGRIB', ier, ' ', irr )
			    proces = .false.
			END IF
		    ELSE
			CALL ER_WMSG  ( 'DG', iret, pfunc, ier )
			proces = .false.
		    END IF
		    nbbms = 0
C
C*		    First generate the GDS.
C
		    IF ( proces ) THEN
			nbgds = MXLGDS
			CALL GDS_MAK ( navchg, rnavo, 16, nbgds, cgds,
     +				       ier )
		    END IF
		    IF ( proces .and. ier .ne. 0 ) THEN
			CALL ER_WMSG ( 'GDGRIB', ier , ' ', irr )
			proces = .false.
		    END IF
C
C*		    Second, generate the BDS.
C
		    IF ( proces ) THEN
			nbbds = MXLBDS
			CALL BDS_MAK ( nxo, nyo, precsn, grdo, nbbds,
     +				       cbds, ip10sc, makbms, ier )
		    END IF
		    IF ( proces .and. ier .ne. 0 ) THEN
			CALL ER_WMSG ( 'GDGRIB', ier , ' ', irr )
			proces = .false.
		    END IF
C
C*		    Third, generate the BMS, if needed.
C
		    IF ( proces .and. makbms ) THEN
			nbbms = MXLBMS
			CALL BMS_MAK ( grdo, nxo, nyo, nbbms,
     +				       cbms, ier )
		    END IF
		    IF ( proces .and. ier .ne. 0 ) THEN
			CALL ER_WMSG ( 'GDGRIB', ier , ' ', irr )
			proces = .false.
		    END IF
C
C*		    Now generate the PDS.
C
		    IF ( proces ) THEN
			nbpds = MXLPDS
			CALL GDGPDS ( pdsval, vercen, rnavo, parm,
     +				      ivcord, level, makbms, ip10sc,
     +				      lasttm, time, gbtbls, igpds,
     +				      nbpds, cpds, cdd, chhmm, ier )
		    END IF
		    IF ( proces .and. ier .ne. 0 ) THEN
			CALL ER_WMSG ( 'GDGRIB', ier , ' ', irr )
			proces = .false.
		    END IF
C
C*		    Finally make the WMO header.
C
		    IF ( proces ) THEN
			ncntr = ICHAR ( cpds (5) )
			CALL GDGWMO ( wmohdr, ncntr, cdd, chhmm, chdr,
     +				      ier )
		    END IF
		    IF ( proces .and. ier .lt. 0 ) THEN
			CALL ER_WMSG ( 'GDGRIB', ier , ' ', irr )
			proces = .false.
		    ELSE IF ( ier .gt. 0 ) THEN
			CALL ER_WMSG ( 'GDGRIB', ier , ' ', irr )
		    END IF
C
C*		    Write the grid to the file.
C
		    IF  ( proces )  THEN
C
C*		        Make section 0.
C
			itot = 8 + nbpds + nbgds + nbbms + nbbds + 4
			csc0 (1) = 'G'
			csc0 (2) = 'R'
			csc0 (3) = 'I'
			csc0 (4) = 'B'
			ii = 4
			nb = 3
			CALL GDIGIT ( itot, 256, nb, ibyts, ier )
			IF ( ier .ne. 0 ) THEN
			    ier = -8
			    CALL ER_WMSG ( 'GDGRIB', ier , ' ', irr )
			    proces = .false.
			END IF
			DO i = 3, 1, -1
			    ii = ii + 1
			    csc0 (ii) = CHAR ( ibyts (i) )
			END DO
			csc0 (8) = CHAR (1)
			csc5 = '7777'
C
C*			Write out all sections, as needed.
C
			IF ( chdr .ne. ' ' .and. proces ) THEN
			    CALL GBF_WRIT ( 21, chdr, ier )
			END IF
			IF ( ier .eq. 0 .and. proces ) THEN
			    CALL GBF_WRIT ( 8, csc0, ier )
			END IF
			IF ( ier .eq. 0 .and. proces ) THEN
			    CALL GBF_WRIT ( nbpds, cpds, ier )
			END IF
			IF ( ier .eq. 0 .and. proces ) THEN
			    CALL GBF_WRIT ( nbgds, cgds, ier )
			END IF
			IF ( ier .eq. 0 .and. makbms .and. proces ) THEN
			    CALL GBF_WRIT ( nbbms, cbms, ier )
			END IF
			IF ( ier .eq. 0 .and. proces ) THEN
			    CALL GBF_WRIT ( nbbds, cbds, ier )
			END IF
			IF ( ier .eq. 0 .and. proces ) THEN
			    CALL GBF_WRIT ( 4, csc5, ier )
			END IF
			IF ( ier .eq. 0 .and. proces ) THEN
			    WRITE (6,*) ' GRIB message written.'
			END IF
		    END IF
		END IF
C
C*		Prompt for next diagnostic to be done.
C
		CALL IP_DYNM  ( done, ier )
	    END IF
	END DO
	IF ( gbfcur .ne. ' ' ) CALL GBF_CLOS ( ier )
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'GDGRIB', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
C*
	END
