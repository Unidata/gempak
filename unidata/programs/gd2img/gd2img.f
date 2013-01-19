	PROGRAM  GD2IMG
C************************************************************************
C* PROGRAM GD2IMG							*
C*									*
C* Interpolates any gridded quantity to the location specified and      *
C* prints the output.                                                   *
C*									*
C**									*
C* Log:									*
C* T.W. Barker/WR/MSO	 9/96	Created from GDTHGT			*
C* S. Chiswell/Unidata	11/03	Fixed typo in gr_sscl call		*
C* S. Chiswell/Unidata	 3/06	Aded DG_ONAV call			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(LLMXLN)	gdfile, gpoint, gdatim, scale, gfunc,
     +                  glevel, gvcord, pfunc, parm, proj, kxky,
     +			gdarea, cpyfil, cpytmp, cpyf(2), anlyss,
     +			filnam, satfil, calinfo, wmohdr
C
	REAL	        rgx(1), rgy(1), rlat(1), rlon(1),
     +			yy(1), rnvblk ( LLNNAV ),
     +			anlblk(LLNANL), rltln(4)
	CHARACTER	timfnd*36, cprj*10,
     +			time(2)*20, firstm*20, lastim*20
	INTEGER		lev(2)
	LOGICAL		respnd, done, proces, first, gintp, gottm, 
     +			wrtflg

	INCLUDE		'ERMISS.FNC'
C*
C-----------------------------------------------------------------------
C*	Initialize TAE and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'GD2IMG', ier )
	IF ( iperr .eq. 0 )  THEN
	    mode = 1
	    CALL GG_INIT  ( mode , iperr )
	    IF (iperr.eq.0) THEN
	        done = .false.
	    ELSE 
	        done = .true.
	    ENDIF
	ELSE
	    done = .true.
	END IF
C
C*	Initialize the DG library.
C
	CALL DG_INTL ( ier )
C
C*	Main loop to read in TAE parameters
C
	DO WHILE  ( .not. done )
C
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the TAE.
C
	    CALL GDPTIN  ( gdatim, gdfile, scale, glevel, gvcord, gfunc,
     +			proj, gdarea, kxky, cpyfil, satfil, calinfo, 
     +			wmohdr, iperr )
C
C*	    Exit if there is an error.
C
	    IF ( iperr .ne. 0 )  THEN
		done = .true.
	    ELSE
C
C*		Process the GDFILE input.
C
		CALL DG_NFIL ( gdfile, ' ', ier )
		IF ( ier .ne. 0 ) THEN
                        CALL ER_WMSG ( 'DG', ier, ' ', irr )
                        proces = .false.
                END IF
C
C*		Process the GDATTIM input; setup the time server.
C
		CALL DG_NDTM ( gdatim, ier )
		IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'DG', ier, gdatim, irr )
                    proces = .false.
                END IF
C
C*		Loop over times.
C
		itime = 1
		gottm = proces
		first=.true.
		DO WHILE ( gottm )
C
C*		   Get the next time to process from the time server.
C
		   CALL DG_NTIM ( .true., .true., time, gottm, iret )
		   proces = ( iret .eq. 0 .and. gottm )
		   IF ( iret .ne. 0 ) THEN
		      write (*,*) 'Time not found ',time(1)
		   END IF

		   CALL TG_DUAL ( time, timfnd, iret )
		   IF ( proces .and. first ) THEN
                       CALL DG_FLNO  ( gfunc, iflnos, ier )
                       CALL DG_QDTM ( iflnos, firstm, lastim, ier )
                       IF ( firstm .eq. ' ' ) THEN
                           proces = .false.
                       END IF
                   END IF

		   IF ( proces ) THEN
C
C*      Set a new output reference navigation 
C
		   CALL ST_CLST ( cpyfil, '|', ' ', 2, cpyf, num,
     +                              ier )
		   cpytmp = cpyf(1)
C
C*		   CASE 1: Build new navigation block from user input.
C
		   IF (cpytmp .eq. ' ') THEN
		       CALL GDCNAV ( proj, kxky, gdarea, cprj, kx, ky,
     +                                rltln, rnvblk, ier )
                        IF  ( ier .eq. 0 )  THEN
                           anlyss = '0/0;0;0;0'
                           CALL GDCANL ( anlyss, rnvblk, anlblk, ier )
                        END IF
C
C*                      CASE 2: Build new navigation and analysis blocks from
C*                      grid navigation table input.
C
                    ELSE IF  ( cpytmp(1:1) .eq. '#') THEN
                        CALL GDCTBL (cpytmp, gname, cprj, kx, ky, rltln,
     +                               rnvblk, anlblk, ier)
C
C*                      CASE 3: Get the navigation and analysis blocks from
C*                      the existing file.
C
                    ELSE
                        CALL FL_MFIL ( cpytmp, ' ', filnam, ier )
                        IF ( ier .eq. 0) THEN
			   wrtflg = .false.
                           CALL GD_OPEN ( filnam, wrtflg, LLNANL, 
     +				LLNNAV, iflno, anlblk, rnvblk, maxg, 
     +				ier)
                           IF ( ier .eq. 0) THEN
C
C*                            Depricated Call to gdclos
                              CALL GD_CLOS ( iflno, ier )
                              CALL GR_RNAV ( rnvblk, cprj, kx, ky, ier )
                           END IF
                        END IF
                    END IF

		    IF (ier .ne. 0) THEN
			proces = .false.
		    ELSE
			CALL ST_NULL ( timfnd, timfnd, nt, ier )
        		CALL ST_NULL ( glevel, glevel, nt, ier )
        		CALL ST_NULL ( gvcord, gvcord, nt, ier )
        		CALL ST_NULL ( gfunc,  gfunc, nt, ier )
        		CALL ST_NULL ( satfil, satfil, nt, ier )
        		CALL ST_NULL ( calinfo, calinfo, nt, ier )
        		CALL ST_NULL ( wmohdr, wmohdr, nt, ier )
C
			CALL DGCONAV ( rnvblk, timfnd, glevel, gvcord, 
     +				gfunc, kx,ky, satfil, calinfo, 
     +				wmohdr, iret )
        	    END IF
		    END IF
C
                END DO
C
		CALL IP_DYNM ( done, ier )
	    END IF
	END DO
	CALL GENDP ( 1, iret )
	CALL IP_EXIT ( iret )

	END
