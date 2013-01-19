	PROGRAM  GDCSV
C************************************************************************
C* PROGRAM GDCSV							*
C*									*
C* Outputs any gridded quantity with location information		*
C* prints the output.                                                   *
C*									*
C**									*
C* Log:									*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(LLMXLN)	gdfile, gdatim, scale, gfunc,
     +                  glevel, gvcord, pfunc, parm, garea, proj,
     +			 output, garout, prjout
C
	REAL	        hgrd(LLMXGD), anlblk(LLNANL), rnvblk(LLNNAV)
	REAL		altln(4)
	INTEGER		ksubx(2), ksuby(2)
	CHARACTER	timfnd*36, trange*36, time(2)*20
	CHARACTER	devs(4)*1, astr*(LLMXLN)
	INTEGER		lev(2), luns(4), iarr(3)
	LOGICAL		respnd, done, proces, first, gottm, fill, subset
C*
C-----------------------------------------------------------------------
C*	Initialize TAE and GEMPLT.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'GDCSV', ier )
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
	    CALL GDPTIN  ( gdatim, garea, proj, gdfile, scale,
     +                     glevel, gvcord, gfunc, output, iperr )
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
		ELSE
		    CALL DG_QTMS ( 2, .true., timfnd, ntms,
     +                                 trange, iperr )
                    IF ( iperr .ne. 0 .or. ntms .gt. 1 ) 
     +			proces = .false.
                END IF
C
C*		Loop over times.
C
		itime = 1
		gottm = proces
		first=.true.
		DO WHILE ( gottm )
C
C*		    Get the next time to process from the time server.
C
		    CALL DG_NTIM ( .true., .false., time, gottm, iret )
		    proces = ( iret .eq. 0 .and. gottm )
		    IF ( iret .ne. 0 ) THEN
		        write (*,*) 'Time not found ',time(1)
		    ELSE
		        CALL TG_DUAL ( time, timfnd, iret )
		    END IF
C
C*
C
		    IF ( proces ) THEN
		        CALL DG_INXT ( .true., .true., time, iret )
		        proces = ( iret .eq. 0 )
		    END IF

		    IF ( proces ) THEN
C
C*		        Query reference grid navigation and analysis block.
C
		        CALL DG_QREF ( LLNANL, LLNNAV, anlblk, rnvblk, 
     +					mxgrd, iret )
C
C*		        Setup the grid subset that covers the graphics area.
C
		        fill = .false.
		        CALL GR_SUBA ( garea, fill, rnvblk, altln,
     +			    ksubx, ksuby, subset, iret)
 		        IF  ( iret .ne. 0 )  THEN
                            CALL ER_WMSG ( 'DG', iret, ' ', ier )
                            proces = .false.
                        END IF
                    END IF
C
		    IF ( proces ) THEN
C
C*			Call GG_MAPS to allow (HILO) functions to be computed
C*			that involve call to GPTVIS (GSMPRJ)
C
			CALL DG_FIXA ( garea, proj, garout, prjout, ier )
			CALL GG_MAPS ( prjout, garout, ' ', 
     +				idrpfl, iret )
C
C*			Do computation on subset domain only
C
			IF ( subset ) THEN
			    astr = ' '
			    write(astr, 1000) 0,ksubx(1),ksubx(2),0,
     +				ksuby(1),ksuby(2)

1000     FORMAT ( I1,';',I10,';',I10,'/',I1,';',I10,';',I10)
			    ipos = 1
			    DO ii = 1, len(astr)
			       IF ( astr(ii:ii) .ne. ' ' ) THEN
				  astr(ipos:ipos) = astr(ii:ii)
				   ipos = ipos + 1
				END IF  
			    END DO
			    DO ii=ipos,len(astr)
				astr(ii:ii) = ' '
			    END DO
			    CALL DG_SUBG ( astr, ix1, ix2, iy1, iy2, iret )
			ELSE
			    ksubx(1) = IMISSD
			    ksubx(2) = IMISSD
			    ksuby(1) = IMISSD
			    ksuby(2) = IMISSD
			END IF
C
C*			Get the output units.
C
			CALL IN_OUTT  ( output, 'GDCSV', luns, nlun,
     +				devs, iret )
C
			CALL CWRAP_GCALC ( timfnd, glevel, gvcord, 
     +				gfunc, ksubx, ksuby, scale, nlun, luns, 
     +				devs, iret)
		    END IF
		END DO

		CALL IP_DYNM ( done, ier )
	    END IF
	END DO
	CALL GENDP ( 1, iret )
	CALL IP_EXIT ( iret )

	END
