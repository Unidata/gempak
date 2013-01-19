	PROGRAM SFCHCK 
C************************************************************************
C* SFCHCK								*
C* 									*
C* This program opens a GEMPAK surface data file and searches station-	*
C* by-station to determine if data is available for each hour in the	*
C* file.  Stations not in the station table can also be displayed.  A 	*
C* table is output, listing the reporting characterstics of each 	*
C* requested station.							*
C**									*
C* Log:									*
C* K. Tyle/GSC		 4/97						*
C* K. Tyle/GSC		 4/97	Write to file only if "proces" = true,	*
C*				write error message if no station 	*
C*				found; change 'non' to 'rep'; 		*
C*				allow display of WMO ID's; rewrote	*
C*				documentation				*
C* K. Tyle/GSC		 5/97	Added calls to SF_QSPC and SF_QTXT	*
C* K. Tyle/GSC		 5/97	Use ERMISS to check unlisted stations	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* A. Hardy/GSC		 3/99   Added priority parameter to SF_SNXT     *
C* A. Hardy/GSC		 3/99   Removed ispri = 0			*
C* J. Wu/GSC             7/00   Moved INCLUDE 'ERMISS.FNC' before the   *
C*                              DATA statement                          *
C* R. Curtis/EAI	10/00   Increased string length from 132 to 512 *
C* D. Kidwell/NCEP	10/00   Added 2nd header line;corrected prologue*
C* S. Jacobs/NCEP	 7/03	Changed arrays from LLSTFL to MMHDRS	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	sffile*(LLMXLN), output*(LLMXLN),
     +			keynam*(LLMXLN), dattim*(LLMXLN),
     +			stntyp*(LLMXLN), filnam*(LLMXLN),
     +			area*(LLMXLN), arecur*(LLMXLN), sffcur*(LLMXLN),
     +			datcur*(LLMXLN)
C*
	CHARACTER	head*512, times(LLMXTM)*20, outdev(4)*1,
     +			sta*8, flg(MMHDRS)*1, string*512, tstr*4, tot*4,
     +			starr ( MMHDRS )*8, stid*8, stmp*8, head2*512
	INTEGER		luns (4) 
	LOGICAL		respnd, done, proces, newfil, missing,
     +			datflg ( MMHDRS, LLMXTM ), rep, miss, unlst,
     +			list, unlist(MMHDRS), wflg, found, dtflg,
     +			txflg, spflg
C*
	INCLUDE		'ERMISS.FNC'
	DATA		sffcur, datcur, arecur / 3 * ' ' /
C------------------------------------------------------------------------
C*	Initialize user interface.
C
	CALL IP_INIT ( respnd, iperr )
	IF ( iperr .ne. 0 ) THEN
	    CALL ER_LMSG ( 0, 'SFCHCK', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT ( 'SFCHCK', ier )
C
C*	Main loop.
C
	done  = .false.
	iflno = 0
	DO WHILE ( .not. done )
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFKINP ( sffile, output, area, dattim, keynam, 
     +			  stntyp, iperr )
	    IF ( iperr .ne. 0 ) THEN
		CALL ER_LMSG ( 0, 'SFCHCK', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Make sure keynam is capitalized.
C
	    CALL ST_LCUC ( keynam, keynam, ier )
	    IF ( keynam .ne. 'STNM' ) keynam = 'STID'
C
C*	    Open the data file.
C
	    CALL FL_MFIL ( sffile, ' ', filnam, iret )
	    IF ( iret .ne. 0 ) 
     +		CALL ER_LMSG ( 0, 'FL', iret, ' ', ier )
	    CALL SFKOSF ( filnam, sffcur, iflno, newfil, iret )
	    IF ( iret .ne. 0 ) proces = .false.
C
C*	    Process area name.
C
	    IF ( proces ) THEN
	    	CALL SF_UARE ( iflno, area, newfil, arecur,
     +			       sta, ier )
		IF ( ier .ne. 0 ) proces = .false.
	    END IF
C
C*	    Select type of stations to display.
C
	    IF ( proces ) THEN
		CALL IN_STYP ( stntyp, rep, miss, unlst, list, ier)
		IF ( ier .ne. 0 ) THEN 
		    proces = .false.
		    CALL ER_WMSG ( 'IN', ier, 'STNTYP', ierr )
		END IF
	    END IF
C
C*	    Get the times to use.
C
	    IF ( proces ) THEN
		CALL SFKDAT ( iflno, dattim, newfil, datcur, ntime,
     +			      times, ier )
		CALL TI_SORT ( ntime, times, times, ier )
		IF ( ier .ne. 0 ) proces = .false.
	    END IF
C
C*	    Set up output device.
C
	    IF ( proces ) THEN
		CALL IN_OUTT ( output, 'SFCHCK', luns, nlun,
     +			       outdev, ier )
	    END IF
C
C*	    Set up output header if we are OK to here.
C
	    IF ( proces ) THEN
     		CALL SFKHDR ( times, ntime, head, head2, iret )
		CALL ST_LSTR ( head, lenr, ier )
		CALL ST_LSTR ( head2, lenr2, ier )
		DO ilun = 1, nlun
		    WRITE ( luns ( ilun ), '(A)') head ( : lenr )
		    IF ( lenr2 .gt. 0 ) WRITE ( luns ( ilun ), '(A)') 
     +					head2 ( : lenr2 )
		    WRITE ( luns ( ilun ), '(A)') 
		END DO
		i = 1
		found = .false.
	    END IF
C
C*	    Loop through times finding data.
C
	    DO WHILE ( ( i .le. ntime ) .and. proces )
	    	CALL SF_STIM ( iflno, times (i), ier )
		IF ( ier .eq. 0 ) THEN
		    iout = 0
		    ns   = 0
		    stmp = ' '
		    DO WHILE ( iout .eq. 0 )
			CALL SF_SNXT ( iflno, sta, id, slat, slon, 
     +				       selv, ispri, iout )
			IF ( iout .eq. 0 ) THEN
			    ns = ns + 1	
			    IF ( i .eq. 1 ) THEN
				IF ( keynam .eq. 'STNM' ) THEN
				    CALL ST_INCH ( id, stid, ier )
                                    CALL ST_LSTR ( stid, lens, ier )
C
C*				    Check for station id's that begin
C*				    with one or more '0's.
C
				    IF ( lens .eq. 5 ) THEN
					stmp (1:1) = '0'
					stmp (2:8) = stid(1:7)
					stid = stmp
				      ELSE IF ( lens .eq. 4 ) THEN
					stmp (1:2) = '00'
					stmp (3:8) = stid (1:6)
					stid = stmp
				    END IF
				    starr ( ns ) = stid
				  ELSE
				    starr ( ns ) = sta
				END IF
C
C*				Check for missing latitude, longitude, and
C*				elevation.  Only check once per station.
C
				IF ( ( ERMISS ( slat ) ) .and.
     +				     ( ERMISS ( slon ) ) .and.
     +				     ( ERMISS ( selv ) ) ) THEN
				    unlist ( ns ) = .true.
				    flg ( ns ) = '#'
				  ELSE
				    unlist ( ns ) = .false.
				    flg ( ns ) = ' '
				END IF
			    END IF
C
C*			    Check for data.
C			
			    CALL SF_QDAT (iflno, dtflg,  ier )
			    CALL SF_QTXT (iflno, txflg,  ier )
			    CALL SF_QSPC (iflno, spflg,  ier )
			    IF ( dtflg .or. txflg .or. spflg ) THEN
				datflg ( ns, i ) = .true.
			      ELSE
				datflg ( ns, i ) = .false.
			    END IF
			END IF
		    END DO
		    i = i + 1
		END IF
	    END DO
	    IF ( proces ) THEN
C
C*		Build output string for each station.
C
		DO i = 1, ns
		    itot = 0
		    string = ' '
C
C*		    Remove leading 'C' or 'K' for 4-char ID's.
C
		    CALL ST_LSTR ( starr(i), len, ier)
		    IF ( len .eq. 4 .and. ( starr (i) (1:1) .eq. 
     +			 'C' .or. starr (i) (1:1) .eq. 'K' ) ) THEN
			tstr = starr (i)
			tstr (1:3) = starr(i)(2:4)
			tstr (4:4) = ' '
			starr(i)   = tstr
		    END IF
		    string (3:10) = starr (i)
		    missing = .true.
		    DO j = 1, ntime
			ipos = 3 * ( j - 1 ) + 12 
			IF ( datflg ( i, j )) THEN
			    itot = itot + 1
			    string ( ipos:ipos ) = '+'
			    missing = .false.
			  ELSE
			    string ( ipos:ipos ) = '-'
			END IF
		    END DO
		    IF ( missing ) THEN
			flg (i) = 'N'
		    END IF
		    string (1:1) = flg(i)
		    CALL ST_INCH ( itot, tot, ier )
		    string ( ntime*3 + 12 : ntime*3+14 ) = tot
		    CALL ST_LSTR ( string, lenr, ier )
		    DO ilun = 1, nlun
C
C*			Determine which stations will appear in table.
C
			wflg = .false.
			IF ( ( rep   .and. miss ) .or. 
     +			     ( unlst .and. list ) ) THEN
			    wflg = .true.
			  ELSE IF ( rep .and. unlst ) THEN
			    IF ( ( .not. missing ) .and. unlist(i) )
     +				wflg = .true.
			  ELSE IF ( rep .and. list ) THEN
			    IF ((.not. missing ).and.(.not. unlist(i) ))
     +				wflg = .true.
			  ELSE IF ( miss .and. unlst ) THEN
			    IF ( missing .and. unlist(i) )
     +				wflg = .true.
			  ELSE IF ( miss .and. list ) THEN
			    IF ( missing .and. (.not. unlist(i) ) )
     +				 wflg = .true.
			  ELSE IF ( rep ) THEN
			    IF ( .not. missing ) wflg = .true.
			  ELSE IF ( miss ) THEN 
			    IF ( missing ) wflg = .true.
			  ELSE IF ( unlst ) THEN
			    IF ( unlist(i) ) wflg = .true.
			  ELSE IF ( list ) THEN
			    IF ( .not. unlist(i) ) wflg = .true.
			END IF
			IF ( wflg ) THEN
			    WRITE ( luns ( ilun ), '(A)' ) 
     +				    string ( : lenr )
			    found = .true.
			END IF
		    END DO
		END DO
C
C*		Write an error message if no stations were found.
C
		IF ( .not. found )
     +		    CALL ER_LMSG ( 0, 'SFCHCK', -3, ' ' , ier )
	    END IF
C
	    CALL IP_DYNM ( done, ier )
	END DO
C
C*	Exit.
C
	CALL IP_EXIT ( iret )
	END

C
