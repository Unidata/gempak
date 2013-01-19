	PROGRAM SFCFIL
C************************************************************************
C* SFCFIL								*
C*									*
C* This program will create a surface data file.			*
C*									*
C* Log:									*
C* I. Graffman/RDS							*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* M. desJardins/GSFC	11/89	Set STMFLG to true so time can be saved	*
C* K. Brill/NMC          9/90   Changes for IN_PRMF			*
C* S. Jacobs/EAI	 7/92	Removed SS_EXIT from end of code	*
C* K. Brill/NMC		 8/93	Change for 8-char ID ISPRI		*
C* L. Williams/EAI	 7/94	Removed call to SFFUPD			*
C* D. Keiser/GSC	 4/96	Added parameter SFFSRC			*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER 	sffile*(LLMXLN), prmfil*(LLMXLN),
     +			stnfil*(LLMXLN), timstn*(LLMXLN)
C*
	CHARACTER	sffsrc*12, pfile*72
	LOGICAL		shipfl
C*
	LOGICAL		respnd, done, proces, pkflg
	CHARACTER	coun (LLSTFL)*2, stid (LLSTFL)*8
	CHARACTER	parms (MMPARM)*4, stat (LLSTFL)*2, sffstr (2)*4
	REAL 		slat (LLSTFL), slon (LLSTFL), selv (LLSTFL)
	INTEGER		ids (LLSTFL), ispri (LLSTFL), ilist (2)
	INTEGER         iscale (MMPARM), iofset (MMPARM),
     +                  ibits (MMPARM)
	CHARACTER	filstn*48
C-----------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SFCFIL', iperr, ' ', ier )
	    done = .true.
        ELSE
	    done = .false.
	END IF
	CALL IP_IDNT  ( 'SFCFIL', ier )
C
C*	Main loop.
C
	DO WHILE (.not. done)
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFFINP  ( sffile, prmfil, stnfil, shipfl, timstn,
     +			   sffsrc, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFCFIL', iperr, ' ', ier )
	    END IF
	    proces = .true.
C
C*	    Get the file source, if SFFSRC is blank, then iflsrc is 
C*	    set to zero, (i.e. -- missing). 
C
	    iflsrc = 0
	    pfile = prmfil
	    CALL ST_CLST  ( sffsrc, '|', ' ', 2, sffstr, n, ier )
	    IF      ( sffstr(1) .eq. 'AIRW' ) THEN
			iflsrc = MFAIRW
	    ELSE IF ( sffstr(1) .eq. 'METR' ) THEN
			iflsrc = MFMETR
	    ELSE IF ( sffstr(1) .eq. 'SHIP' ) THEN
			iflsrc = MFSHIP
	    ELSE IF ( sffstr(1) .eq. 'BUOY' ) THEN
			iflsrc = MFBUOY
	    ELSE IF ( sffstr(1) .eq. 'SYNP' ) THEN
			iflsrc = MFSYNP
	    ELSE IF ( sffstr(1) .eq. 'TEXT' ) THEN
			iflsrc = 100
			pfile = 'TEXT'
	    END IF
	    IF (( sffstr(2) .eq. 'TEXT') .and .( iflsrc .lt. 100 )) THEN
		iflsrc = iflsrc + 100
	    END IF
C
C*	    Get the number of times and the number of stations to
C*	    add.
C
	    CALL ST_ILST  ( timstn, '/', IMISSD, 2, ilist, n, ier )
	    IF  ( ilist (1) .eq. IMISSD )  THEN
		maxtim = 1
	      ELSE
		maxtim = ilist (1)
	    END IF
	    IF  ( ilist (2) .eq. IMISSD )  THEN
		iadstn = 0
	      ELSE
		iadstn = ilist (2)
	    END IF
C
C*	    Get the stations from the table file provided that this
C*	    is not a ship file.
C
	    filstn = ' '
	    nstns  = 0
	    IF  ( shipfl .and. ( stnfil .ne. ' ' ) )  THEN
		CALL ER_WMSG  ( 'SFCFIL', +1, stnfil, ier )
C
C*		Get station information for station file.
C
	      ELSE IF  ( stnfil .ne. ' ' )  THEN
		CALL SFFSFL  ( stnfil, nstns, stid, ids, slat, slon, 
     +			       selv, stat, coun, ispri, ier )
		IF  ( ier .eq. 0 )  filstn = stnfil
	    END IF
C
C*	    Write error message for invalid # of stations and times.
C
	    IF  ( iadstn .lt. 0 )  THEN
		CALL ER_WMSG  ( 'SFCFIL', +2, ' ', ier )
		iadstn = 0
	    END IF
	    nst = nstns + iadstn
	    IF  ( nst .eq. 0 )  THEN
		CALL ER_WMSG  ( 'SFCFIL', -4, ' ', ier )
		proces = .false.
	    END IF
	    IF  ( maxtim .le. 0 )  THEN
		CALL ER_WMSG  ( 'SFCFIL', -5, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Check that parameter packing information exists.
C
	    CALL IN_PRMF ( pfile, nparm, parms, iscale, iofset,
     +                     ibits, pkflg, ier )
	    IF  ( ier .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFCFIL', -6, prmfil, ier )
		proces = .false.
	    END IF
C
C*	    Check that output file name is not blank.
C
	    CALL ST_LSTR  ( sffile, lensff, ier )
	    IF  ( lensff .eq. 0 )  THEN
		CALL ER_WMSG  ( 'SFCFIL', -7, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Give the user a chance to exit.
C
	    IF  ( proces )  THEN
		CALL SFFOPT  ( sffile, prmfil, filstn, nstns, iadstn,
     +			       nst, maxtim, shipfl, ier )
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Create the file.
C
	    IF  ( proces )  THEN
		IF  ( shipfl )  THEN
		    maxrpt = nst * maxtim
	            CALL SF_CSDF  ( sffile, iflsrc, nparm,  parms,
     +				    maxrpt, pkflg, iscale, iofset,
     +			            ibits, .true., isffln, iret)
		  ELSE
	            CALL SF_CREF  ( sffile, iflsrc, nparm, parms, nst,
     +			            maxtim, pkflg, iscale, iofset,
     +			            ibits, .true., isffln, iret)
		END IF
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Now add the stations to the file.
C
	    IF  ( proces .and. ( nstns .gt. 0 ) )  THEN
		CALL SF_ASTN  ( isffln, nstns, stid, ids, slat, slon, 
     +				selv, stat, coun, ispri, nadd, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SF', iret, ' ', ier )
		  ELSE
		    WRITE ( 6, * ) 'The file has been created.'
		END IF
	    END IF
C
C*	    Close the surface file and update global parameters.
C
	    IF  ( proces )  THEN
		CALL SF_CLOS  ( isffln, ier )
	    END IF
C
C*	    Call the dynamic tutot.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
C*
	END
