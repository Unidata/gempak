	PROGRAM SNCFIL
C************************************************************************
C* SNCFIL								*
C*									*
C* This program will create a sounding data file.			*
C*									*
C* Log:									*
C* I. Graffman/RDS							*
C* M. desJardins/GSFC	 6/88	Reorganized				*
C* M. desJardins/GSFC	 5/90	Cleaned up				*
C* K. Brill/NMC		 9/90   Changes for IN_PRMF			*
C* K. Brill/NMC		02/92	Error message for nparms < 2		*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* L. Williams/EAI	 7/94	Removed call to SNFUPD			*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* D. Kidwell/NCEP       2/01   Turned on trpflg in call to SN_CRUA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER 	snfile*(LLMXLN), prmfil*(LLMXLN),
     +			stnfil*(LLMXLN), timstn*(LLMXLN), mrgdat*(LLMXLN)
C*
	LOGICAL		respnd, done, proces, pkflg, mrgtyp
	CHARACTER	coun (LLSTFL)*2, stid (LLSTFL)*8
	CHARACTER	parms (MMPARM)*4, stat (LLSTFL)*2
	REAL 		slat (LLSTFL), slon (LLSTFL), selv (LLSTFL)
	INTEGER		ids (LLSTFL), ilist (2)
	INTEGER		iscale (MMPARM), iofset (MMPARM),
     +                  ibits (MMPARM)
	CHARACTER	filstn*48
C-----------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SNCFIL', iperr, ' ', ier )
	    done = .true.
	  ELSE
	    done = .false.
	    CALL IP_IDNT  ( 'SNCFIL', ier )
	END IF
C
C*	Main loop.
C
	DO WHILE (.not. done)
C
C*	    Get user input and exit if there is an error.
C
	    CALL SNFINP  ( snfile, prmfil, stnfil, mrgdat, timstn,
     +			   iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SNCFIL', iperr, ' ', ier )
		done = .true.
		proces = .false.
	      ELSE
		proces = .true.
C
C*		Decode input for MRGDAT.
C
		CALL IN_MRGD  ( mrgdat, mrgtyp, imtype, ier )
C
C*		Get the number of times and the number of stations to
C*		add.
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
C*		Get the stations from the table file.
C
		filstn = ' '
		nstns  = 0
		IF  ( stnfil .ne. ' ' )  THEN
		    CALL SNFSFL ( stnfil, nstns, stid, ids, slat, slon, 
     +				  selv, stat, coun, ier )
		    IF  ( ier .eq. 0 )  filstn = stnfil
		END IF
C
C*		Write error message for invalid # of stations and times.
C
		IF  ( iadstn .lt. 0 )  THEN
		    CALL ER_WMSG  ( 'SNCFIL', +2, ' ', ier )
		    iadstn = 0
		END IF
		nst = nstns + iadstn
		IF  ( nst .eq. 0 )  THEN
		    CALL ER_WMSG  ( 'SNCFIL', -4, ' ', ier )
		    proces = .false.
		END IF
		IF  ( maxtim .le. 0 )  THEN
		    CALL ER_WMSG  ( 'SNCFIL', -5, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Check that parameter packing file exists if this is a
C*	    merged file.
C
	    IF  ( mrgtyp .and. proces )  THEN
		CALL IN_PRMF  ( prmfil, nparm, parms, iscale,
     +                          iofset, ibits, pkflg, ier )
		IF  ( ier .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SNCFIL', -6, prmfil, ier )
		    proces = .false.
		END IF
	 	IF ( nparm .lt. 2 ) THEN
		    CALL ER_WMSG ( 'SNCFIL', -8, parms (1), ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Check that output file name is not blank.
C
	    IF  ( proces )  THEN
		CALL ST_LSTR  ( snfile, lensnf, ier )
		IF  ( lensnf .eq. 0 )  THEN
		    CALL ER_WMSG  ( 'SNCFIL', -7, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Give the user a chance to exit.
C
	    IF  ( proces )  THEN
		CALL SNFOPT  ( snfile, prmfil, filstn, nstns, iadstn,
     +			       nst, maxtim, mrgtyp, imtype, 
     +			       ier )
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Create the file.
C
	    IF  ( proces )  THEN
		IF  ( mrgtyp )  THEN
	            CALL SN_CREF ( snfile, 4, nparm, parms, nst,
     +                             maxtim, pkflg, iscale, iofset,
     +                             ibits, .false., isnfln, iret)
		  ELSE
		    CALL SN_CRUA  ( snfile, 4, imtype, nst, maxtim,
     +				    .true., .true., .true., isnfln,
     +				    ier )
		END IF
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Now add the stations to the file.
C
	    IF  ( proces .and. ( nstns .gt. 0 ) )  THEN
		CALL SN_ASTN  ( isnfln, nstns, stid, ids, slat, slon, 
     +				selv, stat, coun, nadd, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SN', iret, ' ', ier )
		  ELSE
		    WRITE ( 6, * ) 'The file has been created.'
		END IF
	    END IF
C
C*	    Close the sounding file and update global parameters.
C
	    IF  ( proces )  THEN
		CALL SN_CLOS  ( isnfln, ier )
	    END IF
C
C*	    Call the dynamic tutor.
C
	    IF  ( .not. done )  CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
	END
