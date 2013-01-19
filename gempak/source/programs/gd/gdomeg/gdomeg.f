	PROGRAM  GDOMEG
C************************************************************************
C* PROGRAM GDOMEG							*
C*									*
C* This program computes OMEG using the O'brien technique and stores	*
C* grids of OMEG in the grid file.  The grid file must contain winds	*
C* on pressure levels.							*
C*									*
C**									*
C* Log:									*
C* K. Brill/NMC		11/90						*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* L. Williams/EAI	 7/94	Removed call to GDAUPD			*
C* S. Jacobs/NCEP	 5/96	Changed GR_FILE to DG_FILE		*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
C* R. Tian/SAIC		 1/05	Applied time/file mgmt			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gdatim*(LLMXLN), gpack*(LLMXLN)
C*
	LOGICAL		respnd, done, proces, gottm
	CHARACTER	time (2)*20, trange*36
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDOMEG', ier )
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 1, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn
C 
		CALL GD_INIT  ( ier )
C
C*  Initialize the DG library.
C
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
C*	    Set flag to indicate processing will be done.
C
	    proces = .true.
C
C*	    Read in the variables from the TAE.
C
	    CALL GDAINP  ( gdfile, gdatim, gpack, iperr )
C
C*	    Exit if there is an error.
C
	    IF  ( iperr .ne. 0 )  THEN
	        done = .true.
	      ELSE
C
C*              Process the GDFILE input.
C
                CALL DG_NFIL ( gdfile, gdfile, ier )
                IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'DG', ier, ' ', irr )
                    proces = .false.
                END IF
C
C*              Process the GDATTIM input.
C
                CALL DG_NDTM ( gdatim, ier )
                IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'DG', ier, ' ', irr )
                    proces = .false.
		  ELSE
		    CALL DG_QTMS ( 2, .false., time, ntms, trange, ier )
		    IF ( ier .ne. 0 .or. ntms .gt. 1 ) proces = .false.
                END IF
C
C*              Get the time to process.
C
                CALL DG_NTIM ( .false., .false., time, gottm, ier )
                IF ( ier .ne. 0 ) THEN
                    CALL ER_WMSG ( 'DG', ier, ' ', irr )
                    proces = .false.
                END IF
C
C*	        Generate the OMEG grids.
C
     	        IF  ( proces )  THEN
	            CALL GDAGEN  ( time, gpack, iret )
	        END IF
C
C*	        Prompt for next diagnostic to be done.
C
	        CALL IP_DYNM  ( done, ier )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'GDOMEG', iperr, ' ', ier )
C*
	CALL IP_EXIT  ( iret )
C*
	END
