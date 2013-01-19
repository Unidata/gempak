	PROGRAM GDSTAT
C************************************************************************
C* GDSTAT								*
C*									*
C* This program calculates an average, standard deviation and number	*
C* of points for grids of different times.				*
C*									*
C* Log:									*
C* I. Graffman/RDS	 4/87						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 1/89	Added call to IP_EXIT			*
C* M. desJardins/GSFC	11/89	Changed GR_FILE to DG_OFIL		*
C* M. desJardins/GSFC	 8/90	GEMPAK 5.0				*
C* M. desJardins/GSFC	10/90	Fix initialization of internal arrays	*
C* S. Jacobs/EAI         2/94   Added COLADD flag to DG_OFIL            *
C* L. Williams/EAI	 7/94	Removed call to GDYUPD			*
C* P. Bruehl/COMET	 8/95	Check for missing data in averaging (DK)*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Lee/GSC		 1/99	Increased LLMXTM to LLMXGT		*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
C* T. Lee/GSC		 7/01	Processed multiple files		*
C* T. Lee/SAIC		10/01	Reopened output file after DG_CLAL call	*
C* S. Jacobs/NCEP	 1/02	Fixed DG_MFIL to not add a col to grid	*
C* K. Brill/HPC		11/02	Added MAX and MIN grids			*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* K. Brill/HPC		 5/03	Check for change in grid size (kx,ky)	*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
C* A. Hardy/NCEP	 4/04	Removed check on ntime > MXLOOP		*
C* R. Tian/SAIC          3/05   Changes for time/file mngmnt            *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* B. Hebbard/SAIC      03/08   Remove LLMXGD size restriction on grids *
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gfunc*(LLMXLN), 
     +			gdatim*(LLMXLN), glevel*(LLMXLN), 
     +			gvcord*(LLMXLN), gdoutf*(LLMXLN), 
     +			grdnam*(LLMXLN)
        CHARACTER       dattim (2)*20
C*
	LOGICAL		respnd, done, proces, gottm
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDSTAT', ier )
C
C*  Initialize GEMPLT.
C
	    mode = 1
	    CALL GG_INIT  ( mode, ier )
	    IF  ( ier .eq. 0 )  THEN
C
C*  Initialize grid library common area grdcmn.cmn.
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
C*	Main loop for user input and computations.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
	  CALL GDYINP  ( gdfile, gdoutf, gfunc, gdatim, glevel, 
     +			 gvcord, grdnam, iperr )
C
C*	  Exit if there is an error.
C
	  IF  ( iperr .ne. 0 )  THEN
	    done   = .true.
	  ELSE
C
C*          Process the GDFILE and GDOUTF input.
C
            CALL DG_NFIL ( gdfile, gdoutf, ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, ' ', irr )
                proces = .false.
            END IF
C
C*          Process the GDATTIM input.
C
            CALL DG_NDTM ( gdatim, ier )
            IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, gdatim, irr )
                proces = .false.
            END IF
C
C           Get the grid dimensions:
C
C           (a)  Before we can call DG_KXKY to get grid dimensions,
C                we make a call to DG_NTIM to advance to the next
C                (in this case first) time in the grid file...
C 
            IF (proces) THEN
              CALL DG_NTIM ( .false., .false., dattim, gottm, ier )
              IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, ' ', irr )
                proces = .false.
              END IF
            END IF
C
C           (b)  Now get the grid dimensions.  Note that this program
C                (GDSTAT) will error if all grids (across all times)
C                do not have the same dimensions, so it does not matter
C                that we query just the first grid in the file...
C
            IF (proces) THEN
              CALL DG_KXKY ( kx, ky, ier )
              IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, ' ', irr )
                proces = .false.
              END IF
            END IF
C
C           (c) However, we need to reset the grid time back to the
C               first grid in the file, so that it will be selected
C               when the real work begins later...
C
            IF (proces) THEN
              CALL DG_RSTM ( ier )
              IF ( ier .ne. 0 ) THEN
                CALL ER_WMSG ( 'DG', ier, ' ', irr )
                proces = .false.
              END IF
            END IF
C
C*          Call the wrapper routine to allocate storage and do the work
C
            IF (proces) THEN
              CALL GDSTAA ( kx, ky, glevel, gvcord, gfunc, grdnam, ier )
            END IF
C
C
C*	    Prompt for next action to be done.
C
	    CALL IP_DYNM  ( done, ier )
	  END IF
	END DO
C
C*	Print general error messages if necessary.
C
        IF ( iperr .ne. 0 ) CALL ER_WMSG ( 'GDSTAT', iperr, ' ', ier )
C
C*      Exit from the TAE.
C
	CALL IP_EXIT  ( iret )
	END
