	PROGRAM SFEDIT
C************************************************************************
C* PROGRAM SFEDIT							*
C* 									*
C* This program takes the data in a surface edit data file and adds	*
C* it to a surface file.						*
C**									*
C* Log:									*
C* I. Graffman/RDS	10/85						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C* G. Huffman/GSC	 1/89	Eliminate extra SFF-2 error msg		*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* P. Bruehl/Unidata	 1/94	Added ability to import ship data	*
C* L. Williams/EAI	 7/94	Removed call to SFEUPD			*
C* S. Jacobs/NMC	10/94	Rewrote ship data access; Added SF_FTYP	*
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* A. Hardy/GSC		 7/01   Increased output character length       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER	sfefil*(LLMXLN), sffile*(LLMXLN)
C*
	LOGICAL		respnd, done, proces, stndrd 
	CHARACTER	prmdst (MMPARM)*4, parms (MMPARM)*4, 
     +                  filnam*(LLMXLN)
	INTEGER		iploc (MMPARM)
C------------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SFEDIT', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SFEDIT', ier )
C
C*	Main loop.
C
	isffln = 0
	lunedt = 0
	done   = .false.
	DO WHILE  ( .not. done )
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFEINP  ( sfefil, sffile, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFEDIT', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Open the surface file.
C
	    CALL FL_MFIL ( sffile, ' ', filnam, iret )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	    CALL SF_OPNF  ( filnam, .true., isffln, isr, npmdst,
     +			    prmdst, iret )
	    IF  ( iret .ne. 0 )  THEN
		proces = .false.	
	    END IF
C
C*	    Check for a standard or non-standard file.
C
	    IF  ( proces )  THEN
		CALL SF_FTYP ( isffln, stndrd, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SF', iret, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Open the edit file and get parameter names.
C
	    IF  ( proces )  THEN
		CALL SFEPRM  ( sfefil, prmdst, npmdst, stndrd, lunedt,
     +			       parms, nparm, iploc, iret )
		IF  ( iret .ne. 0 )  THEN
		    IF  ( iret .eq. -8 )  iret = -9
		    CALL ER_WMSG  ( 'SFEDIT', iret, sfefil, ier )
		    CALL SF_CLOS  ( isffln, ier )
		    isffln = 0
		    proces = .false.
		END IF
	    END IF
C
C*	    Give user a chance to exit.
C
	    IF  ( proces )  THEN
		CALL SFEOPT  ( sfefil, filnam, parms, nparm, 
     +			       iret )
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		    CALL SF_CLOS  ( isffln, ier )
		    CALL FL_CLOS  ( lunedt, ier )
		END IF
	    END IF
C
C*	    Get the data and write to the file.
C
	    IF  ( proces )  THEN
		IF  ( stndrd )  THEN
		    CALL SFEDTA  ( lunedt, isffln, nparm, iploc, iret )
		  ELSE
		    CALL SFESHP  ( lunedt, isffln, nparm, iploc, iret )
		END IF
		CALL SF_CLOS  ( isffln, ier )
		CALL FL_CLOS  ( lunedt, ier )
	    END IF
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
	END
