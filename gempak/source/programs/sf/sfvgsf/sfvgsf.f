	PROGRAM SFVGSF
C************************************************************************
C* PROGRAM SFVGSF							*
C* 									*
C* This program takes the data in a Vector Graphics file and adds	*
C* it to a surface file.						*
C**									*
C* Log:									*
C* S. Jacobs/NCEP	 2/99	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER	vgfile*(LLMXLN), sfoutf*(LLMXLN),
     +			sfparm*(LLMXLN), dattim*(LLMXLN),
     +			colors*(LLMXLN)
C*
	LOGICAL		respnd, done, proces, stndrd 
	CHARACTER	prmdst (MMPARM)*4, parms (MMPARM)*4, filnam*72,
     +			plist*(LLMXLN), clist*(LLMXLN)
	INTEGER		iploc (MMPARM), icolr (MMPARM)
C------------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SFVGSF', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SFVGSF', ier )
C
C*	Main loop.
C
	isffln = 0
	done   = .false.
	DO WHILE  ( .not. done )
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFVINP  ( vgfile, sfoutf, dattim, sfparm, colors,
     +			   iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFVGSF', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Open the surface file.
C
	    CALL FL_MFIL ( sfoutf, ' ', filnam, iret )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	    CALL SF_OPNF  ( filnam, .true., isffln, isr, npmdst,
     +			    prmdst, iret )
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SF', iret, ' ', ier )
		proces = .false.	
	    END IF
C
C*	    Get the parameters and colors from the parameter table.
C
	    IF  ( proces )  THEN
		CALL TB_PARM ( sfparm, plist, clist, iret )
		IF  ( iret .lt. 0 )  THEN
		    CALL ER_WMSG ( 'TB', iret, ' ', ier )
		    proces = .false.
		  ELSE IF ( iret .eq. 2 ) THEN
		    plist = sfparm
		    clist = colors
		  ELSE
		    IF ( colors .ne. ' ' ) clist = colors
		END IF
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
C*	    Cannot process a non-standard file.
C
	    IF  ( .not. stndrd )  THEN
		CALL ER_WMSG  ( 'SFVGSF', -4, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Open the edit file and get parameter names.
C
	    IF  ( proces )  THEN
		CALL SFVPRM  ( plist, clist, prmdst, npmdst,
     +			       parms, iploc, icolr, nparm, iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'SFVGSF', iret, ' ', ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Give user a chance to exit.
C
	    IF  ( proces )  THEN
		CALL SFVOPT  ( vgfile, filnam, dattim, parms, icolr,
     +			       nparm, iret )
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		    CALL SF_CLOS  ( isffln, ier )
		END IF
	    END IF
C
C*	    Get the data and write to the file.
C
	    IF  ( proces )  THEN
		CALL SFVDTA  ( vgfile, isffln, dattim, nparm, iploc,
     +			       icolr, iret )
		CALL SF_CLOS ( isffln, ier )
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
