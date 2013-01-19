	PROGRAM SFLIST
C************************************************************************
C* PROGRAM SFLIST							*
C*									*
C* This program lists the data in a surface file.			*
C*									*
C* Log:									*
C* I. Graffman/RDS	 7/87	GEMPAK4					*
C* M. desJardins/GSFC	10/87	Changed formats for output		*
C* M. desJardins/GSFC	 6/88	Added keynam				*
C* M. desJardins/GSFC	 4/90	Changed LC_UARE to SF_UARE		*
C* J. Whistler/SSAI	 5/91	Changed output*24 to output*48		*
C* L. Williams/EAI	 3/94	Clean up declarations of user input	*
C*				variables				*
C* L. Williams/EAI	 7/94	Removed call to SFLUPD			*
C* D. Keiser/GSC	 4/96	Add choice between TEXT and DATA	*
C* S. Jacobs/NCEP	 6/96	Added SPCL parm to list special SAOs	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call,	*
C*				use filnam in call to SFLFIL		*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* T. Lee/SAIC		 8/01	Fixed error message			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	sffile*(LLMXLN), output*(LLMXLN),
     +			keynam*(LLMXLN), parms*(LLMXLN), dattim*(LLMXLN),
     +			area*(LLMXLN)
C*
	CHARACTER	arecur*48, sffcur*72, datcur*48, prmcur*72
	CHARACTER	prmdst (MMPARM)*4, cparm (MMPARM)*4, filnam*72
	CHARACTER	times (LLMXTM)*20
	CHARACTER	outdev (4)*1, stn*8
	LOGICAL 	respnd, done, proces, newfil, chrflg (MMPARM)
	LOGICAL		tflg, sflg
	INTEGER		luns (4)
C*
	DATA		sffcur, prmcur, datcur, arecur  / 4 * ' ' /
C------------------------------------------------------------------------
C*	Initilaize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SFLIST', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SFLIST', ier )
C
C*	Main loop.
C
	done  = .false.
	iflno = 0
	DO WHILE (.not. done)
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFLINP  ( sffile, parms, output, area, dattim, keynam,
     +			   iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFLIST', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Make sure keynam is capitalized.
C
	    CALL ST_LCUC  ( keynam, keynam, ier )
	    IF  ( keynam .ne. 'STNM' )  keynam = 'STID'
C
C*	    Open the data file.
C
	    CALL FL_MFIL ( sffile, ' ', filnam, iret )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	    CALL SFLFIL  ( filnam, sffcur, iflno, newfil, iflsrc,
     +			   prmdst, npmdst, ier )
	    IF  ( ier .ne. 0 )  proces = .false.
C
C*	    Set up the output parameters.
C
	    IF  ( proces )  THEN
		tflg = .false.
		sflg = .false.
		CALL ST_LCUC ( parms, parms, ier )
		CALL ST_RMST ( parms, 'TEXT', ipos, parms, ier )
		IF ( ( ipos .ne. 0 ) .and. ( iflsrc .ge. 100 ) )
     +						tflg = .true.
		CALL ST_RMST ( parms, 'SPCL', ipos, parms, ier )
		IF  ( ( ipos .ne. 0 ) .and.
     +		      ( (iflsrc .eq. 101) .or. (iflsrc .eq. 102) ) )
     +						sflg = .true.
		CALL SFLPRM  ( newfil, parms, prmdst, npmdst, tflg,
     +			       sflg, prmcur, ncprm, chrflg, cparm, 
     +			       iret )
		IF  ( ( iret .ne. 0 ) .and. ( .not. tflg ) .and. 
     +		      ( .not. sflg ) )  proces = .false.
	    END IF
C
C*	    Process area name.
C
	    IF  ( proces )  THEN
		CALL SF_UARE  ( iflno, area, newfil, arecur, stn, ier )
	        IF  ( ier .ne. 0 )   THEN
		    proces = .false.
		    CALL ER_WMSG ( 'SF', ier, area, iret )
		END IF
	    END IF
C
C*	    Get the times to use.
C
	    IF  ( proces )  THEN
		CALL SFLDAT  ( iflno, dattim, newfil, datcur,
     +			       ntime, times,  ier )
		IF  ( ier .ne. 0 )  proces = .false.
	    END IF
C
C*	    Set up output.
C
	    IF  ( proces )  THEN
		CALL IN_OUTT  ( output, 'SFLIST', luns, nlun, outdev, 
     +				ier )
	    END IF
C
C*	    List all the data.
C
	    IF  ( proces .and. ncprm .ne. 0 )  THEN
		CALL SFLPRC  ( iflno, chrflg, ncprm, cparm, nlun, luns, 
     +			       times, ntime, keynam, ier )
	    END IF
C
C*	    List the text data.
C
	    IF  ( proces .and. ( tflg .or. sflg ) )  THEN
		CALL SFLPRT ( iflno, nlun, luns, times, ntime,
     +			      tflg, sflg, ier )
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
