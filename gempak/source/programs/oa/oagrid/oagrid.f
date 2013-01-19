	PROGRAM OAGRID                                             
C************************************************************************
C* OAGRID								*
C*									*
C* This program creates a grid file that can be used in the objective	*
C* analysis programs.							*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/85						*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   CALL IP_IDNT & print error messages	*
C* M. desJardins/GSFC	 7/90	Eliminate extra error message		*
C* J. Whistler/SSAI	 5/91	Added error message for large grids	*
C* J. Whislter/SSAI	 6/91	Added INCLUDE statement omitted earlier	*
C* L. Williams/EAI	 7/94	Removed call to OAGUPD			*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* M. Li/SAIC		11/07	Removed check for grid limit		*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gridar*(LLMXLN),
     +			dataar*(LLMXLN), snfile*(LLMXLN),
     +	 		sffile*(LLMXLN), source*(LLMXLN),
     +			sfparm*(LLMXLN), snparm*(LLMXLN),
     +			levels*(LLMXLN), extend*(LLMXLN),
     +			cdeltn*(LLMXLN), cdeltx*(LLMXLN), 
     +			cdelty*(LLMXLN), dattim*(LLMXLN),
     +			cmaxgd*(LLMXLN), flnmsf*(LLMXLN),
     +			flnmsn*(LLMXLN)
C*
	CHARACTER	arenam*6
	INTEGER		iextnd (4)
	LOGICAL		respnd, done, proces, datflg
	REAL		gltln (4), dltln (4), eltln (4), grltln (4)
C------------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'OAGRID', ier )
	iperr = iperr + ier
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( ier )
	    done = .false.
	ELSE
	    done = .true.
	END IF
C
C*	Main loop to read in TAE parameters.
C
	DO  WHILE  ( .not. done )
C
C*	    Set the process flag
C
	    proces = .true.
C
C*	    Read in the parameters from the TAE.
C
	    CALL OAGINP  ( gdfile, cdeltn, cdeltx, cdelty, gridar,
     +			   extend, dataar, snfile, sffile, source,
     +			   sfparm, snparm, dattim, levels, cmaxgd, 
     +			   iperr )
C
C*	    Check for errors reading in TAE parameters.
C
	    IF  ( iperr .ne. 0 ) THEN
		done = .true.
	      ELSE
C
C*		Translate the areas.
C
		CALL OAGARE  ( gridar, dataar, gltln, arenam, dltln,
     +			       datflg, iret )
		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Get values for deltan, deltax, deltay and maxgrd.
C
		IF  ( proces )  THEN
		    CALL ST_RLST  ( cdeltn, '/', 0., 1, deltan, n, ier )
		    CALL ST_RLST  ( cdeltx, '/', 0., 1, deltax, n, ier )
		    CALL ST_RLST  ( cdelty, '/', 0., 1, deltay, n, ier )
		    CALL ST_ILST  ( cmaxgd, '/', 400, 1, maxgrd, n, i )
		END IF
C
C*		Compute station spacing if required.
C
		IF  ( proces .and. ( ( deltan .le. 0. ) .or. 
     +				     ( deltax .le. 0. ) .or. 
     +				     ( deltay .le. 0. ) ) )  THEN
		    CALL FL_MFIL ( sffile, ' ', flnmsf, iret )
		    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret,
     +						      ' ', ier )    
		    CALL FL_MFIL ( snfile, ' ', flnmsn, iret )
		    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret,
     +						      ' ', ier )    
		    CALL OAGCMP  ( gltln, flnmsn, flnmsf, 
     +				   source, sfparm, snparm, dattim, 
     +				   levels, deltan, deltax, deltay, 
     +				   iret )
		    IF  ( iret .ne. 0 )  THEN
	              CALL ER_WMSG ( 'OAGRID', iret, ' ', ier )
		      proces = .false.
	            END IF
		END IF
C
C*		Compute actual grid areas.
C
		IF  ( proces )  THEN
		    CALL OAGAGN  ( gltln, extend, deltax, deltay, 
     +				   datflg, grltln, eltln, iextnd, kx, 
     +				   ky, dltln, iret )
		    IF  ( iret .ne. 0 )  THEN
	                CALL ER_WMSG ( 'OAGRID', iret, ' ', ier )
			proces = .false.
	 	    END IF
		END IF
C
C*		Display the parameters to the user.
C
		IF  ( proces )  THEN
		    CALL OAGDSP  ( gltln, arenam, grltln, deltan, 
     +				   deltax, deltay, iextnd, kx, ky, 
     +				   eltln, dltln, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C*
		IF  ( proces )  THEN
		    CALL OAGGRD  ( gdfile, kx, ky, grltln, eltln, dltln,
     +				   deltan, deltax, deltay, maxgrd, 
     +				   iret )
		END IF
C
C*	    	Prompt for new values of variables.
C
		CALL IP_DYNM  ( done, ier )
	    END IF
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'OAGRID', iperr, ' ', ier )
	CALL IP_EXIT  ( ier )
	END
