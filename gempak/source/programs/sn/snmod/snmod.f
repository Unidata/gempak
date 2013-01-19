	PROGRAM SNMOD
C************************************************************************
C* SNMOD								*
C*									*
C* This program creates a modified sounding file from another sounding	*
C* file.								*
C**									*
C* Log:									*
C* M. Goodman/RDS	11/84	Original source				*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* S. Schotz/GSC	 1/90	Added capability to write to unmerged	*
C*                              files					*
C* S. Schotz/GSC	 1/90	Close output file for error conditions  *
C* S. Schotz/GSC	 8/90	Corrected errors creating unmerged file	*
C* S. Schotz/GSC	 8/90	Added IDNTYP for processing files with	*
C*				no station numbers			*
C* L. Williams/EAI	 7/94	Removed call to SNOUPD			*
C* S. Jacobs/NMC	 3/95	Changed call to SNOLEV to pass file num	*
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* D. Kidwell/NCEP       2/01   Increased number of part names          *
C* T. Piper/SAIC	 4/02	Fixed UMR; init arecur, datcur, snfcur, *
C*							isnfln		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), snoutf*(LLMXLN),
     +			snparm*(LLMXLN), area*(LLMXLN), 
     +			dattim*(LLMXLN), levels*(LLMXLN),
     +			vcoord*(LLMXLN), timstn*(LLMXLN),
     +                  mrgdat*(LLMXLN), idntyp*(LLMXLN)
C*
	LOGICAL		respnd, done, proces, newfil, exist, 
     +                  inpmrg, outmrg
	CHARACTER	prmdst (MMPARM)*4, snfcur*72, voutc*4, stn*8,
     +			arecur*48, datcur*48, times (LLMXTM)*20,
     +			prmout (MMPARM)*4, parms (MMPARM)*4, 
     +                  prtnam (16)*4, filnam*72
	REAL		rlevel (LLMXLV)
C*
	INTEGER		iarr (2)
	EQUIVALENCE	( iarr (1), maxtim ), ( iarr (2), maxstn )
C------------------------------------------------------------------------
        iptype = 0
        iflout = 0
	isnfln = 0
	arecur = ' '
	datcur = ' '
	snfcur = ' '
        inpmrg = .false.
        outmrg = .false.
C
C*	Initialize the TAE variable block.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SNMOD', ier  )
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C*
	DO WHILE  ( .not. done )
C
C*	    Get user input.
C
	    CALL SNOINP  ( snfile, snoutf, snparm, area, dattim, levels,
     +			   vcoord, timstn, mrgdat, idntyp, iperr )
	    IF  ( iperr .lt. 0 ) THEN
		done   = .true.
		proces = .false.
	      ELSE
		proces = .true.
	    END IF
C
C*	    Set default for IDNTYP.
C
            CALL ST_LCUC ( idntyp, idntyp, ier )
	    IF  ( idntyp .ne. 'STID' )  idntyp = 'STNM'	    	    
C
C*          Check for blank output file name.
C
	    IF  ( snoutf .eq. ' ' )  THEN
                CALL ER_WMSG ( 'SNMOD', -13, ' ', ier )
                proces = .false.
            END IF           
C
C*	    Process input file.
C
	    IF  ( proces )  THEN
		CALL FL_MFIL ( snfile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		CALL SNOINF  ( filnam, snfcur, isnfln, newfil, prmdst,
     +			       npmdst, ivert, inpmrg, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Process area and time.
C
	    IF  ( proces )  THEN
		CALL LC_UARE  ( area, newfil, isnfln, arecur, stn,
     +				iret )
		IF  ( iret .eq. 0 )  THEN
		    CALL SNOTIM  ( isnfln, dattim, newfil,
     +				   datcur, ntime, times, iret )
		END IF
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Open output file if it exists.
C
	    IF  ( proces )  THEN
		INQUIRE  ( FILE = snoutf, EXIST = exist )
	        IF  ( exist )  THEN
C
C*		    Open the output file and check whether merged or
C*		    unmerged file.
C
		    CALL SN_OPNF  ( snoutf, .true., iflout, isrc,
     +				    npmout, prmout, jvert, outmrg,
     +				    iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
		    ELSE IF  ( .not. outmrg .and. inpmrg )  THEN
			CALL ER_WMSG  ( 'SNMOD', -3, snoutf, ier )
			proces = .false.
		    END IF
                    IF ( proces .and. ( .not. outmrg ) ) THEN
C
C*                      Get type of unmerged data for existing output 
C*                      file.
C
                        CALL DM_PNAM ( iflout, nprt, prtnam, ier ) 
			CALL ST_FIND ( 'TTCC', prtnam, nprt, ipos, ier )
			IF ( ipos .ne. 0 ) THEN
			    iptype = 3
			  ELSE
			    CALL ST_FIND ( 'TTBB', prtnam, nprt, ipos, 
     +					   ier )
			    IF ( ipos .ne. 0 ) THEN
				iptype = 2
			      ELSE
			        CALL ST_FIND ( 'TTAA', prtnam, nprt, 
     +					       ipos, ier )
				IF ( ipos .ne. 0 ) THEN
				    iptype = 1
				  ELSE
				    proces = .false.
				END IF
			    END IF
			END IF
                    END IF
		ELSE
C
C*                  Make sure # of stations and times non-zero for new
C*                  output file
C
		    CALL ST_ILST  ( timstn, '/', 0, 2, iarr, n, ier )
		    IF  ( maxtim .le. 0 )  THEN
			CALL ER_WMSG  ( 'SNMOD', -4, ' ', ier )
			proces = .false.
		    ELSE IF  ( maxstn .le. 0 )  THEN
			CALL ER_WMSG  ( 'SNMOD', -5, ' ', ier )
			proces = .false.
		    END IF
C
C*                  Determine whether new output file is merged or 
C*                  unmerged from user input, check validity.
C
                    CALL IN_MRGD ( mrgdat, outmrg, iptype, ier )
		    IF  ( .not. outmrg .and. inpmrg )  THEN
			CALL ER_WMSG  ( 'SNMOD', -3, snoutf, ier )
			proces = .false.
                    END IF
		END IF
	    END IF
C
C*	    Process levels and vertical coordinate for merged output 
C*          file.
C
	    IF  ( proces .and. outmrg )  THEN
		CALL SNOLEV  ( isnfln, levels, vcoord, ivert, nlev,
     +			       rlevel, levtyp, voutc, jvert, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get list of output parameters for merged output file.
C
	    IF  ( proces .and. outmrg )  THEN
		CALL SNOPRM  ( snparm, voutc, prmdst, npmdst, exist, 
     +			       prmout, npmout, parms, nparms, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Display options.
C
	    IF  ( proces )  THEN
                IF  ( .not. outmrg ) THEN
		    voutc  = 'PRES'
		    nparms = 0
		END IF
		CALL SNOOPT  ( filnam, snoutf, dattim, area, levels,
     +			       voutc, parms, nparms, exist, maxtim,
     +			       maxstn, iptype, idntyp, iret )
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		    IF  ( exist )  CALL SN_CLOS  ( iflout, ier )
		END IF
	    END IF
C
C*	    Create new file.
C
	    IF  ( proces .and. ( .not. exist ) )  THEN
                IF ( outmrg) THEN
		    CALL SN_CREF  ( snoutf, MFRAOB, nparms, parms, 
     +				    maxstn, maxtim, .false., is, io, 
     +				    ib, .true., iflout, iret )
                ELSE
                    CALL SN_CRUA ( snoutf, MFRAOB, iptype, maxstn, 
     +                             maxtim, .false., .true., .true.,
     +                             iflout, iret )
                END IF 
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Move data from input to output file.
C
	    IF  ( proces )  THEN
		CALL SNODAT  ( isnfln, iflout, times, ntime, rlevel,
     +			       nlev, levtyp, jvert, nparms, iptype, 
     +                         idntyp, iret )
		CALL SN_CLOS ( iflout, ier )
                iflout = 0
	    END IF
C
C*          Close output file if open when errors have occurred.
C
	    IF ( iflout .ne. 0 ) CALL SN_CLOS ( iflout, ier )
C
C*	    Call the dynamic tutor.
C
	    IF  ( .not. done )  CALL IP_DYNM  ( done, iret )
	END DO
C
C*	Close the input file.
C
	IF  ( isnfln .ne. 0 )  CALL SN_CLOS  ( isnfln, ier )
	CALL IP_EXIT  ( ier )
C*
	END
