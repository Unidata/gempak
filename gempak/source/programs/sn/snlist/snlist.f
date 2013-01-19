	PROGRAM SNLIST
C************************************************************************
C* SNLIST								*
C*									*
C* This program lists data from a sounding dataset.			*
C*									*
C* Log:									*
C* I. Graffman/RDS	 8/87						*
C* M. desJardins/GSFC	10/88	Rewritten				*
C* M. desJardins/GSFC	 4/89	Modify to list unmerged data		*
C* S. Schotz/GSC	 8/90	Corrected bogus error message for 	*
C*				unmerged listing			*
C* J. Whistler/SSAI	 5/91	Changed output*20 to output*48		*
C* S. Jacobs/NMC	 6/94	STNDEX*48 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to SNLUPD			*
C* S. Jacobs/NMC	 3/95	Changed call to SNLLEV to pass file num	*
C* D. Keiser/GSC	 8/96	Added FL_MFIL to search for file type	*
C* K. Tyle/GSC		 8/96	Added ER_WMSG call after FL_MFIL call	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* D. Kidwell/NCEP       2/01   Added text output option                *
C* T. Piper/SAIC	 4/02	Initialized arecur, datcur, snfcur	*
C* T. Piper/SAIC	 4/02	Fixed error return variable 4 FL_MFIL	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER	snfile*(LLMXLN), area*(LLMXLN),
     +			dattim*(LLMXLN), snparm*(LLMXLN),
     +			stndex*(LLMXLN), levels*(LLMXLN),
     +			vcoord*(LLMXLN), output*(LLMXLN),
     +			mrgdat*(LLMXLN)
C
	CHARACTER	snfcur*72, pmdset(MMPARM)*4, arecur*48, stn*8,
     +			datcur*48, times(LLMXTM)*20, voutc*4,
     +			prmlst(MMPARM)*4, stnprm(MMPARM)*4, 
     +			outdev(4)*1, filnam*72
	REAL		vlevel(LLMXLV)
	INTEGER		lun(4)
	LOGICAL		newfil, proces, respnd, done, mrgflg, prtflg,
     +			tflg
C------------------------------------------------------------------------
C*	Initialize user interface.
C
	isnfln = 0
	arecur = ' '
	datcur = ' '
	snfcur = ' '
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SNLIST', ier )
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Loop through program listing data.
C
	DO WHILE  ( .not. done )
C
C*	    Read in variables from user interface.
C
	    CALL SNLINP  ( snfile, area, dattim, snparm, stndex, levels,
     +			   vcoord, output, mrgdat, iperr )
	    IF  ( iperr .lt. 0 )  THEN
	        done   = .true.
		proces = .false.
	      ELSE
	        proces = .true.
C
C*	        Open the input file.
C
		CALL FL_MFIL ( snfile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	        CALL SNLFIL  ( filnam, snfcur, isnfln, newfil, iflsrc,
     +			       pmdset, npmdst, ivert, mrgflg, iret )
		IF  ( iret .ne. 0 )  proces = .false.
C
C*		Decode merge type.
C
		IF  ( proces )  THEN
		    CALL IN_MRGD  ( mrgdat, prtflg, ipttyp, ier )
		    prtflg = .not. prtflg
		    IF  ( prtflg .and. mrgflg )  THEN
			CALL ER_WMSG  ( 'SNLIST', -11, ' ', ier )
			proces = .false.
		    END IF
		END IF
C
C*	        Set the area.
C
	        IF  ( proces )  THEN
		    CALL LC_UARE  ( area, newfil, isnfln, arecur, stn,
     +				    iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*	        Get the levels and vertical coordinate.
C
	        IF  ( proces .and. ( .not. prtflg ) )  THEN
		    CALL SNLLEV  ( isnfln, levels, vcoord, ivert, nlevl,
     +				   vlevel, levtyp, voutc,  lvert, 
     +				   nparts, iret )
                    IF  ( iret .ne. 0 ) proces = .false.
		END IF
C
C*	        Get input times and pointers.
C
	        IF  ( proces )  THEN
		    CALL SNLDAT  ( isnfln, dattim, newfil,
     +				   datcur, ntime, times, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
		END IF
C
C*		Check if text has been requested.
C
		tflg = .false.
		IF  ( proces ) THEN
		    CALL ST_LCUC  ( snparm, snparm, ier )
		    CALL ST_RMST  ( snparm, 'TEXT', ipos, snparm, ier )
		    IF  ( ( ipos .ne. 0 ) .and. ( iflsrc .ge. 100 ) )
     +			tflg = .true.
		    CALL ST_LCUC  ( stndex, stndex, ier )
		    CALL ST_RMST  ( stndex, 'TEXT', ipos, stndex, ier )
		    IF  ( ( ipos .ne. 0 ) .and. ( iflsrc .ge. 100 ) )
     +   		tflg = .true.
		    IF  ( tflg .and. prtflg ) THEN
			IF ( ipttyp .eq. 1 ) THEN
			    nparts = 1
			  ELSE IF ( ipttyp .eq. 2 ) THEN
			    nparts = 3
			  ELSE
			    nparts = 4
			END IF
		    END IF
		END IF
C
C*	        Get parameter information.
C
	        IF  ( proces .and. ( .not. prtflg ) )  THEN
		    CALL SNLPRM  ( snparm, stndex, voutc, pmdset, 
     +				   npmdst, nparms, prmlst, nstnp, 
     +				   stnprm, iret )
                    IF  ( (nparms .le. 0) .and. (nstnp .eq. 0) ) THEN
			 proces = .false.
			 CALL ER_WMSG  ( 'SNLIST', -3, ' ', ier )    
                    END IF
                    IF  ( (nlevl .eq. 0) .and. (nstnp .eq. 0) )  THEN
			proces = .false.
			CALL ER_WMSG  ( 'SNLIST', -12, ' ', ier )
                    END IF
		END IF
C
C*		Set the output devices.
C
		IF  ( proces .or. tflg )  THEN
		    CALL IN_OUTT  ( output, 'SNLIST', lun, nlun, 
     +				    outdev, ier )
		END IF
C
C*		Get and list the data.
C
		IF  ( proces )  THEN
		    IF  ( .not. prtflg )  THEN
			CALL SNLDTA  ( isnfln, times, ntime, vlevel, 
     +				       nlevl, lvert, levtyp, prmlst, 
     +				       nparms, stnprm, nstnp, lun, 
     +				       nlun, iret )
		      ELSE
			CALL SNLPDT  ( isnfln, times, ntime, ipttyp,
     +				       lun, nlun, iret )
		    END IF
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'SNLIST', -9, ' ', ier )
			tflg = .false.
		    END IF
		END IF
C
C*		Get and list the text.
C
		IF  ( tflg ) THEN
		    CALL SNLPRT ( isnfln, nlun, lun, times, ntime, 
     +				  nparts, ier )
		END IF
C
C*	        Dynamic tutor.
C
	        CALL IP_DYNM  ( done, iret )
	    END IF
	END DO
C
C*	Final error message and close file.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'SNLIST', iperr, ' ', ier )
	CALL SN_CLOS  ( isnfln, iret )
	CALL IP_EXIT  ( iret )
C*
	END
