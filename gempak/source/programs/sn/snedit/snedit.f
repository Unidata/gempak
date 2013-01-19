	PROGRAM SNEDIT
C************************************************************************
C* SNEDIT								*
C*									*
C* This program reads a sounding edit file and adds the data to a	*
C* sounding dataset.							*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88	New version for GEMPAK 4.1		*
C* S. Schotz/GSC	12/89	Now can add unmerged data		*
C* K. Brill/NMC		 8/93	Change for 8-char ID			*
C* L. Williams/EAI	 7/94	Removed call the SNEUPD			*
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* D. Kidwell/NCEP       2/01   Added more parts for unmerged data      *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( MXPART = 12, MXPARM = 6 )	
C*
	CHARACTER	snefil*(LLMXLN), snfile*(LLMXLN), timstn*(LLMXLN)
C*
	CHARACTER	parms (MMPARM)*4, ptnam ( MXPART)*4
	CHARACTER	stid*8, time*20, filnam*72
	INTEGER		nplevs(MXPART), iptime(MXPART)
	REAL		data (LLMXDT), ptdata ( LLMXLV * MXPARM, MXPART)
	LOGICAL		respnd, done, proces, newfil, merge, more
        LOGICAL		zwind(MXPART)
C------------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	CALL IP_IDNT  ( 'SNEDIT', ier )
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
C
C*	Loop through processing data.
C
	DO WHILE  ( .not. done )
C
C*	    Get user input.
C
	    CALL SNEINP  ( snefil, snfile, timstn, iperr )
	    IF  ( iperr .ne. 0 )  THEN
	        done   = .true.
		proces = .false.
	      ELSE
		proces = .true.
	    END IF
C
C*	    Open the edit file and read in parameters, determine
C*          whether merged or unmerged
C
	    IF  ( proces )  THEN
		
		CALL SNEPRM  ( snefil, lunedt, parms, nparms, iret )
                IF  ( iret .eq. 0 ) THEN
                    merge = .true.
                ELSE IF ( iret .eq. -4) THEN
C
C*                  Open edit file check validity as unmerged file
C
                    CALL SNEOUE ( snefil, lunedt, iret )
		    IF ( iret .eq. 0) THEN
                        merge = .false.
		    ELSE
 		        CALL ER_WMSG  ( 'SNEDIT', iret, snefil, ier )
		        proces = .false.
                    END IF
		ELSE  
		    CALL ER_WMSG  ( 'SNEDIT', iret, snefil, ier )
		    proces = .false.
		END IF
	    END IF
C
C*	    Open / create the sounding dataset.
C
	    IF  ( proces )  THEN
		CALL FL_MFIL ( snfile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		IF ( merge ) THEN
		    CALL SNEOPN ( filnam, timstn, parms, nparms, 
     +		    	           isnfln, newfil, iret )
                ELSE
                    CALL SNEOPU ( filnam, timstn, isnfln, newfil, iret )
                END IF
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		    CALL FL_CLOS  ( lunedt, ier )
		    CALL ER_WMSG  ( 'SNEDIT', iret, snfile, ier )
		ELSE
		    CALL SNEDSP   ( filnam, merge, newfil, parms, 
     +				    nparms, iret )
		    IF  ( iret .ne. 0 )  THEN
			proces = .false.
			CALL SN_CLOS  ( isnfln, ier )
			CALL FL_CLOS  ( lunedt, ier )
		    END IF
		END IF
	    END IF
C
C*	    Read the edit file, adding stations to sounding file.
C
	    IF  ( proces )  THEN
		more = .true.
		DO WHILE  ( more )
C
C*		    Get station information.
C
		    CALL SNESTN  ( lunedt, time, istime, stid, istnm, 
     +				   slat, slon, selv, iret )
		    IF  ( iret .ne. 0 )  more = .false.
C
C*		    Get station data.
C
		    IF  ( more )  THEN
			IF ( merge ) THEN
			    CALL SNEDTA ( lunedt, nparms, data, nlev, 
     +                                    iret )
			    IF  ( ( nlev .eq. 0 ) .or. ( iret .ne. 0 ) )
     +			    THEN
                                more = .false.
  		                CALL ER_WMSG  ( 'SNEDIT', iret, 
     +                                           stid, ier )
                            END IF
				
                        ELSE
                            CALL SNEDTU ( lunedt, ptdata, ptnam, iptime,
     + 			                  zwind, nplevs, nparts, iret )
                            IF ( iret .ne. 0 ) THEN
                                more = .false. 
  		                CALL ER_WMSG  ( 'SNEDIT', iret, 
     +                                           stid, ier )
                            END IF
                        END IF
		    END IF
C
C*		    Write station data to file.
C
		    IF  ( more )  THEN
                        IF ( merge ) THEN
			    CALL SNEWRT ( isnfln, time, istime, stid, 
     +				          istnm, slat, slon, selv, 
     +                                    data, nlev, iret )
			    IF ( iret .ne. 0 )  more = .false.
                        ELSE
                            CALL SNEWRU ( isnfln, time, iptime, stid, 
     +                                    istnm, slat, slon, selv,
     +                                    ptdata, ptnam, nplevs, 
     +                                    nparts, zwind, iret )
                            IF ( iret .ne. 0 ) more = .false.
                        END IF
		    END IF
		END DO
	    END IF
C
C*	    Update global parameters.
C
	    IF  ( proces )  THEN
		CALL FL_CLOS  ( lunedt, ier )
		CALL SN_CLOS  ( isnfln, ier )
	    END IF
C
C*	    Call dynamic tutor.
C
	    IF  ( .not. done )  THEN
		CALL IP_DYNM  ( done, iret )
	    END IF
	END DO
C*
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'SNEDIT', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
C*
	END
