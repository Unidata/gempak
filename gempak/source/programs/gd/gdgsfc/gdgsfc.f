	PROGRAM GDGSFC
C************************************************************************
C* PROGRAM GDGSFC							*
C*									*
C* This program interpolates grid point data to stations and writes	*
C* the new station data to a GEMPAK surface file.			*
C*									*
C**									*
C* Log:									*
C* J. Hoopingarner/CPC	 3/97						*
C* S. Jacobs/NCEP	10/99	Renamed to GDGSFC; Overhaul		*
C* T. Lee/GSC		11/99	Allowed user opts to exit; Scaling	*
C* S. Jacobs/NCEP	 3/01	Replaced DG_OFIL with DG_MFIL 		*
C* T. Lee/SAIC		 4/03	Handled file with no write access	*
C* K. Brill/HPC		 4/03	CALL DG_INTL; initilize contin before	*
C*				loop over stations			*
C* T. Lee/SAIC		 5/03	Fixed error message			*
C* R. Tian/SAIC         11/03   Added nuflg to DG_INTL call             *
C* R. Tian/SAIC          2/04   Removed nuflg from DG_INTL call         *
C* R. Tian/SAIC          1/04   Applied time/file mgmt			*
C* M. Li/SAIC		11/07	Added gdzdat to remove LLMXGD    	*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	gdfile*(LLMXLN), gdatim*(LLMXLN),
     +			gvcord*(LLMXLN), gfunc*(LLMXLN),
     +			glevel*(LLMXLN), scale*(LLMXLN),
     +			sffile*(LLMXLN), sfparm*(LLMXLN)
C*
	CHARACTER	time (2)*20, parms(MMPARM)*4, 
     +			sfp*(LLMXLN), timfnd*36
	LOGICAL		respnd, done, proces, gottm
C*	
	INCLUDE		'ERMISS.FNC'
C-----------------------------------------------------------------------
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
	    CALL IP_IDNT  ( 'GDGSFC', ier )
C
C*  Initialize GEMPLT.
C
	    CALL GG_INIT  ( 0, ier )
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
C*  Main loop to read in TAE parameters and write SFC file.
C
	DO WHILE  ( .not. done )
C
C*	    Get the user input.
C
	    proces = .true.
	    CALL GDZINP  ( gdfile, gdatim, gvcord, gfunc, glevel, 
     +			   scale, sffile, sfparm, iperr )
C
	    IF  ( iperr .ne. 0 )  THEN
		done = .true.
	      ELSE
C
C*		Open the surface file.
C
		IF  ( proces )  THEN
		    CALL SF_OPNF  ( sffile, .true., isffln, iflsrc, 
     +				    nparm, parms, iperr )
		    IF  ( iperr .ne. 0 )  proces = .false.
		END IF
C
C*		Find the output parameter name in the surface file.
C
		IF  ( proces )  THEN
		    CALL ST_LCUC ( sfparm, sfp, ier )
		    CALL ST_FIND ( sfp, parms, nparm, ipos, ier )
		    IF  ( ipos .eq. 0 )  THEN
			CALL ER_WMSG ( 'GDGSFC', -4, sfp, ier )
			proces = .false.
		    END IF
		END IF
C
C*              Process the GDFILE input.
C
		IF ( proces ) THEN
                    CALL DG_NFIL ( gdfile, ' ', iperr )
                    IF ( iperr .ne. 0 ) THEN
                        CALL ER_WMSG ( 'DG', iperr, ' ', ier )
                        proces = .false.
                    END IF
		END IF
C
C*              Process the GDATTIM input.
C
                IF ( proces ) THEN
                    CALL DG_NDTM ( gdatim, iperr )
                    IF ( iperr .ne. 0 ) THEN
                        CALL ER_WMSG ( 'DG', iperr, ' ', ier )
                        proces = .false.
                      ELSE
                        CALL DG_QTMS ( 2, .false., time, ntms,
     +                                 timfnd, iperr )
                        IF ( iperr .ne. 0 .or. ntms .gt. 1 )
     +                      proces = .false.
                    END IF
                END IF
C
C*              Get the time to process.
C
                IF ( proces ) THEN
                    CALL DG_NTIM ( .false., .false., time,
     +		                   gottm, iperr )
                    IF ( iperr .ne. 0 .or. .not. gottm ) THEN
                        CALL ER_WMSG ( 'DG', iperr, ' ', ier )
                        proces = .false.
                      ELSE
                        CALL TG_DUAL ( time, timfnd, iperr )
                    END IF
                END IF
C
C*		Read, compute, interpolate, and write surface file
C
		IF ( proces ) THEN
		    iproc = 1;
		  ELSE
		    iproc = 0;
		END IF
		CALL GDZDAT ( iproc, ipos, timfnd, gdfile, glevel, 
     +                      gvcord, gfunc, scale, sffile, sfp,
     +                      isffln, iret )
C
C*		Close the surface and grid files.
C
		IF  ( isffln .gt. 0 )  CALL SF_CLOS  ( isffln, ier )
C
		CALL IP_DYNM ( done, iperr )
	    END IF
	END DO
C
C*	Print general error messages if necessary.
C
	IF  (iperr .ne. 0)  CALL ER_WMSG ( 'GDGSFC', iperr, ' ', ier )
C
C*	Exit from GEMPLT and the TAE.
C
	CALL GENDP   ( 0, iret )
	CALL IP_EXIT ( iret )
C*
	END
