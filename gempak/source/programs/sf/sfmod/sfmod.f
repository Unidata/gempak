	PROGRAM SFMOD
C************************************************************************
C* PROGRAM SFMOD							*
C*									*
C* This program moves data from one surface data set to another.	*
C*									*
C* Log:									*
C* I. Graffman/RDS	10/85						*
C* M. desJardins/GSFC	10/86	Added GEMPAK parameter names		*
C* M. desJardins/GSFC	 6/88	Changed a lot				*
C* M. desJardins/GSFC	11/89	Changed station time			*
C* K. Brill/NMC          9/90   Declare STID as CHAR*8			*
C* M. desJardins/GSFC	 1/91	Set stn using ISTNM when STID is blank	*
C* J. Whistler/SSAI	 4/91	Changed ier to loop through stations	*
C*				correctly				*
C* K. Brill/NMC		 8/93	Added ISPRI to SF_ASTN call		*
C* L. Williams/EAI	 7/94	Removed call to SFCUPD			*
C* K. Tyle/GSC	 	 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* A. Hardy/GSC		 1/99	Added DATOUT option                     *
C* A. Hardy/GSC		 3/99	Added priority parameter to PC_SSTN     *
C* D. Kidwell/NCEP	 3/99	Added SFPARM option; moved SFCOPT call  *
C* A. Hardy/GSC          3/99   Added priority parameter SF_QSTN,SF_SNXT*
C*	                        SF_SSTN 				*
C* A. Hardy/GSC          3/99   Removed ispri = 0			*
C* T. Piper/SAIC	 4/02	Fixed UMR; initialized sffinp & sffout	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( NEXP = 100 )
	CHARACTER	sffile*(LLMXLN), sfoutf*(LLMXLN),
     +			dattim*(LLMXLN), area*(LLMXLN),
     +			datout*(LLMXLN), sfparm*(LLMXLN)
C*
	CHARACTER	sffout*72, sffinp*72, stn*8, stat*4, coun*4,
     +                  stid*8, filnam*72
	LOGICAL		respnd, done, proces, dotime, sfadd
	CHARACTER	timinp (LLMXTM)*20, timout (LLMXTM)*20 
	CHARACTER	outtm (LLMXTM)*20
	CHARACTER	chdat (MMPARM)*8, tt*20
	CHARACTER	times (LLMXTM)*20
        CHARACTER       list (NEXP)*20
	REAL		data (MMPARM), cmpdat (MMPARM), 
     +			outdata (MMPARM)
	INTEGER		itarr(5), ipos (MMPARM)
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	sffinp = ' '
	sffout = ' '
C
C*	Initialize user interface.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .ne. 0 )  THEN
	    CALL ER_WMSG  ( 'SFMOD', iperr, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'SFMOD', ier )
C
C*	Main loop.
C
	done   = .false.
	iflinp = 0
	iflout = 0
	DO WHILE  ( .not. done )
C
C*	    Get user input and exit if there is an error.
C
	    CALL SFCINP  ( sffile, sfoutf, dattim, area, datout,
     +			   sfparm, iperr )
	    IF  ( iperr .ne. 0 )  THEN
		CALL ER_WMSG  ( 'SFMOD', iperr, ' ', ier )
		CALL SS_EXIT
	    END IF
	    proces = .true.
C
C*	    Process input file name.
C
	    CALL FL_MFIL ( sffile, ' ', filnam, iret )
	    IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
	    CALL SFCOFL  ( filnam, sffinp, iflinp, ntdset, timinp,
     +			   iret )
	    IF  ( iret .ne. 0 )  proces = .false.
C
C*	    Process output file name.
C
	    IF  ( proces )  THEN
		CALL SFCNFL  ( sfoutf, sfparm, sffout, iflout, ntout,
     +			       outtm, ncols, ipos, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Decode user input time.
C
	    IF  ( proces )  THEN
C
C*	        Process input date and times.
C
		CALL TI_FIND  ( dattim, ntdset, timinp, tt,
     +				ntmprc, times, iret )
		IF  ( iret .ne. 0 )  THEN
		    proces = .false.
		END IF
C
C*	        Process output date and times.
C
                IF ( proces .and. ( datout .ne. ' ' ) ) THEN
C
C*	            Check if times exist in output file.
C
		    IF ( ntout .ne. 0 ) THEN
			CALL TI_FIND  ( datout, ntout, outtm, tt,
     +                                  ntmout, timout, iret )
		      ELSE
	                CALL ST_CLST ( datout, ';',' ', NEXP, list, 
     +                                 nlist, ier )
		        DO i = 1, nlist
 	   		    CALL TI_STAN  ( list (i), times (ntmprc), 
     +		    		            timout (i), iret )
		        END DO
                        ntmout = nlist
		    END IF 
		    IF  ( iret .ne. 0 )  THEN
		        proces = .false.
		    END IF
C
C*	            Check if the number of input times is less than
C*		    output times. If it is less, then quit.
C
                    IF ( ntmprc .lt. ntmout ) THEN
                        CALL ER_WMSG  ( 'SFMOD', -5, ' ', ier )
                        proces = .false.
C
C*	              Check if the number of input times is greater than
C*		      output times. If so, fill datout with last time.
C
		      ELSE IF ( ntmprc .gt. ntmout) THEN
		        DO i = ntmout + 1, ntmprc 
			    timout (i) = timout (ntmout)
		        END DO 
                    END IF
C
C*	          Accept input times as output times.
C
                  ELSE
                    datout = dattim
		    ntmout = ntmprc
		    DO i = 1, ntmprc
		        timout(i) = times(i)
		    END DO
                END IF
	    END IF
C
C*	    Set up area.
C
	    IF  ( proces )  THEN
		CALL LC_SARE  ( area, iflinp, stn, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Write message displaying the user options.
C
	    IF  ( proces )  THEN
	        CALL SFCOPT   ( filnam, sfoutf, area, times,
     +			        timout, ntmprc, sfparm, ier )
	        IF  ( ier .ne. 0 )  THEN
		    proces = .false.
	        END IF
	    END IF
C
C*	    Loop through each of the times requested.
C
	    itime = 1
	    DO WHILE  ( proces .and. ( itime .le. ntmprc ) )
		dotime = .true.
C
C*		Set the time in the input file.
C
		CALL SF_STIM  ( iflinp, times (itime), iret )
C
C*		Check for time in output file.  If not found, 
C*		add the time.
C
		CALL SF_STIM  ( iflout, timout (itime), iret )
		IF  ( iret .ne. 0 )  THEN
		    CALL SF_ATIM  ( iflout, timout (itime), iret )
		    CALL SF_STIM  ( iflout, timout (itime), iret )
		    IF  ( iret .ne. 0 )  THEN
			CALL ER_WMSG  ( 'SFMOD', -4, timout (itime), k )
			dotime = .false.
		    END IF
		END IF
C
C*		Process this time.
C
		IF  ( dotime )  THEN
		    ier = 0
C
C*		    Reset stations to beginning of file.
C
		    CALL SF_BEGS  ( iflinp, iret )
C
C*		    Loop through stations.  Read next station.
C
		    sfadd = .false.
		    DO WHILE  ( ier .eq. 0 )
			CALL SF_SNXT  ( iflinp, stid, istnm, rlt, rln, 
     +					rel, ispri, ier )
			CALL ST_LSTR  ( stid, islen, ier2 )
			IF  ( islen .gt. 0 )  THEN
			    stn = stid
			  ELSE
			    CALL ST_INCH  ( istnm, stn, ier2 )
			END IF
C
C*			Read in the data.
C
			IF  ( ier .eq. 0 )  THEN
			    CALL SF_RDAT  ( iflinp, data, ihhmm, iret )
C
C*			    If there is data, set station in the output
C*			    file.
C
			    IF  ( iret .eq. 0 )  THEN
				CALL SF_SSTN  ( iflout, stn, stid, in,
     +						rl1, rl2, re, ispri,
     +						iret )
C
C*				If the station is not in output file,
C*				add it.
C
				IF  ( iret .ne. 0 )  THEN
				    CALL SF_QSTN  ( iflinp, stid, istnm,
     +						    rlt, rln, rel, 
     +						    ispri, stat, coun, 
     +						    ier )
				    CALL SF_ASTN  ( iflout, 1, stid,
     +						    istnm, rlt, rln, 
     +						    rel, stat, coun,
     +						    ispri, n, iret )
				    IF  ( ( iret .ne. 0 )  .and. 
     +					  ( .not. sfadd ) )  THEN
					CALL ER_WMSG ( 'SFMOD', +3, ' ',
     +							iii )
					sfadd = .true.
				      ELSE IF  ( iret .eq. 0 )  THEN
					CALL SF_SSTN  ( iflout, stn, 
     +					  stid, in, rl1, rl2, re, 
     +					  ispri, iret )
				    END IF
				END IF
			    END IF
C
C*			    Compute parameters for output file and
C*			    write data.
C
			    IF  ( iret .eq. 0 )  THEN
			      CALL PC_SSTN  ( stn, id, rlt, rln, rel,
     +					      ispri, ihhmm, 1, ier )
			      CALL PC_CMLV  ( 1, data, cmpdat, chdat, 
     +					      iret )
C
C*			      Read in the outfile data.
C
                              CALL SF_RDAT ( iflout, outdata, jhhmm,
     +					     iret )
C
C*			      Replace missing data with computed values.
C
			      DO i = 1, ncols
 				  IF ( ERMISS ( outdata ( ipos (i) ) ) ) 
     +      			      outdata ( ipos (i) ) = cmpdat (i)
                              END DO
			      IF ( jhhmm .eq. IMISSD ) THEN
				  CALL TI_CTOI ( timout (itime),
     +					         itarr, ier )
			          jhhmm = itarr(4) * 100 + itarr(5)
			      END IF
C
C*			      Write out array.
C
      			      CALL SF_WDAT ( iflout, jhhmm, outdata, 
     +					     iret )
			    END IF
			END IF
		    END DO
		END IF
C
C*		Increment the time.
C
		itime = itime + 1
	    END DO
C
C*	    Call the dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Close open files.
C
	CALL SF_CLOS  ( iflinp, ier )
	CALL SF_CLOS  ( iflout, ier )
C
C*	Exit.
C
  	CALL IP_EXIT  ( iret )
	END
