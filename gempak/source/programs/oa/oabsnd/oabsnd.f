	PROGRAM  OABSND
C************************************************************************
C* PROGRAM OABSND							*
C*									*
C* This program performs a Barnes analysis for sounding data.		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86	From existing code			*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   Added guess file and multiple files	*
C* K. Brill/NMC          8/90   Fix multiple files			*
C* J. Nielsen/SUNYA	10/90	Use first guess in first pass		*
C*				Adjust weights only after first pass	*
C* K. Brill/NMC         10/90   Quit for NSTN=0; Output guess rms	*
C* K. Brill/NMC		06/91	Added EXTRAP and COSSLT; replace COSLSQ	*
C*				with COSLAT				*
C* S. Jacobs/EAI         3/93   Add J Nielsen's change to extrap        *
C* L. Williams/EAI       3/94   Clean up declarations of user input	*
C*				variables				*
C* S. Jacobs/NMC	 6/94	STNDEX*48 --> *72			*
C* L. Williams/EAI	 7/94	Removed call to OANUPD			*
C* S. Jacobs/NMC	 3/95	Added mand data only flag; Call SN_MAND	*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 6/98	Added guess flag to OA_BARN call	*
C* T. Lee/GSC		 3/99	Processed multiple files & missing data	*
C* D. Kidwell/NCEP	 5/99	Added mandat to OANPRM call             *
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* R. Tian/SAIC		 1/04	Added nuflg to DG_INTL call		*
C* R. Tian/SAIC		 2/04	Removed nuflg from DG_INTL call		*
C* R. Tian/SAIC          3/05   Modfied to adapt new file/time mgnt     *
C* J. Wu/SAIC		 4/05	Added bounds blocking			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C* S. Chiswell/SRS	 8/09	Moved SN_MAND call due to logic error	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	PARAMETER	( LLDTMX = LLSTFL * MMFILE )
C*
	CHARACTER	snfile*(LLMXLN), gdfile*(LLMXLN),guess*(LLMXLN),
     +                  snparm*(LLMXLN), area*(LLMXLN), dattim*(LLMXLN),
     +			cgamma*(LLMXLN), csrch*(LLMXLN), pass*(LLMXLN),
     +			stndex*(LLMXLN), levels*(LLMXLN),
     +			vcoord*(LLMXLN), filnam*(LLMXLN),
     +			finam(MMFILE)*(LLMXLN), cqcntl*(LLMXLN),
     +			stid(LLDTMX)*8, guesfun*(LLMXLN), 
     +                  coabnd*(LLMXLN), cproj*4
C*
	LOGICAL		done, respnd, proces, gsflag, mandat,
     +			found, mrgdat (MMFILE)
	CHARACTER	parms (MMPARM)*4, timdst (LLMXTM, MMFILE)*20, 
     +			udattm*20, prmsn (MMPARM, MMFILE)*4,
     +			gdattm*20, stnprm (MMPARM)*4,
     +                  stncnd (MMPARM)*12, gsdttm*48, stn*8,
     +			prmcnd (MMPARM)*12, timfil (LLMXTM, MMFILE)*20, 
     +			timall (LLMXTM)*20, stnam (MMPARM, LLDTMX)*8,
     +                  gsfunc (MMPARM)*72
	REAL		gltln (4), prjang (3)
	INTEGER		nqc (MMPARM, 2)
	INTEGER		iextnd (4), isnfln (MMFILE), ntimds (MMFILE)
        INTEGER         nprmsn (MMFILE), ivcin (MMFILE), ntfil (MMFILE),
     +			ivert (MMFILE) 
C*
	REAL		slat (LLDTMX), slon (LLDTMX), srow (LLDTMX),
     +			scol (LLDTMX), cosslt (LLDTMX)
	REAL		gelat (LLMXGD), gelon (LLMXGD), coslat (LLMXGD)
        INTEGER		stnin(LLDTMX), gpin (LLMXGD)
C*	
	REAL		rms  (LLOAGD), rlevel (LLMXLV), qcntl (LLOAGD)
	INTEGER		isn  (LLOAGD), iglevl (LLOAGD), igcord (LLOAGD)
	CHARACTER	gparm (LLOAGD)*4, voutc*4, gsparm (LLOAGD)*72
C*
	LOGICAL		extrap, eeee
C*
	PARAMETER	( ISZBUF = 300000 )
	REAL		data (ISZBUF)
C-----------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, iperr )
	IF  ( iperr .eq. 0 )  THEN
C
C*      Initialize grid library common area grdcmn.cmn
C 
            CALL GD_INIT  ( iperr )
	    CALL GG_INIT  ( 1, iperr )
	END IF
	IF  ( iperr .eq. 0 )  THEN
	    done = .false.
	  ELSE
	    done = .true.
	END IF
	CALL IP_IDNT  ( 'OABSND', ier )
C
C*	Initialize the DG library.
C
	CALL DG_INTL ( ier )
C
C*	Main loop to read in TAE parameters and perform analysis.
C
	DO WHILE  ( .not. done )
C
C*	  Set flag to indicate processing will be done.
C
	  proces = .true.
C
C*	  Read in the variables from the TAE.
C
	  CALL OANINP  ( snfile, gdfile, guess, snparm, stndex, area,
     +			 dattim, levels, vcoord, cgamma, csrch, pass, 
     +			 cqcntl, guesfun, coabnd, iperr )
C
C*	  Exit if there is an error
C
	  IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	   ELSE
C
C*	    Decode values for gamma, search, npass and QC threshold.
C
	    IF  ( proces )  THEN
		CALL ST_RLST  ( cgamma, '/', .3, 1, gamma, n, ier )
C
C*		Check for extrapolation flag in CSRCH.
C
		CALL ST_LCUC ( csrch, csrch, ier )
		iextrp = INDEX ( csrch, 'EX' )
		IF ( iextrp .ne. 0 ) THEN
		     extrap = .true.
		ELSE
		     extrap = .false.
		END IF
C*
		CALL ST_RLST  ( csrch, '/', 20., 1, search, n, ier )
		CALL ST_ILST  ( pass, '/', 2, 1, npass, n, ier )
		IF  ( ( gamma .lt. 0. ) .or. ( gamma .gt. 1. ) )  THEN
		    gamma = .3
		    CALL ER_WMSG  ( 'OABSND', +1, cgamma, ier )
		  ELSE IF  ( gamma .eq. 0 )  THEN
		    CALL ER_WMSG  ( 'OABSND', +2, cgamma, ier )
		    npass = 1
		END IF
		IF  ( ( search .le. 0. ) .or. ( search .gt. 50. ) ) THEN
		    search = 20.
		    CALL ER_WMSG  ( 'OABSND', +3, csrch, ier )
		END IF
		IF  ( ( npass .lt. 1 ) .or. ( npass .gt. 5 ) )  THEN
		    npass = 2
		    CALL ER_WMSG  ( 'OABSND', +4, pass, ier )
		  ELSE IF  ( npass .ne. 2 )  THEN
		    CALL ER_WMSG  ( 'OABSND', +5, ' ', ier )
		END IF
C*
		CALL ST_RLST ( cqcntl, ';', 0., MMPARM, qcntl, nn, ier )
		DO  i = 1, nn
		    IF  ( qcntl (i) .lt. 0. .or. guess .eq. ' ' )
     +			  qcntl (i) = 0.
		END DO	
	    END IF
C
C*	    Get list of requested level parameters.
C
	    IF ( proces ) THEN
	      CALL IN_PRMC  ( MMPARM, snparm, parms, prmcnd,
     +                        nparms, ier )
              IF ( nparms .eq. 0 .or. ier .ne. 0 ) THEN
                iret = -9
	        CALL ER_WMSG ( 'OABSND', iret, snparm, ier )
	        proces = .false.
	      END IF
	    END IF
C
C*          Get list of guess functions.
C
            IF ( proces ) THEN
                CALL IN_PARM ( MMPARM, guesfun, gsfunc, nguess, ier )
                DO i = 1, nparms
                    IF ( gsfunc (i) .eq. 'BLNK ' .or.
     +                   i .gt. nguess ) THEN
                        gsfunc (i) = parms (i)
                    END IF
                END DO
            END IF
C
C*	    Get levels.
C
	    CALL LV_INPT  ( levels, LLMXLV, vcoord, nlevel,
     +		            rlevel, levtyp, voutc,  ilvert,
     +			    mandat, iret )
C
C*	    Check that this is not level range.
C
	    IF  ( levtyp .ne. 1 )  THEN
	      nlevel = 0
	      iret   = -10
	      CALL ER_WMSG  ( 'OABSND', iret, ' ', ier )
	    END IF
C
C*	    Get list of station parameters.
C
	    CALL IN_PRMC  ( MMPARM, stndex, stnprm, stncnd,
     +			    nstnpm, ier )
C
C*	    Check that there are some computable parameters.
C
	    ngrid = nstnpm + nparms * nlevel
	    IF  ( ngrid .eq. 0 )  THEN
	      iret = -9
	      CALL ER_WMSG  ( 'OABSND', iret, ' ', ier )
	    END IF
	    IF ( iret .ne. 0 ) proces = .false.
C*
	    IF ( proces ) THEN
C
C*	      Save the level, vertical coordinate and parameter name for
C*	      each grid to be computed.
C
	      igrid = 0
	      DO  il = 1, nlevel
	        DO  ip = 1, nparms
	  	  igrid = igrid + 1
		  iglevl (igrid) = NINT ( rlevel (il) )
		  igcord (igrid) = ilvert
		  gparm  (igrid) = parms (ip)
		  gsparm (igrid) = gsfunc (ip) 
		  qcntl  (igrid) = qcntl (ip) 
	        END DO
   	      END DO
C*
	      DO  ip = 1, nstnpm
	        igrid = igrid + 1
	        iglevl (igrid) = 0
	        igcord (igrid) = 0
	        gparm  (igrid) = stnprm (ip)
		gsparm (igrid) = stnprm (ip)
	      END DO
C*
	    END IF
C
C*	    Open the sounding data files.
C
	    IF  ( proces )  THEN
		CALL FL_MFIL ( snfile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) CALL ER_WMSG ( 'FL', iret, ' ', ier )    
C
C*              Close all grid files in order to open surface file.
C
                CALL DG_NEND ( iret )
		CALL OANOPN  ( filnam, finam, timdst, ntimds, 
     +			       nfiles, prmsn, nprmsn, ivcin, iret )
		IF  ( iret .ne. 0 )  proces = .false.
	    END IF
C
C*	    Get the analysis times; first check to see if an actual
C*          time is requested.  If an actual time is not requested,
C*          the first or last time in the first data set is used
C*          for the time search.
C
	    CALL ST_LCUC ( dattim, dattim, ier )
	    IF ( dattim .eq. 'LAST' ) THEN
	      dattim = timdst ( ntimds (1), 1 )
	    ELSE IF ( dattim .eq. 'FIRST' ) THEN
	      dattim = timdst ( 1, 1 )
	    END IF
C*
	    IF  ( proces )  THEN
	      i = 0
	      DO WHILE ( proces .and. i .lt. nfiles )
	        i = i + 1
		CALL TI_FIND  ( dattim, ntimds (i), timdst (1,i),
     +				udattm, ntfil (i), timfil (1,i), iret )
	      END DO
C
C*	      Get the list of OA times.
C
	      ntall = 0
	      DO  i = 1, nfiles
		nt = ntfil (i)
		DO  j = 1, nt
		    found = .false.
		    DO  k = 1, ntall
			IF  ( timfil (j,i) .eq. timall (k) )
     +			      found = .true.
		    END DO
C
		    IF  ( .not. found )  THEN
			ntall = ntall + 1
			timall (ntall) = timfil (j,i)
		    END IF
		END DO
	      END DO
	    END IF
C
C*	    Sort the time.
C
	    CALL TI_SORT ( ntall, timall, timall, ier )
C
C*	    Start the loop over times.
C
	    itime = 0
	    DO WHILE ( proces .and. itime .lt. ntall )
	      itime = itime + 1
              gdattm = timall ( itime )
C
C*	      Open the grid file.
C
	      CALL OA_GFIL  ( gdfile, guess, gdattm, area, gsflag,
     +	                      gsdttm, cproj, prjang, deltan, gltln,
     +                        kex, key, iextnd, iret )
	      kexy = kex * key
	      IF  ( iret .ne. 0 )  THEN
		proces = .false.
	        ELSE IF  ( kexy .gt. LLMXGD )  THEN
		  iret = -4
		  CALL ER_WMSG ( 'OABSND', iret, ' ', ier )
		  proces = .false.
	        ELSE
		  CALL OA_LTLN  ( kex, key, iextnd, gelat, gelon,
     +		                  coslat, iret )
		  IF  ( iret .ne. 0 )  proces = .false.
	      END IF
C                    
C*	      Set the bounds - tranformed to sys_N using the
C*            projection information retrieved from grid files.
C		    		    
	      IF ( itime .eq. 1 ) THEN
                  CALL ST_NULL ( coabnd, coabnd, lens, iret )
		  CALL ST_NULL ( cproj, cproj, lens, iret )
	          CALL OA_BNDSET ( coabnd, cproj, prjang, gltln, iret )
		  CALL ST_RNUL ( coabnd, coabnd, lens, iret )
		  CALL ST_RNUL ( cproj, cproj, lens, iret )
              END IF
C
C*	      Open input files and loop over the files to get the
C*	      data. First initialize NSTN, the number of stations.
C
	      nstn = 0
	      CALL DG_NEND ( ier )
              DO inn = 1, nfiles
	        nsav = nstn
C
C*		Open the upper-air file.
C
		CALL SN_OPNF  ( finam (inn), .false., isnfln (inn),
     +			   	iflsrc, nprmsn (inn), prmsn (1, inn),
     +				ivert (inn), mrgdat (inn), iret )
C
C*	    	Set the mandatory data only flag.
C
		CALL SN_MAND ( isnfln ( inn ), mandat, ier )
C
C*		Set the area for input data.
C
		IF  ( proces )  THEN
		    CALL LC_SARE ( area, isnfln (inn), stn, iret )
		    IF  ( iret .ne. 0 )  proces = .false.
C
C*                  Write warning if the area is not the DATA area.
C
                    IF  ( area .ne. 'DATA' )  THEN
	                CALL ER_WMSG  ( 'OABSND', +6, area, ier )
		    END IF
		END IF
C
C*		Get the valid parameters.
C
	        CALL OANPRM  ( ivcin (inn),
     +                         parms, prmcnd, nparms, stnprm,
     +		  	       stncnd, nstnpm, nlevel, prmsn (1, inn),
     +                         nprmsn (inn), mandat, iret )
	        IF  ( iret .ne. 0 )  THEN
C
C*	          Write message about non-contributing file.
C
	          CALL ER_WMSG ( 'OABSND', +10, finam (inn), ier )
	        ELSE
C
C*		    Read in the sounding data.
C
	            CALL SN_STIM  ( isnfln (inn), gdattm, ier )
	            CALL SN_BEGS  ( isnfln (inn), ier )
		    CALL OANDTA  ( isnfln (inn), nparms, rlevel, nlevel,
     +				   ilvert, nstnpm, ngrid, data, slat, 
     +				   slon, cosslt, nstn, stid, iret )
	            IF ( iret .ne. 0 ) THEN
                      CALL ER_WMSG ( 'OABSND', +10, finam (inn), ier )
	              nstn = nsav
	            END IF
	        END IF
C
C*		Close the data file.
C	
		CALL SN_CLOS  ( isnfln (inn), ier )
	      END DO
	      IF ( nstn .eq. 0 ) proces = .false.
C
C*	      Give user a chance to exit.
C
	      IF  ( proces ) THEN
	        CALL OANDSP  ( gdfile, gltln, iextnd, kex, key, 
     +		  	       deltan, filnam, area, parms, nparms, 
     +			       rlevel, nlevel, vcoord, stnprm, nstnpm,
     +			       gdattm, gamma, search, npass, guess,
     +			       gsflag, gsdttm, extrap, qcntl, 
     +                         coabnd, iret )
	        IF  ( iret .ne. 0 ) proces = .false.
	      END IF
C
C*	      Analyze data. 
C
	      IF  ( proces )  THEN
C
C*		    Write the time.
C
		    WRITE  ( 6, 1000 )  gdattm
1000		    FORMAT ( / ' Processing data at time ', A )
C
C*		    Continue unless there were too many or too few 
C*		    stations.
C
		    IF  ( iret .eq. 0 )  THEN
C
C*			Set pointer for grid data in large buffer.
C
			ipsint = ( nstn * ngrid ) + 1
			ipgrid = ( 2 * ipsint ) - 1
C
C*			Check for room for all grid data.
C
			igsize = kexy * ngrid
			itotal = igsize + ipgrid
			IF  ( itotal .gt. ISZBUF )  THEN
			    iret = -5
			    CALL ER_WMSG  ( 'OABSND', iret, ' ', ier )
			END IF
		    END IF
C
C*		    Convert lat/lon to row/col.
C
		    IF  ( iret .eq. 0 )  THEN
			CALL OA_BOXC  ( slat, slon, nstn, iextnd, srow,
     +					scol, iret )
		    END IF
C
C*		    Compute weighting factor and search radius.
C
		    IF ( iret .eq. 0 )
     +		    CALL OA_WFSR  ( deltan, search, weight, srad, iret )
C
C*		    Initialize the grid.
C
		    IF  ( iret .eq. 0 )  THEN
			CALL OA_IGRD  ( ngrid, kex, key, kexy, 
     +		                        iextnd, iglevl, igcord,
     +				        gsflag, gsdttm, gsparm,
     +                                  data ( ipgrid ), iret )
	            END IF
C
C*		    Compute differences from initial guess, if any.
C
		    IF  ( iret .eq. 0 )  THEN
			IF  ( gsflag )  THEN
			    CALL OA_GINT  ( ngrid, nstn, stid, data, 
     +					    srow, scol, kex, key, qcntl,
     +					    data (ipgrid), iextnd,
     +					    data (ipsint), rms, isn,
     +					    nqc, stnam, iret )
			  ELSE
			    CALL OA_SINT  ( ngrid, nstn, data, srow, 
     +					    scol, kex, key, 
     +					    data (ipgrid), iextnd, 
     +					    data (ipsint), rms, isn, 
     +					    iret )
			END IF
		    END IF
C
	            IF ( iret .eq. 0 .and. gsflag ) THEN
C
C*			Write station IDs to terminal.
C
			CALL OANSTN   ( gparm, ngrid, iglevl, stnam,
     +					nqc, qcntl, iret )
C
C*			Write guess field RMS values to terminal.
C
		        ipass = 0
			CALL OANRMS  ( ipass, gparm, iglevl, ngrid,
     +				       rms, isn, ier )
                    END IF
C
C*		    Check out grid points/stations within the blocking bound 
C
                    IF ( iret .eq. 0 ) THEN
		        CALL OA_BNDINB ( 'M', kexy, gelat, gelon, gpin, iret )
			CALL OA_BNDINB ( 'M', nstn, slat, slon, stnin, iret )
		    END IF
C
C*		    
                    IF ( iret .eq. 0 ) THEN
                    
C
C*			Do analysis passes.
C
			DO  ipass = 1, npass
C
C*			    Adjust the weighting factor and search 
C*			    radius after the first pass.
C
			    IF  ( ipass .eq. 2 )  THEN
				weight = weight * gamma
				srad   = srad   * gamma
			    END IF
C
C*                          Check for last pass. Use the "extrapolation"
C*                          procedure for all but the last pass. Let
C*                          the user determine what to do for the last
C*			    pass.
C
			    IF  ( ipass .eq. npass )  THEN
				eeee = extrap
			    ELSE
				eeee = .true.
			    END IF

C
C*			    Perform the analysis.
C
			    CALL OA_BARN  ( ngrid, weight, srad,
     +					    kexy, nstn, data (ipsint), 
     +                                      slat, slon, gelat, gelon,
     +					    coslat, cosslt, extrap,
     +					    gsflag, gpin, stnin,
     +                                      isn, data (ipgrid), iret )
C
C*			    Interpolate analysis back to stations.
C
			    CALL OA_SINT  ( ngrid, nstn, data, srow, 
     +					    scol, kex, key, 
     +					    data (ipgrid), iextnd, 
     +					    data (ipsint), rms, isn, 
     +					    iret )
C
C*			    Write RMS values to terminal.
C
			    CALL OANRMS  ( ipass, gparm, iglevl, ngrid,
     +					   rms, isn, iret )
			END DO
C
C*			Write grid to grid file.
C
			CALL OA_WGRD  ( gdattm, ngrid, gparm,
     +					iglevl, igcord, data (ipgrid), 
     +					kex, key, iextnd, iret )
		    END IF
	      END IF
	    END DO
C
C*	    Prompt for next analysis to be done.
C
	    CALL IP_DYNM  ( done, ier )
C
C*	    End of processing loop
C
	  END IF
	END DO
C
C*	Final error messages and exit from the TAE.
C
	IF  ( iperr .ne. 0 )  CALL ER_WMSG ( 'OABSND', iperr, ' ', ier )
	CALL IP_EXIT  ( iret )
C*
	END
