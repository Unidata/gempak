	 SUBROUTINE OABSDR ( gelat, gelon, coslat, data, infoflg, iret )
C************************************************************************
C* OABSDR								*
C*									*
C* This subroutine takes all the original OABSFC input parameters and	*
C* performs a Barnes analysis for surface data.				*
C*									*
C* OABSDR ( GELAT, GELON, COSLAT, DATA, INFOFLG, IRET )			*
C*									*
C* Input parameters:							*
C*									*
C* Output parameters:							*
C*	GELAT(*)	REAL		Latitude in degrees		*
C*	GELON(*)	REAL		Longitude in degrees		*
C*	COSLAT(*)	REAL		Cosine of latitude		*
C*	DATA(*)		REAL		Surface data			*
C*	INFOLFG		LOGICAL		Info flag			*
C*	IRET		INTEGER		Return code			*
C*                                        0 = normal return             *
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/86	From existing code			*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* K. Brill/GSC          4/90   Added first guess capability		*
C* K. Brill/GSC          4/90   Added multiple sfc file code		*
C* K. Brill/NMC          8/90   Fix multiple sfc file code		*
C* K. Brill/NMC         10/90   Add J. Nielsen's changes from OABSND	*
C* K. Brill/NMC		06/91	Added EXTRAP and COSSLT; replace 	*
C* 				COSLSQ with COSLAT			*
C* K. Brill/NMC		02/92	Eliminate extra -9 messages		*
C* S. Jacobs/EAI	 3/93	Add J Nielsen's change to extrap	*
C* L. Williams/EAI	 7/94	Removed called to OACUPD		*
C* K. Tyle/GSC		 8/96	Added FL_MFIL to search for file type	*
C* S. Maxwell/GSC        7/97   Increased input character length        *
C* S. Jacobs/NCEP	 6/98	Added guess flag to OA_BARN call	*
C* T. Lee/GSC		 2/99	Processed multiple surface files	*
C* T. Lee/GSC		 3/99	Added QC parameter			*
C* T. Lee/GSC		 3/99	Handled missing data; Increased ISZBUF	*
C* T. Lee/GSC		 3/99	Called OACSTN				*
C* K. Brill/HPC		 4/03	CALL DG_INTL				*
C* R. Tian/SAIC		 3/05	Modfied to adapt new file/time mgnt	*
C* J. Wu/SAIC		 4/05	Added bounds blocking			*
C* R. Tian/SAIC		 4/05	Modified from oabsfc.f			*
C* M. Li/SAIC		 4/05	Added data and infoflg			*
C* D.W.Plummer/NCEP	 5/05	Init DG library				*
C* B. Yin/SAIC		 6/05	Add GDATTIM, GFUNC, GLEVEL and GVCORD	*
C* B. Yin/SAIC		 7/05	check numbers of GDATTIM and DATTIM	*
C* B. Yin/SAIC		 8/05	if GDATTIM is blank, use DATTIM 	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	PARAMETER	( LLDTMX = LLSTFL * MMFILE )
	REAL		gelat(*), gelon(*), coslat(*), data(*)
	LOGICAL		infoflg
	CHARACTER	sffile*(LLMXLN), gdfile*(LLMXLN), guess*(LLMXLN),
     +			sfparm*(LLMXLN), area*(LLMXLN), dattim*(LLMXLN),
     +			cgamma*(LLMXLN), csrch*(LLMXLN), pass*(LLMXLN),
     +			filnam*(LLMXLN), finam(MMFILE)*(LLMXLN),
     +			cqcntl*(LLMXLN), guesfun*(LLMXLN), stid(LLDTMX)*8,
     +                  coabnd*(LLMXLN), cproj*4, gdattim*(LLMXLN),
     +			gfunc*(LLMXLN), glevel*(LLMXLN), gvcord*(LLMXLN)
C*
	LOGICAL		done, proces, gsflag, found
	CHARACTER	prmsf (MMPARM, MMFILE)*4, parms (MMPARM)*4,
     +                  timdst (LLMXTM, MMFILE)*20, ogdattm*20,
     +			gdattm*20, gsdttm*48, udattm*20, stn*8,
     +			timfil (LLMXTM,MMFILE)*20, timall (LLMXTM)*20,
     +			stnam (MMPARM, LLDTMX)*8, otimall (LLMXTM)*20,
     +			ogfunc (MMPARM)*4, oglevel (MMPARM)*10,
     +			ogvcord (MMPARM)*10, ugvcord (MMPARM)*10 
	CHARACTER	prmcnd (MMPARM)*12, gsfunc (MMPARM)*72
	REAL		gltln (4), prjang (3)
	INTEGER		isffln (MMFILE), ntimds (MMFILE), iextnd (4),
     +                  nprmsf (MMFILE), ntfil (MMFILE)
C*
	REAL		slat (LLDTMX), slon (LLDTMX), srow (LLDTMX),
     +			scol (LLDTMX), cosslt (LLDTMX)
        INTEGER		stnin(LLDTMX), gpin (LLMXGD)
C*	
	REAL		rms  (MMPARM), qcntl (MMPARM)
	INTEGER		isn  (MMPARM), levels (MMPARM), 
     +			ivcord (MMPARM), nqc (MMPARM, 2),
     +			igvcord (MMPARM), iglevel (MMPARM)
C*
	LOGICAL		extrap, eeee
C*
C	PARAMETER	( ISZBUF = 300000 )
C	REAL		data (ISZBUF)
C*
	DATA		levels  / MMPARM * 0 /
	DATA		ivcord  / MMPARM * 0 /
	DATA		igvcord / MMPARM * 0 /
C-----------------------------------------------------------------------
	iret = 0
	proces = .true.
C
C*	Initialize DG library.
C
	CALL DG_INTL ( iret )
C
C*	Read in the variables from the TAE.
C
	CALL OACINP  ( sffile, gdfile, guess, sfparm, area, dattim,
     +		       cgamma, csrch, pass, cqcntl, guesfun, 
     +                 coabnd, gdattim, gfunc, glevel, gvcord, iperr )
C
C*      Check if the numbers of dattim and gdattim are the same
C
        CALL ST_LSTR ( dattim, lendt, ier )
	IF ( lendt .gt. 0 ) THEN 
	   numdt = 1
	  ELSE
	   numdt = 0
	END IF
C
	ii = 1
	DO WHILE ( ii .le. lendt ) 
	    IF ( dattim ( ii:ii ) .eq. ';' ) THEN
		numdt = numdt + 1
	    END IF
	    ii = ii + 1
	END DO
C
        CALL ST_LSTR ( gdattim, lengdt, ier )
	IF ( lengdt .gt. 0 ) THEN
	   numgdt = 1
	  ELSE
	   numgdt = 0
	END IF
C
	ii = 1
	DO WHILE ( ii .le. lengdt ) 
	    IF ( gdattim ( ii:ii ) .eq. ';' ) THEN
		numgdt = numgdt + 1
	    END IF
	    ii = ii + 1
	END DO
C
	IF ( ( numgdt .ne. 0 ) .and. ( numdt .ne. numgdt ) ) THEN
	   proces = .false.
	   CALL ER_WMSG ( 'OABSFC', -12, ' ', ier )
	END IF
C
C*	Exit if there is an error
C
	IF  ( iperr .ne. 0 )  THEN
	    done = .true.
	  ELSE
C
C*	    Decode values for gamma, search, npass and QC threshold.
C
	    IF ( proces ) THEN
	        CALL ST_RLST ( cgamma, '/', .3, 1, gamma, n, ier )
C
C*              Check for extrapolation flag in CSRCH.
C
                CALL ST_LCUC ( csrch, csrch, ier )
                iextrp = INDEX ( csrch, 'EX' )
                IF ( iextrp .ne. 0 ) THEN
                    extrap = .true.
                  ELSE
                    extrap = .false.
                END IF
C*
		CALL ST_RLST ( csrch, '/', 20., 1, search, n, ier )
		CALL ST_ILST ( pass, '/', 2, 1, npass, n, ier )
		IF ( gamma .lt. 0. .or. gamma .gt. 1. ) THEN
		    gamma = .3
		    CALL ER_WMSG ( 'OABSFC', +1, cgamma, ier )
		  ELSE IF ( gamma .eq. 0 )  THEN
		    CALL ER_WMSG ( 'OABSFC', +2, cgamma, ier )
		    npass = 1
		END IF
		IF ( search .le. 0. .or. search .gt. 50. ) THEN
		    search = 20.
		    CALL ER_WMSG ( 'OABSFC', +3, csrch, ier )
		END IF
		IF ( npass .lt. 1 .or. npass .gt. 5 ) THEN
		    npass = 2
		    CALL ER_WMSG ( 'OABSFC', +4, pass, ier )
		  ELSE IF ( npass .ne. 2 )  THEN
		    CALL ER_WMSG ( 'OABSFC', +5, ' ', ier )
		END IF
C*
		CALL ST_RLST ( cqcntl, ';', 0., MMPARM, qcntl,
     +		               nn, ier )
		DO  i = 1, nn
		    IF  ( qcntl (i) .lt. 0. .or. guess .eq. ' ' )  
     +		        qcntl (i) = 0.
		END DO
	    END IF
C
C*	    Get list of requested parameters.
C
	    IF ( proces ) THEN
	        CALL IN_PRMC  ( MMPARM, sfparm, parms, prmcnd,
     +	                        nparms, ier )
	        IF ( nparms .eq. 0 .or. ier .ne. 0 )  THEN
	            iret = -9
	            CALL ER_WMSG  ( 'OABSFC', iret, sfparm, ier )
	            proces = .false.
	        END IF
	    END IF
C
C*	    Get list of guess functions.
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
C*	    Open the surface data files.
C
	    IF ( proces )  THEN
	        CALL FL_MFIL ( sffile, ' ', filnam, iret )
		IF ( iret .ne. 0 ) THEN
		    CALL ER_WMSG ( 'FL', iret, ' ', ier )    
		END IF
C
C*              Close all grid files in order to open surface file.
C
                CALL DG_NEND ( iret )
		CALL OACOPN ( filnam, finam, timdst, ntimds,
     +		              nfiles, prmsf, nprmsf, iret )
		IF ( iret .ne. 0 )  proces = .false.
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
	    IF ( proces ) THEN
	        i = 0
	        DO WHILE ( proces .and. i .lt. nfiles )
	            i = i + 1
	            CALL TI_FIND ( dattim, ntimds (i), timdst (1,i),
     +			           udattm, ntfil (i), timfil (1,i),
     +                             iret )
	        END DO
C
C*	        Get the list of OA times.
C
	        ntall = 0
	        DO i = 1, nfiles
		    nt = ntfil (i)
		    DO j = 1, nt
		        found = .false.
		        DO k = 1, ntall
		            IF ( timfil (j,i) .eq. timall (k) ) 
     +			        found = .true.
		        END DO
C
		        IF ( .not. found ) THEN
			    ntall = ntall + 1
			    timall (ntall) = timfil (j,i)
		        END IF
		    END DO
	        END DO
	    END IF
C
C*	    Sort the times.
C
	    CALL TI_SORT ( ntall, timall, timall, ier )
C
C*	    Get the grid date/time
C
	    CALL ST_LCUC ( gdattim, gdattim, ier )
	    CALL ST_CLST ( gdattim,';', ' ',LLMXTM, otimall,
     +						notim, ier )
C
C*	    Get GFUNC, GLEVEL, and GVCORD
C
	    CALL ST_LCUC ( gfunc, gfunc, ier )
	    CALL ST_LCUC ( glevel, glevel, ier )
	    CALL ST_LCUC ( gvcord, gvcord, ier )
C* 
	    CALL ST_CLST ( gfunc,';',' ', MMPARM, ogfunc, ngfunc, ier )
	    CALL ST_CLST ( glevel,';',' ',MMPARM, oglevel, nglevel, ier )
	    CALL ST_CLST ( gvcord,';',' ',MMPARM, ogvcord, ngvcord, ier )
C*
	    DO ii = 1, nparms
	       IF ( ogfunc ( ii ) .eq. ' ' ) THEN
	          ogfunc ( ii ) = parms ( ii )
	       END IF
C*
	       IF  (( ii .eq. 1 ) .and. ( oglevel ( ii ) .eq. ' ' )) THEN
		   oglevel ( ii ) = '0'
		 ELSE IF ( ( ii .gt. 1 ) .and. 
     +			   ( oglevel ( ii ) .eq. ' ' ) ) THEN
		   oglevel ( ii ) = oglevel ( ii - 1 )
	       END IF
C*
	       CALL LV_DECD ( oglevel ( ii ), rlevel, ier )
	       iglevel ( ii ) = rlevel
C*
	       IF  (( ii .eq. 1 ) .and. ( ogvcord ( ii ) .eq. ' ' )) THEN
		   ogvcord ( ii ) = 'NONE'
		 ELSE IF ( ( ii .gt. 1 ) .and. 
     +			   ( ogvcord ( ii ) .eq. ' ' ) ) THEN
		   ogvcord ( ii ) = ogvcord ( ii - 1 )
	       END IF
C*
	       CALL LV_CORD ( ogvcord ( ii ), ugvcord ( ii ), 
     +			      igvcord ( ii ), ier )
	    END DO
C
C*	    Start the loop over times.
C
	    itime = 0
	    DO WHILE ( proces .and. itime .lt. ntall )
	        itime = itime + 1
		gdattm = timall ( itime )
C*
		CALL OABSTM ( timall ( itime ), otimall ( itime ),
     +			      ogdattm, ier )
C
C*	        Process the grid files.
C
	        CALL OA_GFIL ( gdfile, guess, gdattm, area, gsflag,
     +		               gsdttm, cproj, prjang, deltan, gltln,
     +                         kex, key, iextnd, iret )
	        kexy = kex * key
	        IF ( iret .ne. 0 ) THEN
		    proces = .false.
	          ELSE IF ( kexy .gt. LLMXGD ) THEN
		    iret = -4
		    CALL ER_WMSG ( 'OABSFC', iret, ' ', ier )
		    proces = .false.
	          ELSE
		    CALL OA_LTLN  ( kex, key, iextnd, gelat, gelon,
     +		                    coslat, iret )
		    IF ( iret .ne. 0 )  proces = .false.
	        END IF
C                    
C*	        Set the bounds - tranformed to sys_N using the
C*              projection information retrieved from grid files.
C*		Just need to do it once.		    
C		    		    
		IF ( itime .eq. 1 ) THEN
                    CALL ST_NULL ( coabnd, coabnd, lens, iret )
		    CALL ST_NULL ( cproj, cproj, lens, iret )
                    CALL OA_BNDSET ( coabnd, cproj, prjang,
     +                               gltln, iret )
		    CALL ST_RNUL ( coabnd, coabnd, lens, iret )
		    CALL ST_RNUL ( cproj, cproj, lens, iret )
                END IF
C
C*	        Open input files and loop over the files to get the 
C*		data. First, initialize NSTN, the number of stations.
C
	        nstn = 0	    
		CALL DG_NEND ( ier )
	        DO iff = 1, nfiles
C
	            nsav = nstn
C
C*		    Open the surface file.
C
		    CALL SF_OPNF ( finam (iff), .false.,
     +		                   isffln (iff), iflsrc,
     +                             nprmsf (iff), prmsf (1,iff), 
     +				   iret )
		    IF ( iret .ne. 0 ) proces = .false.
C
C*		    Set search area.
C
		    IF ( proces ) THEN
		        CALL LC_SARE ( area, isffln (iff),
     +			               stn, iret )
		        IF ( iret .ne. 0 ) proces = .false.
C
C*                      Write warning if the area is not the DATA
C*                      area.
C
                        IF  ( area .ne. 'DATA' )  THEN
	                    CALL ER_WMSG  ( 'OABSFC', +6, area,
     +			                    ier )
		        END IF
		    END IF
C
C*	            Get the valid parameters.
C
	            IF ( proces ) THEN
		        CALL OACPRM ( isffln (iff), gdattm,
     +                      parms, prmcnd, nparms,
     +                      prmsf (1, iff), nprmsf (iff), nstn,
     +                      data, slat, slon, stid, cosslt, iret )

		        IF  ( iret .ne. 0 )  THEN
	                    nstn = nsav
C
C*		            Write error message about
C*                          non-contributing file.
C
	                    iret = +9
	                    CALL ER_WMSG ( 'OABSFC', iret,
     +		                           finam (iff), ier )
	                END IF
	            END IF
C
C*		    Close surface data file.
C
		    CALL SF_CLOS ( isffln (iff), ier )
	        END DO
	        IF ( nstn .eq. 0 ) THEN
	            iret = -9
	            CALL ER_WMSG ( 'OABSFC', iret, sfparm, ier )
	            proces = .false.
	        END IF
C
C*	        Give user a chance to exit.
C
	        IF ( proces .and. infoflg ) THEN
 	            CALL OACDSP ( gdfile, gltln, iextnd, kex, key, 
     +			    deltan, filnam, area, parms,
     +                      nparms, gdattm, gamma, search, npass,
     +                      guess, gsflag, gsdttm, extrap, qcntl,
     +                      coabnd, ogdattm, ogfunc, oglevel,
     +			    ogvcord, iret )
 	            IF ( iret .ne. 0 ) proces = .false.
	        END IF
C
C*	        Analyze data for this time.
C
	        IF ( proces ) THEN
C
C*		    Write the time.
C
		    IF ( infoflg )  THEN
		        WRITE ( 6, 1000 ) gdattm
1000		        FORMAT ( / ' Processing data at time ', A )
		    END IF
C
C*		    Set pointer for grid data in large buffer.
C
		    ipsint = ( nstn * nparms ) + 1
		    ipgrid = ( 2 * ipsint ) - 1
C
C*		    Check for room for all grid data.
C
		    igsize = kexy * nparms
		    itotal = igsize + ipgrid
		    IF  ( itotal .gt. LLMXTG )  THEN
		        iret = -5
			CALL ER_WMSG  ( 'OABSFC', iret, ' ', ier )
		    END IF
C
C*		    Convert lat/lon to row/col.
C
		    IF ( iret .eq. 0 ) THEN
		        CALL OA_BOXC ( slat, slon, nstn, iextnd,
     +			               srow, scol, iret )
		    END IF
C
C*		    Compute weighting factor and search radius.
C
	            IF ( iret .eq. 0 ) THEN
	                CALL OA_WFSR ( deltan, search, weight,
     +		                       srad, iret )
	            END IF
C
C*		    Initialize the grid.
C
		    IF ( iret .eq. 0 ) THEN
		        ngrid = nparms
			CALL OA_IGRD ( ngrid, kex, key, kexy,
     +			               iextnd, levels, ivcord,
     +                                 gsflag, gsdttm, gsfunc,
     +                                 data ( ipgrid ), iret )
                    END IF
C
C*		    Compute differences from initial guess.
C
                    IF ( iret .eq. 0 ) THEN
			IF ( gsflag ) THEN
			    CALL OA_GINT  ( ngrid, nstn, stid, data, 
     +				        srow, scol, kex, key, qcntl,
     +					data (ipgrid), iextnd, 
     +					data (ipsint), rms, isn, 
     +					nqc, stnam, iret )
			  ELSE 
			    CALL OA_SINT  ( ngrid, nstn, data, srow,
     +                                  scol, kex, key,
     +                                  data (ipgrid), iextnd,
     +                                  data (ipsint), rms, isn,
     +                                  iret )
			END IF
	   	    END IF
C
                    IF ( iret .eq. 0 .and. gsflag ) THEN
C
C*		        Write station IDs to terminal.
C
			CALL OACSTN ( parms, nparms, stnam, nqc, 
     +				      qcntl, iret ) 
C
C*			Write RMS values to terminal.
C
                        ipass = 0
			IF ( infoflg )  THEN
			    CALL OACRMS ( ipass, parms, nparms, rms,
     +				          isn, iret )
     			END IF
                    END IF
C
C*		    Check out grid points/stations within the blocking bound 
C
		    IF ( iret .eq. 0 ) THEN
                        CALL OA_BNDINB ( 'M', kexy, gelat, 
     +                                        gelon, gpin, iret )
			CALL OA_BNDINB ( 'M', nstn, slat, 
     +			                      slon, stnin, iret )
		    END IF
C                        
C*       
		    IF ( iret .eq. 0 ) THEN
C
C*		        Do analysis passes.
C
			DO ipass = 1, npass
C
C*			    Adjust the weighting factor and search 
C*			    radius after the first pass.
C
			    IF ( ipass .eq. 2 )  THEN
			        weight = weight * gamma
				srad   = srad   * gamma
			    END IF
C
C*			    Check for last pass. Use the
C*                          "extrapolation" procedure for all but the
C*                          last pass. Let the user determine what to
C*                          do for the last pass.
C
			    IF ( ipass .eq. npass ) THEN
			        eeee = extrap
			      ELSE
			        eeee = .true.
			    END IF
C
C*			    Perform the analysis.
C
     		            CALL OA_BARN ( ngrid, weight, srad,
     +			  	    kexy, nstn, data (ipsint),
     +			            slat, slon, gelat,
     +				    gelon, coslat, cosslt,
     +				    eeee, gsflag, gpin, stnin,
     +			            isn, data (ipgrid), iret )
C
C*			    Interpolate data back to stations.
C
			    CALL OA_SINT ( ngrid, nstn, data, srow, 
     +				       scol, kex, key, 
     +				       data (ipgrid), iextnd, 
     +				       data (ipsint), rms, isn, 
     +				       iret )
C
C*			    Write RMS values to terminal.
C
			    IF ( infoflg )  THEN
			        CALL OACRMS ( ipass, parms, nparms,
     +			                      rms, isn, iret )
     			    END IF

		        END DO
C
C*			Write grid to grid file.
C
			CALL OA_WGRD ( ogdattm, ngrid, ogfunc, iglevel,
     +			               igvcord, data (ipgrid), kex,
     +                                 key, iextnd, iret )
C
		    END IF
	        END IF
	    END DO
	END IF
C*
	RETURN
	END
