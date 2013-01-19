	PROGRAM BFR2GP
C************************************************************************
C* PROGRAM BFR2GP							*
C*									*
C* This programs decodes Jack Woollen BUFR files and writes data into   *
C* GEMPAK files.  The input BUFR file must be blocked.			*
C**									*
C* Log:									*
C* K. Brill/EMC		 2/97						*
C* K. Brill/EMC		 2/97	Changes for area, ID, shipfiles, IHHMM;	*
C*				Unmerged sounding files			*
C* K. Brill/EMC		 4/97	Time range in DATTIM & added SFFSRC	*
C* K. Brill/EMC		 7/98	Changes for CATFLG (ON 29 categories)	*
C*				Changes for screening parameters	*
C* K. Brill/HPC		 3/01   CALL BFRMRG				*
C* L. Sager/NCO          5/05   Add data thinning routine               *
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER	snefil*132, snoutf*132, sfoutf*132, snprmf*132,
     +			sfprmf*132, area*32, timstn*32, sffsrc*64,
     +			dattim*48, thndat*32
	CHARACTER	time*20, dtmlst*20, stid*8, sidlst*8
	CHARACTER*20	trange(2)
	CHARACTER*4	parmsn (MMPARM), parmsf (MMPARM+13)
	CHARACTER*80	bprmsn (12), bprmsf (12)
	REAL		sndata (LLMXLV*MMPARM), sfdata (2*MMPARM)
	REAL		sndat2 (LLMXLV*MMPARM)
	REAL		sncfac (MMPARM), sfcfac (MMPARM+13),
     +			snctrm (MMPARM), sfctrm (MMPARM+13)
	REAL		sfcbuf (MMPARM)
        CHARACTER*8     keepers(2000,6)      
	INTEGER		irepsf (12)
	CHARACTER*80	catprm
	REAL		catvls (12), grltln (4), centrd (2)
	INTEGER		ncatvl (12)
	CHARACTER	cdproj*32, srcnam*8, stat*2, coun*2
	LOGICAL		done, proces, ok, ship, open, upaflg, chktim
	LOGICAL		catflg
	INCLUDE		'ERMISS.FNC'

        DATA            icount/0/
        DATA            iflag/1/
C------------------------------------------------------------------------
C*	Initialize TAE.
C
	CALL IP_INIT  ( respnd, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -1
	    CALL ER_WMSG  ( 'BFR2GP', iret, ' ', ier )
	    CALL SS_EXIT
	END IF
	CALL IP_IDNT  ( 'BFR2GP', ier )
C
C*      Initialize grid library common area grdcmn.cmn
C 
	CALL GD_INIT  ( ier )
	done = .false.
C
C*	Loop through program until user exits.
C
	DO WHILE  ( .not. done )
C
C*	    Get input.
C
	    CALL BFRINP  ( snefil, sffsrc, snoutf, sfoutf, snprmf,
     +			   sfprmf, area, dattim, timstn, thndat, iret )
C            print *,' thndat is ',thndat
	    IF  ( iret .ne. 0 )  THEN
		CALL ER_WMSG  ( 'BFR2GP', iret, ' ', ier )
		CALL SS_EXIT
	    END IF
 
            IF((thndat .eq. 'YES') .AND. (icount .eq. 0))  THEN
C
C              Read the list of reports to process  
C
               iunit = 12
               icount = 2000

               DO k = 1,icount
                 READ(iunit,102,end=200) keepers(k,1)
                 READ(iunit,104,end=200) keepers(k,2)
                 READ(iunit,104,end=200) keepers(k,3)
                 keepers(k,4) = '00000000'
 102             FORMAT(a8)  
 104             FORMAT(i8)         
C                 print 103,(keepers(k,i),i=1,3)
C 103             FORMAT(' KEEPERS ',A8,' lat lon', 2i8)
               ENDDO     
 200           icount =  k
C               print *,' total number of keppers is ',k
            ENDIF
	    open = .false.
	    proces = .true.
	    kntsn = 0
	    kntsf = 0
C
C*	    Process SFFSRC input.
C
	    CALL BFRCAT ( sffsrc, 12, srcnam, catprm, ncats, ncatvl,
     +			  catvls, ier )
C
C*	    Get the time bounds in DATTIM.
C
	    IF ( dattim .ne. ' ' ) THEN
		chktim = .true.
		CALL ST_CLST ( dattim, '-', ' ', 2, trange, num, ier )
		IF ( trange (1) .eq. ' ' .or. trange (2) .eq. ' '
     +		     .or. num .ne. 2 .or. ier .ne. 0 ) chktim = .false.
	    ELSE
		chktim = .false.
	    END IF
C
C*	    Get the lat/lon bounds of the data area.
C
	    IF ( INDEX ( area, '@' ) .eq. 0 ) THEN
		IF ( area .eq. ' ' ) THEN
		    grltln (1) = -90.
		    grltln (2) = -180.
		    grltln (3) = 90.
		    grltln (4) = 180.
		ELSE
	            CALL LC_GARE ( area, grltln, cdproj, centrd, ier )
		    IF ( ier .ne. 0 ) THEN
			CALL ER_WMSG ( 'LC', ier, ' ', ier )
			proces = .false.
		    END IF
		END IF
	    ELSE
	        CALL ER_WMSG ( 'BFR2GP', -33, ' ', ier )
		proces = .false.
	    END IF
C
C*	    Open the model sounding file, process the parameter
C*	    files and open the output files.  Also read in the
C*	    conversion factors for the parameters.
C
	    IF ( proces ) THEN
                CALL BFROPN ( snefil, srcnam, snoutf, sfoutf,
     +			      snprmf, sfprmf, timstn, 12, 12,
     +			      lunin, lunsn, lunsf,
     +			      nprmsn, nprmsf, sncfac, sfcfac,
     +			      snctrm, sfctrm, parmsn, parmsf,
     +			      bprmsn, bprmsf, irepsf, nbsn, nbsf,
     +			      istrtf, ship, upaflg, catflg, iret )
		IF ( iret .ne. -5 ) open = .true.
	        IF ( iret .ne. 0 )  THEN
		    CALL ER_WMSG  ( 'BFR2GP', iret, snefil, ier )
		    proces = .false.
	        END IF
	        IF ( iret .eq. 2 ) proces = .true.
	        IF ( lunsn .eq. 0 .and. lunsf .eq. 0 ) THEN
		    proces = .false.
		    CALL ER_WMSG ( 'BFR2GP', -28, ' ', ier )
		END IF
	    END IF
C
C*	    Read the input file; write to the output file.
C
	    dtmlst = ' '
	    sidlst = ' '
	    isnlst = 0
            ireprt = 0
	    DO WHILE ( proces )
		CALL BFRRDD ( lunin,
     +			      catprm, ncats, ncatvl, catvls,
     +			      nprmsn, nprmsf, sncfac, sfcfac,
     +			      snctrm, sfctrm, parmsn, parmsf,
     +			      bprmsn, bprmsf, irepsf, nbsn, nbsf,
     +			      istrtf,
     +			      time, stid, istnm, slat, slon,
     +			      selv, sndata, nz, sfdata, ns, ihhmm,
     +			      iret )
		IF ( iret .eq. -13 ) THEN
		    proces = .false.
		    CALL ER_WMSG ( 'BFR2GP', iret, ' ', ier )
		ELSE IF ( iret .eq. 1 ) THEN
		    proces = .false.
		    CALL ER_WMSG ( 'BFR2GP', iret, ' ', ier )
		END IF
C*
                ireprt = ireprt + 1
C                   print *,' #',ireprt,' id =',stid
C                   print *,' slat ',slat
C                   print *,' slon ',slon
C                   print *,' istn ',istnm
C                   print *,' istr ',istrtf
C               endif
                    
                IF (( iret .eq. 0) .and. (icount .ne. 0)) THEN
                   CALL BFMASK (iflag, stid, keepers, slat,slon, 
     +                icount, ier)
C                  print *,' mask check stid is ',stid
C                  print *,' keep flag ',iflag
                END IF 
                if ( iflag .eq. 1 ) THEN
		   IF ( iret .eq. 0 )
     +		      CALL BFRACH ( grltln, slat, slon, ok, ier )
                
		   IF ( ok .and. chktim ) THEN
		      CALL BFRTCH ( trange, time, ok, ier )
		   END IF
		   IF ( iret .eq. 0 .and. ok ) THEN
C
C*		    Set station and time for writing.
C
	            CALL BFRSST ( lunsn, lunsf, ship, stid, istnm,
     +			          slat, slon, selv, time,
     +				  dtmlst, sidlst, isnlst, ierx )
		    IF ( ierx .eq. 0 .and. lunsn .ne. 0 .and.
     +			 .not. upaflg .and. .not. catflg ) THEN
     			CALL SN_RDAT ( lunsn, nz2, sndat2, ihhmm,
     +     			       ier )
			IF ( ier .eq. 0 ) THEN
			    CALL BFRMRG ( nprmsn, nz2, sndat2, nz,
     +					  sndata, ier )
			END IF
			IF ( ierx .eq. 0 ) THEN
     			    CALL SN_WDAT ( lunsn, ihhmm, nz, sndata,
     +     		  	           ier )
			    IF ( ier .eq. 0 ) kntsn = kntsn + 1
			    IF ( ier .ne. 0 ) THEN
			        CALL ER_WMSG ( 'SN', ier, ' ', ierr )
			        IF ( kntsn + 1 .eq. MMHDRS ) THEN
				    proces = .false.
				    WRITE (6,*) ' TOO MANY STATIONS.'
			        END IF
			    END IF
			END IF
		    END IF
C*
		    IF ( ierx .eq. 0 .and. lunsn .ne. 0 .and.
     +			 upaflg ) THEN
			CALL BFRWRU ( lunsn, ihhmm, nprmsn, nz, sndata,
     +				      ier )
			IF ( ier .eq. 0 ) kntsn = kntsn + 1
			IF ( ier .ne. 0 ) THEN 
			    CALL ER_WMSG ( 'BFR2GP', ier, ' ', ierr )
			    IF ( kntsn + 1 .eq. MMHDRS ) THEN
				proces = .false.
				WRITE (6,*) ' TOO MANY STATIONS.'
			    END IF
			END IF
		    ELSE IF ( ierx .eq. 0 .and. lunsn .ne. 0 .and.
     +			      catflg ) THEN
			CALL BFRWR2 ( lunsn, ihhmm, nprmsn, nz, sndata,
     +				      ier )
			IF ( ier .eq. 0 ) kntsn = kntsn + 1
			IF ( ier .ne. 0 ) THEN 
			    CALL ER_WMSG ( 'BFR2GP', ier, ' ', ierr )
			    IF ( kntsn + 1 .eq. MMHDRS ) THEN
				proces = .false.
				WRITE (6,*) ' TOO MANY STATIONS.'
			    END IF
			END IF
		    END IF
C*
		    IF ( ierx .eq. 0 .and. lunsf .ne. 0 .and.
     +			 .not. ship ) THEN
			CALL SF_RDAT ( lunsf, sfcbuf, ihm, ier )
			IF ( ier .eq. 0 ) THEN
			    nnprm = nprmsf - istrtf + 1
			    DO i = 1, nnprm
				IF ( ERMISS ( sfdata (i) ) )
     +				  sfdata (i) = sfcbuf (i)
			    END DO
			END IF
			CALL SF_WDAT ( lunsf, ihhmm, sfdata, ier )
			IF ( ier .eq. 0 ) kntsf = kntsf + 1
		        IF ( ier .ne. 0 ) THEN
			    CALL ER_WMSG ( 'SF', ier, ' ', ierr )
			    IF ( kntsf + 1 .eq. MMHDRS .and.
     +				 ier .eq. -12 ) THEN
				proces = .false.
				WRITE (6,*) ' TOO MANY STATIONS.'
			    END IF
			END IF
		    END IF
C*
		    IF ( ierx .eq. 0 .and. lunsf .ne. 0 .and.
     +			 ship ) THEN
			stat = ' '
			coun = ' '
			CALL SF_WSDD ( lunsf, time, stid, istnm,
     +				       slat, slon, selv, stat, coun,
     +				       ihhmm, sfdata, ier )
			IF ( ier .eq. 0 ) kntsf = kntsf + 1
		        IF ( ier .ne. 0 ) THEN 
			    CALL ER_WMSG ( 'SF', ier, ' ', ierr )
			    IF ( kntsf + 1 .eq. MMHDRS .and.
     +				 ier .eq. -12 ) THEN
				proces = .false.
				WRITE (6,*) ' TOO MANY STATIONS.'
			    END IF
			END IF
		    END IF
               END IF
C*
             END IF
	    END DO
	    WRITE (6,*) ' Number of single level BUFR messages used = ',
     +			kntsf
	    WRITE (6,*) ' Number of profile type BUFR messages used = ',
     +			kntsn
            print *,' total number of reports is ',ireprt
C
C*	    Close files.
C
	    IF ( open ) CALL JB_CLOS  ( lunin, ier )
	    CALL SN_CLOS  ( lunsn, ier )
	    CALL SF_CLOS  ( lunsf, ier )
C
C*	    Call dynamic tutor.
C
	    CALL IP_DYNM  ( done, ier )
	END DO
C
C*	Exit.
C
	CALL IP_EXIT  ( ier )
C*
	END		
