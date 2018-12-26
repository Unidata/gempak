        SUBROUTINE GG_WACT ( inumb, dattim, systim, type, strtim,
     +        		     stptim, ncnty, cnties, wfocty, knt, 
     +			     etmchg, itest, sacod, numarr,ircntm, iret) 
C************************************************************************
C* GG_WACT        							*
C*        								*
C* This subroutine creates an array of active counties for each active  *
C* watch number.        						*
C*        								*
C* GG_WACT ( INUMB, DATTIM, SYSTIM, WTYPE, STRTIM, STPTIM, NCNTY,       *
C*           CNTIES, WFOCTY, KNT, ETMCHG, ITEST, SACOD, NUMARR, 	*
C*	     IRCNTM, IRET ) 						*
C*        								*
C* Input parameters:        						*
C*        INUMB		INTEGER		Current watch number 		*
C*        DATTIM 	CHAR*		User specified time(YYMMDD/HHMM)*
C*        SYSTIM	CHAR*		System time or ' ' to use dattim*
C*        								*
C* Output parameters:        						*
C*        WTYPE		CHAR* 		Watch type			*
C*        STRTIM 	CHAR*		Watch start time - GEMPAK format*
C*        STPTIM 	CHAR*		Watch end time - GEMPAK format  *
C*        NCNTY 	INTEGER		Number of counties in CNTIES	*
C*      CNTIES (NCNTY)  CHAR*        	Active county array (WCN + WOU)	*
C*        WFOCTY (KNT)  CHAR*           Most recent active WOU counties *
C*        KNT		INTEGER		Number of WOU counties in WFOCTY*
C*	  ETMCHG	LOGICAL	 	Flag for ending time change	*
C*        ITEST		INTEGER		Test flag			*
C*					  0,1 - not a test		*
C*					  2,3 - a test			*
C*	  SACOD(NUMARR) CHAR*		VTEC action codes		*
C*	  NUMARR 	INTEGER		Number of action codes		*
C*        IRCNTM 	CHAR*		Most recent start time 		*
C*        IRET		INTEGER		Return code			*
C*					 8 = Dim of cnties/wfocty exceed*
C*					 7 = Dim. of sacod exceeded	*	
C*					 2 = User time=end of watch time*
C*					 0 = Normal return		*
C*					-1 = No active counties		*
C*				        -3 = Bad input string in TI_STAN*
C*        								*
C**        								*
C* Log:        								*
C* A. Hardy/NCEP         2/03						*
C* A. Hardy/NCEP         3/03	Added time extension, cleaned up	*
C* A. Hardy/NCEP         3/03	Compare issue, start, stop and dattim 	*
C*                              for locating reports			*
C* A. Hardy/NCEP         4/03   Fixed watch number check		*
C* A. Hardy/NCEP         4/03   Moved array counter knt2; increased WOU *
C*                              search mins       			*
C* A. Hardy/NCEP	 7/03	Return WOU cnties, end time chg 	*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM     	*
C* A. Hardy/NCEP	 5/04	Check for single canceled WCNs		*
C* A. Hardy/NCEP	 8/04	Added itest to call; changed carr(10)	*
C*				to (11)					*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* A. Hardy/NCEP	03/05	Added return of action code array	*
C* A. Hardy/NCEP	03/05	Add ircntm to call; reworked finding	*
C*				time extensions;most current WOU cnties	*
C* F. J. Yen/NCEP	08/05	Fixed saving latest WOU cnties; CNL from*
C*				only WOUs (missing from WCNs); & cleanup*
C* F. J. Yen/NCEP	11/05	Fixed erroneous WOU CON issued after WOU*
C*				was cancelled; reinitial over and ircntm*
C* F. J. Yen/NCEP	12/05	Fixed WCN CAN counties still in WOU and *
C*				status (checked for matching sorig);#851*
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* F. J. Yen/NCEP	 3/06	Remove over & use only extend(#58 & 78);*
C*				Clarify definitions for dattim & systim	*
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP	 7/06	Refined WCN start time difference test	*
C*				to fix premature cancellation of #458.	*
C* F. J. Yen/NCEP	 1/07	Drop EXP cnties w/EXT.Fix:gdtim;bad time*
C*				w/EXT;removal of cnties;minuts for 12 hr*
C* F. J. Yen/NCEP	 3/07	Fixed watch status process bug for #44 &*
C* 				#46 by removing dup cnties. Checked size*
C*				of sacod.Replaced NC &LLSTFL w/ MAX_CNTY*
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* F. J. Yen/NCEP	 7/08	Look back 2 days instead of 1 day	*
C* S. Guan/NCEP          1/18   Hard code for LEZ061 and CLE            *
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
C*
        PARAMETER	( NW = 1000 )
C*
        CHARACTER*(*)	dattim, systim, type, strtim, stptim,
     +        		cnties(*), wfocty(*), sacod(*), ircntm
C*
        CHARACTER	srchtm*12
	LOGICAL		etmchg
C* ---------------------------------------------------------------
        CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +        		buffer*128, tfile*128, dattm4*20, carr(11)*128,
     +                  stime*20, flstrt*160, flend*160 
        CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
        CHARACTER	wfoid*128, wfoarr(20)*4, wstn*4,
     +  		wouiss*11, wcnumb*4, arlin(NW)*128, find(20)*4,
     +  	        sissue(20)*11, swnum(20)*4, sorig(20)*4,
     +                  extend*11
	CHARACTER	actncd*3, erlstp*11, latstp*11
C 
        INTEGER		arnum(NW), itarr(5), jtarr(5), inarr, 
     +                  knt2, itype
        SAVE            sissue, swnum, sorig, inarr
        LOGICAL		done, found, gdtim, usecd, dup
C
        DATA            sissue / 20*' ' /, swnum/ 20*' ' /,
     +                  sorig  / 20*' ' /
C-----------------------------------------------------------------------
       iret   = 0
       knt2   = 0
       icnt   = 0 
       inarr  = 0
       itest  = 0
       type   = ' '
       strtim = ' '
       stptim = ' '
       ircntm = ' '
       extend = ' '
       erlstp = ' '
       latstp = ' '
       usecd  = .True.
       found  = .false.
       etmchg = .false.
C
       CALL ST_LSTR ( dattim, lens, ier )
C
C*     See if the system time or the user time will be used as the
C*     current end time.
C
       IF ( systim.eq. ' ' ) THEN
           srchtm = dattim
         ELSE
           srchtm = systim
       END IF
C
C*     Search WOUs.
C
C*     Loop over watch number to find the latest reports.
C
C*     Search WOU directory for latest report for watch number.
C
	filnam = 'WOU'
	path   = ' '
	templ  = ' '
	CALL ST_NULL ( filnam, filnam, nf, ier )
	CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii, ion,
     +          ihb, mnb, iha, mna, mstrct, idtmch, ier )
       CALL ST_RNUL ( path, path, lens, ier )
       CALL ST_RNUL ( templ, templ, lens, ier )
       CALL ST_LSTR ( path, lenp, ier )
       nexp   = MXNMFL
       iorder = -1
       CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*     Check for the last file requested by the user.
C
       CALL ST_LCUC ( dattim, dattim, ier )
       itype = 1
       IF  ( ( dattim .eq. 'LAST' ) .or. ( dattim .eq. 'ALL' ) )  THEN
           CALL CSS_GTIM ( itype, dattm2, ier )
           CALL FL_MNAM ( dattm2, templ, filnam, ier )
         ELSE
           CALL CSS_GTIM ( itype, cdttm, ier )
           CALL TI_STAN ( dattim, cdttm, dattm2, ier )
           IF ( ier .ne. 0 ) THEN
               CALL ER_WMSG ( 'TI', ier, dattim, ierr )
               iret = -3
               RETURN
           END IF	    
           CALL FL_MNAM ( dattm2, templ, filnam, ier )
       END IF
C
C*     Find the earliest file to start searching. For ALL times
C*     go back 10 days, for any other entry for dattim subtract
C*     2 days from the time given.
C
       IF  ( dattim .eq. 'ALL' )  THEN
           minuts = 14400
         ELSE
           minuts = 2880
       END IF
       CALL TI_CTOI ( dattm2, itarr, ier )
       CALL TI_SUBM ( itarr, minuts, jtarr, ier )
       CALL TI_ITOC ( jtarr, stime, ier )
       CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*     Get 4-digit year to compare dates.
C
       CALL TI_DTM4 ( dattm2, dattm4, ier )
C
       DO iii = 1, MAX_CNTY
          wfocty(iii) = ' '
       END DO 
C
C*     Decode each file until the end time is reached.
C
       nwtch = 0
       ifl = 1
       knt = 0
C
       DO WHILE  ( ifl .le. nfile .and. .not. found )
           IF  ( files(ifl) .le. filnam )  THEN
               IF  ( files(ifl) .ge. flstrt )  THEN
        	   tfile = path(:lenp) // '/' // files(ifl)
        	   CALL FL_SOPN ( tfile, lunf, ier )
C
        	   iostat = 0
        	   DO WHILE  ( iostat .eq. 0 )
        	       READ ( lunf, 2, IOSTAT = iostat ) buffer
2        	       FORMAT ( A )
        	       IF  ( iostat .eq. 0 )  THEN
        		   IF  ( buffer(1:1) .eq. '|' )  THEN
        		       CALL ST_CLST ( buffer, '|', ' ', 11,
     +        				   carr, ignore, ier )
C
        		       CALL ST_NUMB ( carr(10), jflag, ier )
        		       jcorr = MOD ( jflag, 2 )
        		       jtest = jflag 
C
                               CALL TI_DIFF ( dattim, carr(4),
     +                               nmstr, ier )
                               CALL ST_NUMB ( carr(6), icnmb, ier )
                               IF ( ( icnmb .eq. inumb ) .and.
     +				    (nmstr .ge. 0 ) )THEN
C
C*                                 Save watch type and start time. 
C*                                 Store in arrays WFO ids and end times. 
C*                                 Save number of WFO ids.
C
                                   found = .true.
        		           type  = carr(2)
        		           wouiss = carr(3)(:11)
        		           strtim = carr(4)
                                   wfoid  = carr(8)
        		           itest  = jtest
			           CALL ST_NUMB ( carr(11), jcncl, ier )
C
C*                                 Get the most recent ending time.
C
                                   CALL TI_DIFF ( carr(5), stptim,
     +                               nmstr, ier )
                                   IF (nmstr .ge. 0 ) THEN
         		               stptim = carr(5)
                                   END IF
                                   CALL ST_CLST ( wfoid, ';',' ',
     +                                          20, wfoarr, nwfo, ier )
C
C*                                 Get the most recent starting time.
C*                                 This is used for the unique option.
C
                                   CALL TI_DIFF ( carr(4), ircntm,
     +                                            nmstr, ier )
                                   IF ( nmstr .ge. 0 ) THEN
                                       ircntm = carr(4)
                                   END IF
C
         		           jostat = 0
       			           knt = 0
         		           DO WHILE  ( jostat .eq. 0 )
         		             READ ( lunf, 2, IOSTAT = jostat )
     +						buffer
         		             IF  ( jostat .eq. 0 )  THEN
         			       IF  ( buffer(1:1) .eq. '|' ) THEN
         			         CALL FL_BKSP ( lunf, ier )
         			         jostat = -1
         			        ELSE
          			         IF ( knt .lt. MAX_CNTY )  THEN
         			           knt = knt + 1
                                           wfocty(knt) = buffer
					  ELSE
					   iret = 8
         			         END IF
         			       END IF
         		             END IF
         		           END DO
			       END IF
                           END IF
        	       END IF
        	   END DO
C
                   CALL FL_CLOS ( lunf, ier )
C
               END IF
C
           END IF
           ifl = ifl + 1
       END DO
C
C*     Begin searching the WCN files for active counties.
C*     unless there was a WOU cancel.
C
       IF ( found .and. (jcncl .ne. 1) ) THEN
           CALL ST_NUMB ( swnum(1), iswnm, ier)
C
C*	   Initialize save arrays if new watch number.
C
           IF ( iswnm .ne. inumb ) THEN
               inarr = 0
               DO ik = 1, 20
                   sissue (ik) = ' '
                   swnum (ik)  = ' '
                   sorig (ik)  = ' '
               END DO
           END IF
C
C*         Search WCN directory for latest reports for watch number.
C
C*	   Scan the WCN directory for all the WCN data files.
C
	    filnam = 'WCN'
	    CALL ST_NULL ( filnam, filnam, nf, ier )
	    CALL CTB_DTGET ( filnam, path, templ, ic, is, if, ir, ii,
     +			ion, ihb, mnb, iha, mna, mstrct, idtmch, ier )
            CALL ST_RNUL ( path, path, lens, ier )
            CALL ST_RNUL ( templ, templ, lens, ier )
	    CALL ST_LSTR ( path, lenp, ier )
            nexp   = MXNMFL
	    iorder = -1
	    CALL FL_SCND (path, templ, iorder, nexp, files, nfile, ier)
C
C*	    Check for the last file requested by the user.
C
	    CALL ST_LCUC ( dattim, dattim, ier )
	    itype = 1 
	    IF  ( ( dattim .eq. 'LAST' ) .or.
     +	          ( dattim .eq. 'ALL' ) )  THEN
	        CALL CSS_GTIM ( itype, dattm2, ier )
	        CALL FL_MNAM ( dattm2, templ, filnam, ier )
	      ELSE
	        CALL CSS_GTIM ( itype, cdttm, ier )
	        CALL TI_STAN ( dattim, cdttm, dattm2, ier )
	        IF ( ier .ne. 0 ) THEN
	        	CALL ER_WMSG ( 'TI', ier, dattim, ierr )
	        	iret = -3
	        	RETURN
	        END IF	    
	        CALL FL_MNAM ( dattm2, templ, filnam, ier )
	    END IF
C
C*	    Find the earliest file to start searching. For ALL times
C*	    go back 10 days, for any other entry for dattim subtract
C*	    12 hours from the time given.
C
	    IF  ( dattim .eq. 'ALL' )  THEN
	        minuts = 14400
	      ELSE
	        minuts = 720
	    END IF
C
C*	    Find end file name
C
	    CALL TI_CTOI ( dattm2, itarr, ier )
	    CALL TI_ITOC ( itarr, stime, ier )
	    CALL FL_MNAM ( stime, templ, flend, ier )
            CALL ST_LSTR ( flend, len, ier )
C
C*	    Find start file name
C
	    CALL TI_CTOI ( dattm2, itarr, ier )
	    CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	    CALL TI_ITOC ( jtarr, stime, ier )
	    CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*	    Get 4-digit year to compare dates.
C
	    CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*	    Decode each file until the end time is reached.
C
	    nwtch = 0
            jj = 1
	    done  = .false.
	    ifl = 1
            icd = 1
	    DO WHILE  ( ( ifl .le. nfile) .and. ( .not. done ) )
	        IF  ( ifl .gt. nfile )  THEN
 		    done = .true.
	          ELSE
		    IF  ( ( files(ifl) .ge. flstrt ) .and.
     +                    ( files(ifl) .le. flend ) ) THEN
		      tfile = path(:lenp) // '/' // files(ifl)
		      CALL FL_SOPN ( tfile, lunf, ier )
C
C*                    Find header '|' lines  and store their locations 
C*                    and information lines.
C
		      iostat = 0
                      iln = 0
                      ii = 1
                      icnt = 0
		      DO WHILE  ( iostat .eq. 0 )
		        READ ( lunf, 2, IOSTAT = iostat ) buffer
		        IF  ( iostat .eq. 0 )  THEN
			    IF  ( buffer(1:1) .eq. '|' )  THEN
                               iln = iln + 1
                               arnum(iln) = ii - icnt
                               arlin(iln) = buffer
                               ii = ii+1
                               icnt = icnt + 1
C
C*                             Read through county lines looking for 
C*			       next '|'.
C
			       jostat = 0
			       DO WHILE  ( jostat .eq. 0 )
			           READ ( lunf, 2, IOSTAT = jostat ) 
     +                                                           buffer
			           IF  ( jostat .eq. 0 )  THEN
				       IF  ( buffer(1:1) .eq. '|') THEN
				           CALL FL_BKSP ( lunf, ier )
				           jostat = -1
				       END IF
			           END IF
                                   ii = ii+1
			        END DO
			    END IF
		        END IF
		      END DO
C
C*                    Start from bottom of file and find last '|' line
C*                    and set buffer to first county line.
C*
C*                    inlf = number of lines in data file.
C*                    inf  = number of lines of info. '|'
C
                      inlf = ii - iln - 1
                      inf = iln
                      itot = inlf
                      DO kk =  inf, 1, -1
                          gdtim = .false.
                          itot = inlf
                          ii = arnum(kk)
                          DO WHILE ( itot .ge. ii )
                              CALL FL_BKSP ( lunf, ier )
                              itot = itot - 1
                          END DO
                          READ ( lunf, 2, IOSTAT = jostat ) buffer
C
C*                    Check dattim vs. issue start, stop time differences.
C
                      CALL TI_DIFF ( dattim, arlin(kk)(6:16),
     +                               nmis, ier )
                      CALL TI_DIFF ( dattim, arlin(kk)(18:28),
     +                               nmst, ier )
                      CALL TI_DIFF ( dattim, arlin(kk)(30:40),
     +                               nmsp, ier )
                      IF ( nmis .ge. 0 ) THEN  
C
C*		          Get the WCN VTEC action code which follows
C*			  the ';' separator and store it in actncd.
C
                          CALL ST_NUMB ( arlin(kk)(42:45), iarlin, ier )
                          actncd = ' '
                          IF ( iarlin .eq. inumb ) THEN
                             CALL ST_NOCC (arlin(kk), ';', 1, npos, ier)
                             IF ( npos .gt. 0 ) THEN
                                 actncd = arlin(kk)(npos-3:npos-1)
                              END IF
			  END IF
C
C*			  If the action code is not 'NEW' or not 'CAN',
C*			  the WCN should take effect immediately at
C*			  issue time.  Rejecting these 'bad' start times
C*			  (start times that are after the issue times) 
C*			  can cause premature cancellations of watches.
C*			  So, testing for this, overrides bad start times.
C*
C*			  (Note that the WCN decoder modifies the start
C*			  times of 'CAN' reports from 000000T0000Z to the
C*			  end times.  So, these reports are handled in the
C*			  "ELSE" section of the "IF" test.)
C
                          IF ( nmst .ge. 0 .or.
     +				   .not.( actncd .eq. 'NEW' .or.
     +				          actncd .eq. 'CAN' .or.
     +				          actncd .eq. ' ' ) ) THEN
			      IF ( nmsp .le. 0 ) THEN
                                  gdtim = .true.
                                ELSE 
                                  CALL TI_DIFF ( arlin(kk)(18:28),
     +                                 arlin(kk)(30:40), nm, ier )

                                  IF ( nm .eq. 0 ) THEN
                                      gdtim = .true.
                                    ELSE
				      IF ( extend .ne. ' ') THEN
					  CALL TI_DIFF (srchtm, extend,
     +					          nxtn, ier)
					  IF (nxtn .le. 0) THEN
					      gdtim = .true.
					    ELSE
					      gdtim = .false.
					  END IF
					ELSE
C
C*					  It is possible that some
C*					  EXPs are late and an
C*					  extension in time has
C*					  not been found yet
C
					  gdtim = .true.
				      END IF
                                  END IF
                              END IF
                            ELSE 
                              CALL TI_DIFF ( arlin(kk)(18:28),
     +                                 arlin(kk)(30:40), nm, ier )
                              IF ( nm .eq. 0 ) THEN
                                  gdtim = .true.
                                ELSE
                                  gdtim = .false.
                              END IF
                          END IF
                        ELSE 
                          gdtim = .false.
                      END IF
C
                      CALL ST_NUMB ( arlin(kk)(42:45), iarlin, ier )
                      IF ( ( iarlin .eq. inumb ) .and.
     +			    ( gdtim ) ) THEN
C
C*                        Check if the data info line has an action code
C*                        by searching for the ';' separator.
C
                          CALL ST_NOCC (arlin(kk), ';', 1, npos, ier)
                          IF ( npos .lt. 1 ) THEN
                              usecd = .False.
                              ipos = 0
                            ELSE
                              usecd = .True.
                              ipos = 4
                          END IF
C  
C*			  erlstp is the earliest stop time.
C*			  latstp is the latest stop time.  If it is
C*			  not equal to erlstp, then it is an actual
C*			  extended time.)
C  
			  IF ( erlstp .eq. ' ') THEN
			      erlstp = arlin(kk) (30:40)
	       		      latstp = erlstp		 
			    ELSE
			      CALL TI_DIFF ( arlin(kk) (30:40),
     +					erlstp, nerdif, ier2 )
			      IF ( nerdif .lt. 0 ) THEN
				  erlstp = arlin(kk) (30:40)
				ELSE
				  CALL TI_DIFF (arlin(kk) (30:40),
     +					latstp, latdif, ier2 )
				  IF ( latdif .gt. 0 ) THEN
				      latstp = arlin(kk) (30:40)
				  END IF
			      END IF
			  END IF
                          IF ( inarr .eq. 0 ) THEN
                              match = 0
                              sissue (1) = arlin(kk)(6:16)
                              swnum (1)  = arlin(kk)(42:45)
                              sorig (1)  = arlin(kk)(47:50)
C
C*                            Check if VTEC action code is decoded.
C
                              IF ( usecd ) THEN
                                   sacod(1) = arlin(kk)(npos-3:npos-1)
                                 ELSE
                                   sacod(1) = ' '
                              END IF
			      inarr = inarr + 1
                              CALL TI_DIFF ( dattim, 
     +					arlin(kk)(30:40), ndiff, ier )
                              IF (ndiff .lt. 0) THEN
                                  extend = arlin(kk)(30:40)
                              END IF
                            ELSE 
                              ij = 1
                              match = 0
                              DO WHILE ( ij .le. inarr ) 
                                  IF ( arlin(kk)(42:45) .eq. swnum(ij) ) 
     +								THEN
                                      IF ( arlin(kk)(47:50) .eq. 
     +						   sorig (ij) ) THEN
                                          match = 1
                                      END IF 
                                      IF ( (arlin(kk)(6:16) .gt. 
     +					          sissue(ij) ) .and.
     +					    ( arlin(kk)(47:50) .eq.
     +					      sorig (ij) ) ) THEN
                                          match = 0
                                        ELSE IF ( ( arlin(kk)(6:16) .eq. 
     +					        sissue(ij) ) .and.
     +                                        ( arlin(kk)(47:50) .eq. 
     +                                          sorig (ij) ) )  THEN
                                          match = 2
                                      END IF 
                                  END IF 
                                  ij = ij + 1
                              END DO
			     IF ( actncd .eq. 'EXP' ) THEN

                              CALL TI_DIFF ( dattim, 
     +					arlin(kk)(30:40), ndiff, ier)
                              IF ( ndiff .lt. 0 ) THEN
                                   CALL TI_DIFF ( extend, 
     +				      arlin(kk)(30:40), ndiff1, ier )
                                   IF ( ndiff1 .lt. 0 ) THEN
                                       extend = arlin(kk)(30:40)
                                   END IF
                              END IF
			     END IF
C
                              IF ( match .eq. 0 ) THEN
                                  inarr = inarr + 1
                                  sissue (inarr) = arlin(kk)(6:16)
                                  swnum (inarr)  = arlin(kk)(42:45)
                                  sorig (inarr)  = arlin(kk)(47:50)
C
C*                                Check for end time extention.
C
                                  CALL TI_DIFF ( dattim, 
     +					arlin(kk)(30:40), ndiff, ier)
                                  IF ( ndiff .lt. 0 ) THEN
                                      CALL TI_DIFF ( extend, 
     +					arlin(kk)(30:40), ndiff1, ier )
C
C*                                    Make sure not to over write with 
C*				      an old end time.
C
                                      IF ( ndiff1 .le. 0 ) THEN
                                          extend = arlin(kk)(30:40)
                                      END IF
                                  END IF
                              END IF
C
                            IF ( ( ( match .eq. 0 ) .or.
     +                                       ( match .eq. 2 ) ) .and. 
     +                                     (ndiff .lt. 0 ) ) THEN
				IF ( (stptim .gt. dattim ) .or.
     +				     (extend .ne. ' ') ) THEN
    				  IF ( icd .lt. 100 ) THEN
                                      icd = icd + 1
                                      IF ( usecd ) THEN
                                        sacod(icd) = 
     +                                       arlin(kk)(npos-3:npos-1)
                                       ELSE
                                        sacod(icd) = ' '
                                      END IF
				    ELSE
				      iret = 7
				  END IF
                                END IF
                              END IF
                          END IF
C
C*                        Store active county lines.
C
		          jostat = 0
		          DO WHILE  ( jostat .eq. 0 )
			      IF  ( jostat .eq. 0 )  THEN
			          IF  ( buffer(1:1) .eq. '|' )  THEN
				      CALL FL_BKSP ( lunf, ier )
				      jostat = -1
			            ELSE
                                      IF ( ( ( match .eq. 0 ) .or.
     +                                       ( match .eq. 2 ) ) .and. 
     +                             (arlin(kk)(56+ipos:56+ipos) .eq.'0')
     +					) THEN
					  IF ( actncd .ne. 'EXP' ) THEN
                                            IF ((stptim .gt. dattim).or.
     +                                          (extend .ne. ' ') ) THEN
C*
C*					      Check for duplicate
C*					      counties using FIPs code
C*
					      dup = .false.
					      k = 1
					      DO WHILE ( .not. dup .and.
     +						      k .le. knt2 )
					 	IF (cnties(k)(10:15).eq.
     +						      buffer(10:15)) 
     +						         dup = .true.
					  	k = k + 1
					      END DO
					      IF (.not. dup) THEN
  					        IF (knt2 .lt. MAX_CNTY)
     +							THEN
				                  knt2 = knt2 + 1
                                                  cnties(knt2) = buffer
						 ELSE
						  iret = 8
						END IF
					      END IF
					    END IF
					   ELSE
C*
C*  					    For EXP, compare the
C*					    expiration time with
C*					    srchtm to determine if
C*					    should add counties to
C*					    the county list.  If orig
C*					    expiration time has
C*					    already passed, then drop
C*					    the counties.
C
					    CALL TI_DIFF ( srchtm,
     +						arlin(kk)(30:40),
     +						ndiff3, ier1 )
					    IF ( ndiff3 .lt. 0 ) THEN
C*
C*					      Check for duplicate
C*					      counties using FIPs code
C*
					      dup = .false.
					      k = 1
					      DO WHILE ( .not. dup .and.
     +						      k .le. knt2 )
					 	IF (cnties(k)(10:15).eq.
     +						      buffer(10:15)) 
     +						         dup = .true.
					  	k = k + 1
					      END DO
       					      IF (.not. dup) THEN
  					        IF (knt2 .lt. MAX_CNTY)
     +							THEN
				                  knt2 = knt2 + 1
                                                  cnties(knt2) = buffer
						 ELSE
						  iret = 8
						END IF
       					      END IF
					    END IF
					  END IF
                                      END IF
			          END IF
			      END IF
			      READ ( lunf, 2, IOSTAT = jostat ) buffer
		              IF  ( buffer(1:1) .eq. '|' )  THEN
C
C*                                Read last county: read to end of file. 
C
		                  DO WHILE  ( jostat .eq. 0 )
     			              READ ( lunf, 2, IOSTAT = jostat ) 
     +                                                          buffer
                                  END DO
     			          READ (lunf, 2, IOSTAT = jostat) buffer
                              END IF
		          END DO
                        ELSE
                          jostat = 0
		          DO WHILE  ( jostat .eq. 0 )
			      IF  ( jostat .eq. 0 )  THEN
			          IF  ( buffer(1:1) .eq. '|' )  THEN
				      CALL FL_BKSP ( lunf, ier )
				      jostat = -1
			          END IF
			      END IF
			      READ ( lunf, 2, IOSTAT = jostat ) buffer
		              IF  ( buffer(1:1) .eq. '|' )  THEN
C
C*                                Read last county: read to end of file. 
C
		                  DO WHILE  ( jostat .eq. 0 )
     			              READ ( lunf, 2, IOSTAT = jostat ) 
     +                                                          buffer
                                  END DO
     			          READ (lunf, 2, IOSTAT = jostat) buffer
                              END IF
		          END DO
                       END IF 
                      END DO
C
C*                    Close file.
C
		      CALL FL_CLOS ( lunf, ier )
C
		    END IF
	        END IF
	        ifl = ifl + 1
	    END DO
C 
C*	    If srchtm has passed latest stop time, then	there should
C*	    be no more counties in the watch.  This test is necessary
C*	    since now allowing stop times that have passed (less than
C*	    srchtm) to have gdtim set to true when time has been extended).
C*	    If it has been extended then latstp would be the extended time.
C
	    CALL TI_DIFF ( srchtm, latstp, ndiff3, ier1 )
	    IF ( ier1 .eq. 0 ) THEN
		IF ( ndiff3 .ge. 0 ) THEN
		    ncnty = 0
		    iret = -1
		    RETURN
		END IF
	    END IF
C
            ncnty = knt2 
            numarr = icd
C
C*          Compare WOU WFO array to WCN WFO array. Check for missing
C*          WCN WFOs.
C
	    CALL ST_LSTR( srchtm, lenr, ier)
	    CALL ST_LSTR( stptim, lenp, ier)
C
            IF (srchtm(:lenr) .lt. stptim(:lenp) )THEN
                ifnd = 0
                DO ii = 1, nwfo
                    ipard = 0
                    DO jj = 1, inarr
                        IF ( wfoarr(ii) .eq. sorig(jj)(2:) ) THEN
                            ipard = 1
                        END IF 
                    END DO
                    IF ( ipard .eq. 0 ) THEN
                        ifnd = ifnd + 1
                        find (ifnd) = wfoarr(ii)
                    END IF
                END DO
C
C*              Loop over unreported WCN WFO stations to retrieve county 
C*	        information from the current WOU.
C
                IF ( ifnd .gt. 0 ) THEN
                    DO il = 1, ifnd
                        wstn = find(il)
                        DO iii = 1, knt
                            IF ( wstn .eq. "CLE" .and. wfocty(iii)(1:6)
     +                         .eq. "LEZ061") THEN
C 
C*                          BUF ONLY has CONVECTIVE WATCH responsibility for
C*                          LEZ061, and all other responsibilities are handled
C*                          by WFO CLE.
C
                            ELSE IF ( wstn .eq. wfocty(iii)(90:92)) THEN 
				IF ( ncnty .lt. MAX_CNTY ) THEN
                                    ncnty = ncnty + 1
                                    cnties(ncnty) = wfocty(iii)
				  ELSE
				    iret = 8
				END IF
                            END IF 
                        END DO
                    END DO
                END IF
C
C*              Print message if user time is equal to end of watch time.
C*              If the end time has been extended, reset the stop time.
C
                IF ( (extend .eq. ' ') .and. (stptim .lt. dattim) ) THEN
                    iret = 2
                    CALL ST_INCH ( inumb, wcnumb, ier )
                    CALL ER_WMSG  ( 'WOUPDT', iret, wcnumb, ierr )
                  ELSE IF ((extend .ne. ' ') .and.
     +				(stptim .lt. extend ) ) THEN
                    stptim = extend
                    etmchg = .true.
                END IF
            END IF
            IF ((extend .ne. ' ') .and. (stptim .lt. extend ) ) THEN
                stptim = extend
                etmchg = .true.
            END IF
          ELSE
C
C*          No active WOU found for that watch number.
C
            ncnty = 0
            iret = -1
        END IF
C*        
        RETURN
        END
