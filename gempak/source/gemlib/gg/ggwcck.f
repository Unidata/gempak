        SUBROUTINE GG_WCCK ( inumb, dattim, systim, irmzn, iznflg, 
     +			     icancl, attnln, stzstr, iret )
C************************************************************************
C* GG_WCCK                                                              *
C*                                                                      *
C* This subroutine checks if a canceled WOU exists for a watch and      *
C* returns the original 'ATTN...WFO' string.                            *
C*                                                                      *
C* GG_WCCK (INUMB, DATTIM, SYSTIM, IZNFLG, ICANCL, ATTNLN, STZSTR, IRET)*
C*                                                                      *
C* Input parameters:                                                    *
C*        INUMB         INTEGER         Current watch number            *
C*        DATTIM        CHAR*           User specified time             *
C*        SYSTIM        CHAR*           System time                     *
C*        IRMZN		INTEGER		Flag for removing marine zone id*
C*                                        0 - do not remove		*
C*                                        1 - remove			*
C*                                                                      *
C* Output parameters:                                                   *
C*	  IZNFLG	INTEGER		Marine zone flag		*
C*					  0 - no marine zones in watch  *
C*					  1 - marine zones in watch	*
C*        ICANCL        INTEGER         Flag for cancel message		*
C*					  0 - No cancel msg., ret. WFOs *
C*					  1 - Found cancel msg.		*
C*        ATTNLN        CHAR*           Original WFO attention string   *
C*        STZSTR        CHAR*           Original WFOs state zone string *
C*        IRET          INTEGER         Return code                     *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/NCEP         7/03                                           *
C* A. Hardy/NCEP         1/04	Added original states string		*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM     	*
C* A. Hardy/NCEP         3/04	Added marine flag iznflg		*
C* T. Lee/SAIC		 9/04	Replaced FL_TMPL with CTB_DTGET		*
C* A. Hardy/NCEP	11/04	Added calls to ST_RNUL			*
C* m.gamazaychikov/SAIC 12/04   Added ion flag to CTB_DTGET CS          *
C* A. Hardy/NCEP	 3/05	Added irmzn to calling sequence		*
C* T. Piper/SAIC	 1/06	Initialize wfoid, ST_NULL attnln	*
C* m.gamazaychikov/SAIC 01/06   Changed templ string length to MXTMPL   *
C* m.gamazaychikov/SAIC 04/06   Added idtmch flag to CTB_DTGET CS       *
C* F. J. Yen/NCEP        4/08   Added bin mins & mstrct to CTB_DTGET CSC*
C* F. J. Yen/NCEP	 7/08	Increase look back from 12 hrs to 48 hrs*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
C*
        PARAMETER	( NC = 1000 )
C*
        CHARACTER*(*)	dattim, systim, attnln, stzstr
C*
        CHARACTER	srchtim*12
C* ---------------------------------------------------------------
        CHARACTER	path*25, templ*(MXTMPL), cdttm*20, dattm2*20, 
     +        		buffer*128, tfile*128, dattm4*20, carr(11)*128,
     +                  stime*20, flstrt*160
        CHARACTER*(MXFLSZ)      filnam, files (MXNMFL)
C*
        CHARACTER	wfoid*128, statarr(50)*2, stid*50, sttstr*256
C 
        INTEGER		itarr(5), jtarr(5), itype
        LOGICAL		done, found, match, haveit
C-----------------------------------------------------------------------
       iret   = 0
       icnt   = 0 
       icancl = 0
       iznflg = 0
       ilen   = 256
       done   = .false.
       found  = .false.
       haveit = .false.
       match  = .false.
       wfoid  = ' '
       attnln = ' '
       stzstr = ' '
C
C*     See if the system time or the user time will be used as the
C*     current end time.
C
       IF ( systim.eq. ' ' ) THEN
           srchtim = dattim
         ELSE
           srchtim = systim
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
       iorder = 1
       CALL FL_SCND ( path, templ, iorder, nexp, files, nfile, ier )
C
C*     Check for the last file requested by the user.
C
       CALL ST_LCUC ( dattim, dattim, ier )
       itype = 1
       CALL CSS_GTIM ( itype, cdttm, ier )
       CALL TI_STAN ( dattim, cdttm, dattm2, ier )
       IF ( ier .ne. 0 ) THEN
           CALL ER_WMSG ( 'TI', ier, dattim, ierr )
           iret = ier
           RETURN
       END IF	    
       CALL FL_MNAM ( dattm2, templ, filnam, ier )
C
C*     Find the earliest file to start searching. Subtract 48 hours
C*     from the time given.
C
       minuts = 2880
       CALL TI_CTOI ( dattm2, itarr, ier )
	CALL TI_SUBM ( itarr, minuts, jtarr, ier )
	CALL TI_ITOC ( jtarr, stime, ier )
	CALL FL_MNAM ( stime, templ, flstrt, ier )
C
C*     Get 4-digit year to compare dates.
C
       CALL TI_DTM4 ( dattm2, dattm4, ier )
C
C*     Decode each file until the end time is reached.
C
       nwtch = 0
       done  = .false.
       ifl = 1
       knt = 0
       jj = 0
C
	DO WHILE  ( ( ifl .le. nfile ) .and. ( .not. done ) )
	IF  ( files(ifl) .gt. filnam )  THEN
c              done = .true.
             ELSE
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
                               CALL ST_NUMB ( carr(6), icnmb, ier )
                               IF (  icnmb .eq. inumb ) THEN
                                   IF ( .not. found ) THEN 
C
C*                                     Store in arrays WFO ids.
C
                                       haveit = .True.
                                       found = .true.
                                       wfoid  = carr(8)
				       CALL ST_RMBL ( wfoid, wfoid, 
     +						len, ier )
                                   END IF
                                   IF ( carr(11) .eq. '1' )THEN
                                       icancl = icancl + 1
                                   END IF
        		       END IF
C
         		     jostat = 0
         		     DO WHILE  ( jostat .eq. 0 )
         		       READ ( lunf, 2, IOSTAT = jostat ) buffer
         		       IF  ( jostat .eq. 0 )  THEN
                                   CALL ST_ALNM (buffer(1:1), ityp,
     +				                  ier)
                                   IF ( (ityp .eq. 2 ) .and.
     +                                   ( haveit )  .and.
     +                                   (  icnmb .eq. inumb ) ) THEN
                                     IF ( jj .eq. 0 ) THEN
                                         statarr(1) = buffer(:2)
                                         jj = jj + 1
                                       ELSE
                                         DO ij  = 1, jj
                                           IF (buffer(1:2) .eq.
     +                                            statarr(ij)) THEN
                                             match = .True.
                                           END IF
					   IF (buffer(3:3).eq.'Z')THEN
                                               iznflg = 1
                                           END IF
                                         END DO
C
                                         IF ( .not. match ) THEN
                                           jj = jj + 1
                                           statarr(jj) = buffer(:2)
                                         END IF
                                         match = .False.
                                     END IF 
                                   END IF
         			   IF  ( buffer(1:1) .eq. '|' )  THEN
         			       CALL FL_BKSP ( lunf, ier )
         			       jostat = -1
         			   END IF
         		       END IF
         		     END DO
                             haveit = .false.
C
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
C*     Put states in array as a single string separated by a blank.
C
       CALL ST_LSTC (statarr, jj, ' ', stid, ier ) 
       CALL ST_RXBL (stid, stid, lens, ier ) 
       CALL ST_NULL (stid, stid, lens, ier ) 
       CALL WBC_DSTS ( stid, ilen, irmzn, stzstr, sttstr, ier )
       CALL ST_RNUL ( stzstr, stzstr, lens, ier )
C
	IF ( icancl .ge. 1 ) THEN 
           attnln = ' '
           stzstr = ' '
         ELSE
           attnln = wfoid
       END IF
       CALL ST_NULL (attnln, attnln, lens, ier )
C*        
       RETURN
       END
