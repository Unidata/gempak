	SUBROUTINE TF_DECD ( rpt, iprms, irtarr, ihhmm, lunf, cntry,
     +			     rdata, jrtarr, ntimes, stidnw, jvehr,
     +			     iret )
C************************************************************************
C* TF_DECD                                                             	*
C*                                                                      *
C* This subroutine decodes a single TAF report.  The input TAF report	*
C* string should begin with the first field after the Z time, which     *
C* follows the station id.                                              *
C*									*
C* TF_DECD ( RPT, IPRMS, IRTARR, IHHMM, LUNF, CNTRY, RDATA, JRTARR,     *
C*	     NTIMES, STIDNW, JVEHR, IRET )				* 
C*								        *
C* Input parameters:						        *
C*	RPT		CHAR*		TAF report string               *
C*	IPRMS (*)	INTEGER		Position of parameters in list  *
C*	IRTARR (5)	INTEGER		Integer report time array       *
C*	IHHMM		INTEGER		Report time (hhmm)              *
C*	LUNF		INTEGER		Surface file number             *
C*	CNTRY		CHAR*		Country code                    *
C*								        *
C* Output parameters:						        *
C*	RDATA		REAL		Forecast data                   *
C*	  (48,MMPARM)                                                   *
C*	JRTARR (5)     	INTEGER		Integer initial fcst time array *
C*	NTIMES		INTEGER		Number of forecast times        *
C*	STIDNW		CHAR*		New station name found in report*
C*	JVEHR		INTEGER		Hour of ending valid period     *
C*	IRET		INTEGER		Return code                     *
C*					  0 = normal return             *
C*					 -1 = valid period not found    *
C**								        *
C* Log:								        *
C* D. Kidwell/NCEP	 9/02                                           *
C* D. Kidwell/NCEP	10/02	Added lunf,stidnw; better time & err chk*
C* A. Hardy/NCEP	12/02   Removed stripping of stn id's 'K' & 'C' *
C* D. Kidwell/NCEP	 5/03	Added VWNM, TVWN, LLWS, MOTV; cleaned up*
C* D. Kidwell/NCEP	 6/03	Allowed WSCONDS to indicate LLWS        *
C* F. J. Yen/NCEP	 2/04	Updated calls to BR_SKY6 & BR_VISB.(CSC)*
C* D. Kidwell/NCEP	10/04	Added CTYL, TCTL for CB/TCU             *
C* F. J. Yen/NCEP	 7/07	Added STIM & CSC: added IHHMM and JVEHR	*
C* L. Lin/NCEP   	 4/08	Allows fcst hours to 30 with new format *
C* L. Lin/NCEP   	 4/08	Accepts the mixture of new or old report*
C*                              time format and treats "BECM" or "BEC"  *
C*                              as "BECMG"                              *
C* S. Jacobs/NCEP	 1/14	Added BY as a valid start of remark	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
C*
	CHARACTER*(*) 	rpt, stidnw, cntry
	INTEGER		iprms (*), irtarr (*), jrtarr (*)
	REAL		rdata (48,*)
C*
	PARAMETER	( NFCTMX = 48 )
	CHARACTER	carr (500)*20, outstr*20, wthr (3)*20, wchr*62,
     +			wthrvc (3)*20, wchrvc*62, stn*8
	INTEGER		locfci (50)
	LOGICAL		done, numerc, good, twofld, dirvis, look, wxfnd,
     +			store, nscflg, nswflg, fm
	REAL		sky (6), skypri (3), MT_CEIL
        LOGICAL         tmidxc, tmidxr
C
        INTEGER         ibday, ieday
C*
	INCLUDE		'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
	ntimes = 0
	stidnw = ' '
C
        ibday = IMISSD
        ieday = IMISSD
C
C*      tmidxr will be set to true if the report period in a new format
C*      such as YYGG/YYgg
C
        tmidxr = .false.
C
C*      tmidxc will be set to true if the forecast change indicator in 
C*      new format such as FMYYGGgg, BECMG 1608/1812, TEMPP 1604/1612
C
        tmidxc = .false.
C
	DO ii = 1, NFCTMX
	    DO jj = 1, MMPARM
		rdata ( ii, jj ) = RMISSD
	    END DO
	END DO
	DO ii = 1, 5
	    jrtarr ( ii ) = irtarr ( ii )
	END DO
C
C*	Break the report into "words".
C
	CALL ST_CLSL ( rpt, ' ', ' ', 500, carr, num, ier )
	CALL ST_FIND ( 'AMD', carr, num, ipos, ier )
	IF ( ipos .gt. 0 ) THEN
	    IF ( ipos .gt. 1 ) THEN
		IF ( carr ( ipos - 1 ) .ne. 'TAF' ) num = ipos - 1
	    END IF
	  ELSE
C
C*	    Check for a remark section and exclude it from processing.
C
C*	    At least one station issued a report for another station
C*	    and marked with with a "BY xxxx" clause that was not
C*	    preceded by a RMK. So, look for BY or RMK to signify
C*	    a remark. 
C
	    CALL ST_FIND ( 'BY', carr, num, ipos, ier )
	    IF ( ipos .gt. 0 ) num = ipos - 1
C
	    CALL ST_FIND ( 'RMK', carr, num, ipos, ier )
	    IF ( ipos .gt. 0 ) num = ipos - 1
	END IF
C
C*	Look for and clean up miscoded PROB and VC fields.
C
	numnew = num
	DO ii = 1, num
	    IF ( carr ( ii ) ( :4 ) .eq. 'PROB' ) THEN
	        CALL ST_LSTR ( carr ( ii ), lens, ier ) 
		IF ( lens .eq. 4 ) THEN
		    ii1 = ii + 1
		    IF ( ( carr ( ii1 ) .eq. '30' ) .or.
     +			 ( carr ( ii1 ) .eq. '40' ) ) THEN
			carr ( ii ) = carr ( ii ) ( :4 ) // carr ( ii1 )
			numnew = numnew - 1
			DO jj = ii1, numnew
			    carr ( jj ) = carr ( jj + 1 )
			END DO
		    END IF
		  ELSE IF ( lens .eq. 6 ) THEN
		    IF ( carr ( ii ) .eq. 'PROB3O' ) THEN
			carr ( ii ) = 'PROB30'
		      ELSE IF ( carr ( ii ) .eq. 'PROB4O' ) THEN
			carr ( ii ) = 'PROB40'
		    END IF
		END IF
	      ELSE IF ( carr ( ii ) ( :2 ) .eq. 'VC' ) THEN
	        CALL ST_LSTR ( carr ( ii ), lens, ier ) 
		IF ( lens .eq. 2 ) THEN
		    ii1 = ii + 1
		    carr ( ii ) = carr ( ii ) ( :2 ) // carr ( ii1 )
		    numnew = numnew - 1
		    DO jj = ii1, numnew
		        carr ( jj ) = carr ( jj + 1 )
		    END DO
		END IF
	    END IF
	END DO
C
	num  = numnew
	ii   = 1
	done = .false.
C
C*	Look for the valid period (2 digit day, 2 digit beginning time, 
C*	2 digit ending time).
C
	DO WHILE ( .not. done )
	    CALL ST_LSTR ( carr ( ii ), lens, ier )
C
C*          Check the bulletin report time in YYGGgg or YYGG/YYgg
C
            IF ( ( lens .eq. 6 ) .or. ( lens .eq. 9 ) ) THEN
               numerc = .true.
               CALL TF_VNUM ( carr ( ii ), lens, numerc )
       	       IF ( numerc ) done = .true.
            ENDIF
C
	    IF ( .not. done ) THEN
		ii = ii + 1
		IF ( ii .gt. num ) THEN
		    iret = -1
		    RETURN
		END IF
C
C*          ii=1 has old report time format
C*          Check if the report time is YYGG/YYgg in the ii=2
C
            ELSE 
                ii = ii + 1
	        CALL ST_LSTR ( carr ( ii ), lens2, ier )
                IF ( lens2 .eq. 9 .and. carr( ii )(5:5) .eq. '/' ) THEN
                   numerc = .true.
                   CALL TF_VNUM ( carr ( ii ), lens, numerc )
       	           IF ( numerc ) done = .true.
                   lens = lens2
                   tmidxr = .true.
                ELSE
                   ii = ii - 1
                   done = .true.
                END IF
	    END IF
	END DO
C
C*	Get the valid period day and beginning and ending times.
C
        CALL TF_GVTM ( carr ( ii ), lens, ivbday, iveday,
     +                 ivbhr, ivehr, iret )
        IF ( iret .eq. -1 ) RETURN
C
C*	Get the initial forecast time array.
C
        CALL TF_GFTM ( irtarr, jrtarr, lens, ivbday, iveday,
     +                 ivbhr, ivehr, ntimes, ibtime, ietime,
     +                 tmidxr, iret )
C
        IF ( iret .eq. -1 ) RETURN
C
	done = .false.
	ii   = ii + 1
	IF ( ii .gt. num ) THEN
	    jvehr = ivehr
	    RETURN
	END IF
C
C*	Find the positions of the forecast change indicators.
C
	iloc = 0
	DO jj = ii, num
	    fm = .false.
 	    IF ( carr ( jj ) ( :2 ) .eq. 'FM' ) THEN
		CALL ST_WORD ( carr ( jj ), ityp, ier )
		IF ( ityp .ne. 0 ) fm = .true.
	    END IF
C
	    IF ( fm .or. ( carr ( jj ) .eq. 'BECMG'  ) .or.
     +	       ( carr ( jj ) .eq. 'BECM'  ) .or.
     +	       ( carr ( jj ) .eq. 'BEC'  ) .or.
     +	       ( carr ( jj ) .eq. 'TEMPO'  ) .or.
     +	       ( carr ( jj ) .eq. 'PROB30' ) .or.
     +	       ( carr ( jj ) .eq. 'PROB40' ) ) THEN
		iloc = iloc + 1
		locfci ( iloc ) = jj
	    END IF
	END DO
	nlocfc = iloc + 1
	locfci ( nlocfc ) = num + 1
C
	itype  = 0
	iloc   = 1
	itprob = IMISSD
	isvprb = IMISSD
	good   = .true.
	lastdx = 0
	IF ( carr ( ii ) .eq. 'NIL' ) done = .true.
	DO WHILE ( .not. done )
	    IF ( ii . eq. locfci ( iloc ) ) THEN
C
C*		A forecast change indicator was found.  Get the type and
C*		beginning and ending times.
C
	        CALL TF_FCIN ( carr, num, ii, itype, ibtime, ietime, 
     +			       itprob, ibday, ieday, tmidxc, ier )
		IF ( ier .ge. 0 ) THEN
		    good = .true.
		    IF ( itype .gt. 1 ) THEN
			ii = ii + 1
			IF ( ier .eq. 1 ) THEN
			    ii   = ii + 1
			    iloc = iloc + 1
			  ELSE
			    IF ( itype .eq. 3 ) itprob = isvprb
			END IF
		    END IF
		    IF ( ( itype .le. 2 )  .and. ( .not. tmidxc ) )
     +                   ietime = ivehr
		    isvprb = IMISSD
		  ELSE
		    good   = .false.
		    isvprb = itprob
		END IF
		ii   = ii + 1
		iloc = iloc + 1
		IF ( ii .gt. num ) done = .true.
	      ELSE IF ( itype .ne. 0 ) THEN
		good = .false. 
		CALL ST_LSTR ( carr ( ii ), lens, ier )
		IF ( lens .eq. 4 ) THEN
C
C*		    Check for a station name (new report).
C
		    CALL ST_WORD ( carr ( ii ), ityp, ier ) 
		    IF ( ityp .eq. 0 ) THEN
		        stn = carr ( ii )
			CALL SF_FSTN ( lunf, stn, ier )
			IF ( ier .eq. 0 ) THEN
			    stidnw = carr ( ii )
			    ii     = num
			END IF
		    END IF
		END IF
		ii = ii + 1
	    END IF
C
	    IF ( good ) THEN
C
C*	        Get the indices associated with the forecast hours.
C
                CALL TF_GITM ( irtarr, jrtarr, itype, iveday, ibday,
     +                       ieday, ivbhr, ntimes, ibtime, ietime,
     +                       tmidxc, tmidxr, indxb, indxe, good, iret )
                IF ( iret .eq. -1 ) RETURN
C
		last   = 0
		lastpv = 0
		ibad   = 0
C
	        DO WHILE ( good .and. ( iret .eq. 0 ) )
C
C*	            Attempt to decode the forecast field.
C*		    Look for wind group.
C
		    IF ( last .eq. 0 ) THEN
		        CALL BR_WIND ( carr ( ii ), drct, sknt, sped,
     +			               gust, gums, idecd, ier )
		        IF ( idecd .eq. 1 ) THEN
		            ii = ii + 1
		            IF ( ii .eq. locfci (iloc) )  good = .false.
		            last = 1
		        END IF
		    END IF
C
C*		    Look for visibility group.
C
		    IF ( last .le. 1 ) THEN
		        IF ( good ) THEN
			    autof = RMISSD
		            CALL BR_VISB ( carr ( ii ), carr ( ii+1 ),
     +			  	           autof, visby, visbk, visfl,
     +					   idecd, twofld, dirvis, ier )
		            IF ( idecd .eq. 1 ) THEN
		                ii = ii + 1
		                IF ( twofld ) ii = ii + 1
		                IF ( ii .eq. locfci ( iloc ) ) 
     +				     good = .false.
			        last = 2
		            END IF
		          ELSE
		            visby = RMISSD
		            visbk = RMISSD
		        END IF
		    END IF
C
C*		    Look for up to 3 weather groups.
C
		    IF ( last .le. 2 ) THEN
		        wxnum  = RMISSD
		        nswflg = .false.
		        wxnvc  = RMISSD
		        IF ( good ) THEN
		            nwea   = 0
		            iwea   = 0
		            iweavc = 0
		            look   = .true.
		            DO WHILE ( look )
	    		        CALL ST_LSTR ( carr ( ii ), lens, ier )
			        CALL DC_WTHR ( carr ( ii ), lens, wxfnd,
     +				               outstr, ier )
			        IF ( .not. wxfnd ) THEN
			            IF ( carr ( ii ) .eq. 'NSW' ) THEN
				        wxfnd  = .true.
				        nswflg = .true.
				        look   = .false.
			            END IF
			        END IF
			        IF ( wxfnd ) THEN
			            nwea = nwea + 1
			            IF (( outstr (:2) .ne. 'VC'  ) .and.
     +				        ( outstr (:3) .ne. '+VC' ) .and.
     +				        ( outstr (:3) .ne. '-VC' )) THEN
				        iwea = iwea + 1
			                wthr ( iwea ) = outstr
			              ELSE
				        iweavc = iweavc + 1
				        IF ( outstr (:2) .eq. 'VC') THEN
				            wthrvc (iweavc) = outstr(3:)
				          ELSE
				            wthrvc (iweavc) = outstr(:1)
     +					                   // outstr(4:)
				        END IF
			            END IF
			            IF ( nwea .eq. 3 ) look = .false.
			            ii   = ii + 1
			            IF ( ii .eq. locfci ( iloc ) ) THEN
				        good = .false.
				        look = .false.
			            END IF
			            last = 3
			          ELSE
			            look = .false.
			        END IF
		            END DO
C
C*		            Get the weather number.
C
		            IF ( (iwea .ge. 1) .and. .not. nswflg ) THEN
			        CALL ST_LSTC ( wthr, iwea, ' ', wchr, 
     +					       ier )
			        CALL ST_RMBL ( wchr, wchr, lens, ier )
     				wxnum = PT_WNMT ( wchr )
		            END IF
C
C*		            Get the vicinity weather number.  Treat
C*			    'VCSH' as 'VCSHRA'.
C
		            IF ( ( iweavc .ge. 1 ) .and. .not. nswflg )
     +				   THEN
			        DO jj = 1, iweavc
			            IF ( ( wthrvc (jj) .eq. 'SH'  ) .or.
     +			                 ( wthrvc (jj) .eq. '+SH' ) .or.
     +			                 ( wthrvc (jj) .eq. '-SH' ))THEN
			                CALL ST_LSTR ( wthrvc (jj), 
     +						       lenw, ier ) 
			                wthrvc ( jj ) = 
     +					   wthrvc (jj) ( :lenw ) // 'RA'
			            END IF
			        END DO
			        CALL ST_LSTC ( wthrvc, iweavc, ' ',
     +					       wchrvc, ier)
			        CALL ST_RMBL ( wchrvc, wchrvc, lens, 
     +					       ier )
			        wxnvc = PT_WNMT ( wchrvc )
		            END IF
		        END IF
		    END IF
C
C*		    Look for up to 6 sky cover groups.
C
		    IF ( last .le. 3 ) THEN
		        DO jj = 1, 3
		            sky ( jj ) = RMISSD
		        END DO
		        ceil   = RMISSD
			ctypll = RMISSD
		        nscflg = .false.
C
		        IF ( good ) THEN
		            isky = 1
		            nsky = 0
		            look = .true.
		            DO WHILE ( look )
			        IF ( carr ( ii ) .eq. 'NSC' ) THEN
			            nscflg = .true.
			            carr ( ii ) = 'SKC'
			        END IF
			        autof = RMISSD
			        CALL BR_SKY6 ( carr ( ii ), carr (ii+1),
     +				               autof, isky, vrtv, cmtn,
     +					       jsky, ctyl, idecd, 
     +					       twofld, ier )
			        IF ( idecd .eq. 1 ) THEN
			            sky ( jsky ) = cmtn 
			            nsky = jsky
				    IF ( ERMISS (ctypll) ) ctypll = ctyl
			            isky = isky + 1
			            IF ( isky .ge. 7 ) look = .false.
			            ii   = ii + 1
			            IF ( twofld ) ii = ii + 1
			            IF ( ii .eq. locfci ( iloc ) ) THEN
				        good = .false.
				        look = .false.
			            END IF
			            last = 4
			          ELSE
			            look = .false.
			        END IF
		            END DO
C
C*		            Choose 3 priority layers if more than 3 were 
C*		            reported.
C
		            IF ( nsky .gt. 3 ) THEN
			        CALL BR_GMSK ( nsky, sky, skypri, ier )
			        nsky = 3
			        DO jj = 1, 3
			            sky ( jj ) = skypri ( jj )
			        END DO
		            END IF
C
C*		            Remove cloud flags added by BR_SKY6.
C
		            DO jj = 1, nsky
                                IF ( sky ( jj ) .gt. 30000. ) THEN
                                    sky ( jj ) = sky ( jj ) - 30000.
                                  ELSE IF ( sky ( jj ) .gt. 20000.) THEN
                                    sky ( jj ) = sky ( jj ) - 20000.
                                END IF
                                IF ( .not. ERMISS ( sky ( jj ) ) )
     +                               sky ( jj ) = ABS ( sky (jj) )
		            END DO
		            ceil = MT_CEIL ( sky (1), sky (2), sky (3) ) 
		        END IF
		    END IF
C
C*		    Look for low level wind shear (WS at or below 
C*		    2000 feet).
C
		    IF ( last .le. 4 ) THEN
			wshear = RMISSD
			IF ( good ) THEN
			    IF ( carr ( ii ) ( :2 ) .eq. 'WS' ) THEN
				CALL ST_LSTR ( carr ( ii ), lens, ier )
				IF ( ( ( lens .eq. 13 ) .and.
     +				       ( carr (ii) (12:13) .eq. 'KT' ) ) 
     +				     .or. ( carr (ii) (6:6) .eq. '/' ) )
     +				     THEN
				    CALL ST_INTG ( carr ( ii ) ( 3:5 ),
     +						   iwsft, ier )
				    IF ( ( ier .eq. 0 ) .and.
     +					 ( iwsft .le. 20 ) ) 
     +				         wshear = 1.
				  ELSE IF ( ( lens .eq. 7 ) .and.
     +					    ( carr ( ii ) ( 3:7 ) .eq.
     +					      'CONDS' ) ) THEN
				    wshear = 1.
				END IF
			    END IF
			    IF ( .not. ERMISS ( wshear ) ) THEN
				ii = ii + 1
				IF ( ii .eq. locfci ( iloc ) ) 
     +				     good = .false.
				last = 5
			    END IF
			END IF	
		    END IF
C
		    IF ( good ) THEN
		        CALL ST_LSTR ( carr ( ii ), lens, ier )
		        IF ( lens .eq. 4 ) THEN
C
C*		            Check for a station name (new report).
C
		            CALL ST_WORD ( carr ( ii ), ityp, ier ) 
		            IF ( ityp .eq. 0 ) THEN
			        stn = carr ( ii )
			        CALL SF_FSTN ( lunf, stn, ier )
			        IF ( ier .eq. 0 ) THEN
			            stidnw = carr ( ii )
			            ii     = num
			        END IF
		            END IF
		        END IF
		        ii = ii + 1
			IF ( ( ii .gt. num ) .or.
     +			     ( ii .eq. locfci ( iloc ) ) .or.
     +			     ( last .eq. 5 ) ) good = .false.
C
C*		        If 3 consecutive undecodable fields were found,
C*		        quit trying to decode for this change indicator.
C
		        IF ( last .eq. lastpv ) THEN
			    ibad = ibad + 1
			    IF ( ( ii - 1 ) .eq. locfci ( iloc ) ) THEN
				iloc = iloc + 1
				good = .false.
			    END IF
     			    IF ( ibad .eq. 3 )  good = .false.
		          ELSE
			    ibad   = 0
			    lastpv = last
		        END IF
		    END IF
	        END DO
C
C*	        Set up values for data storage.
C
              IF ( iret .eq. 0 ) THEN
		IF ( itype .le. 2 ) THEN
		    incr = 0
		    jncr = 0
		  ELSE
		    incr = 8
		    jncr = 1
		    IF ( itype .eq. 3 ) THEN
			curprb = 50.
		      ELSE IF ( itype .eq. 4 ) THEN
		        curprb = 30.
		      ELSE IF ( itype .eq. 5 ) THEN
		        curprb = 40.
		    END IF
		END IF
		tprob = FLOAT ( itprob )
C
		IF ( ERMISS ( sknt ) ) sknt = PR_MSKN ( sped )
		IF ( ERMISS ( gust ) ) gust = PR_MSKN ( gums )
		IF ( ERMISS ( visby ) ) THEN
		    v1    = PR_HGKM ( visbk )
		    v2    = PR_HGMF ( v1 )
		    visby = PR_HGFS ( v2 )
		END IF
C
C*		Look for flag values denoting data values that need to
C*		be forced to missing - VRB for wind direction and NSW
C*		for weather.
C
		IF ( drct .lt. -90. ) drct = RMISSD
		IF ( nswflg ) THEN
		    wxnum = RMISSD
		    wxnvc = RMISSD
		END IF
C
C*		If NSC was specified, use the previous cloud group if it
C*		exists, else treat as SKC.
C
		IF (  ( indxb .eq. 1 ) .or. ( itype .le. 1 ) ) THEN
		    nscflg = .false.
		  ELSE IF ( ERMISS ( rdata (indxb-1, iprms (5)) ) ) THEN
		    nscflg = .false.
		END IF
C
C*		Store the decoded data to the output array.
C
		DO jj = indxb, indxe
		    store = .true.
		    IF ( ( indxb .lt. lastdx ) .and.
     +			 ( itype .le. 2 ) ) store = .false.
		    IF ( ( itype .lt. 2 ) .or. ( itype .gt. 3 ) .or.
     +			 ( ( itype .eq. 2 ) .and. 
     +			   ( cntry .eq. 'US' ) ) ) THEN
C
C*			This is initial conditions or 'FM' or 'PROB30'
C*			or 'PROB40' or, for U.S. stations only, 'BECMG'.
C
			IF ( itype .ge. 4 ) THEN
			    IF ( curprb .lt. rdata ( jj, iprms (17) ) )
     +			         store = .false.
			END IF
C
			IF ( store ) THEN
		            rdata ( jj, iprms ( 1 + incr ) ) = sknt
		            rdata ( jj, iprms ( 2 + incr ) ) = drct
		            rdata ( jj, iprms ( 3 + incr ) ) = gust
		            rdata ( jj, iprms ( 4 + incr ) ) = wxnum
			    IF ( .not. nscflg ) THEN
			      IF ( ( itype .le. 2 ) .or. 
     +			           ( .not. ERMISS ( sky ( 1 ) ) ) ) THEN
		                  rdata ( jj, iprms (5+incr) ) = sky (1)
		                  rdata ( jj, iprms (6+incr) ) = sky (2)
		                  rdata ( jj, iprms (7+incr) ) = sky (3)
				  rdata ( jj, iprms (18+jncr)) = ceil
				  rdata ( jj, iprms (24+jncr)) = ctypll
			        ELSE
		                  rdata ( jj, iprms ( 13 ) ) =
     +		                          rdata ( jj, iprms ( 5 ) )
		                  rdata ( jj, iprms ( 14 ) ) =
     +		                          rdata ( jj, iprms ( 6 ) )
		                  rdata ( jj, iprms ( 15 ) ) =
     +		                          rdata ( jj, iprms ( 7 ) )
				  rdata ( jj, iprms ( 19 ) ) = 
     +					  rdata ( jj, iprms ( 18 ) )
				  rdata ( jj, iprms ( 25 ) ) = 
     +					  rdata ( jj, iprms ( 24 ) )
			      END IF
			    END IF
		            rdata ( jj, iprms ( 8 + incr ) ) = visby
			    IF ( itype .ge. 4 )
     +		        	 rdata ( jj, iprms ( 17 ) ) = curprb
			    rdata ( jj, iprms ( 20 + jncr ) ) = wxnvc
			    rdata ( jj, iprms ( 22 ) ) = wshear
			    rdata ( jj, iprms ( 26 ) ) = float (ihhmm)
			END IF
		      ELSE
C
C*			This is 'BECMG' (for non-U.S. stations) or
C*			'TEMPO'.
C
			IF ( ( itype .eq. 3 ) .and. 
     +			       .not. ERMISS ( tprob ) ) THEN
C
C*			    Do not let 'PROBnn TEMPO' overwrite 'TEMPO'.
C
			    IF ( rdata ( jj, iprms ( 17 ) ) .gt. 45. )
     +				 store = .false.
			END IF
C
			IF ( ( itype .eq. 3 ) .and. store .and.
     +		     	     ( .not. ERMISS (rdata (jj, iprms (17))) ) )
     +			     THEN
			    IF ( rdata ( jj, iprms (17) ) .lt. curprb ) 
     +				 THEN
C
C*				Set 'PROBnn' fields missing before 
C*				overwriting with 'TEMPO' data.  ('TEMPO'
C*				and 'PROBnn' fields should not overlap
C*				in time, but occasionally they do.)
C
				DO kk = 9, 17
				    rdata ( jj, iprms ( kk ) ) = RMISSD
				END DO
				rdata ( jj, iprms ( 19 ) ) = RMISSD
				rdata ( jj, iprms ( 21 ) ) = RMISSD
				rdata ( jj, iprms ( 25 ) ) = RMISSD
			    END IF
			END IF
C
			IF ( store ) THEN
			    IF ( .not. ERMISS ( sknt ) ) THEN
      		                rdata ( jj, iprms ( 1 + incr ) ) = sknt
      		                rdata ( jj, iprms ( 2 + incr ) ) = drct
      		                rdata ( jj, iprms ( 3 + incr ) ) = gust
			    END IF
			    IF ( .not. ERMISS ( wxnum ) )
     +		                 rdata ( jj, iprms ( 4+incr ) ) = wxnum
			    IF ( .not. nscflg ) THEN
			      IF ( .not. ERMISS ( sky ( 1 ) ) ) THEN
		                  rdata ( jj, iprms (5+incr) ) = sky (1)
		                  rdata ( jj, iprms (6+incr) ) = sky (2)
		                  rdata ( jj, iprms (7+incr) ) = sky (3)
			          rdata ( jj, iprms ( 18+jncr ) ) = ceil
			          rdata ( jj, iprms (24+jncr) ) = ctypll
			        ELSE IF ( itype .eq. 3 ) THEN
			          IF ( ERMISS ( rdata(jj, iprms(13)) ) )
     +				       THEN
		                      rdata ( jj, iprms ( 13 ) ) =
     +		                              rdata ( jj, iprms ( 5 ) )
		                      rdata ( jj, iprms ( 14 ) ) =
     +		                              rdata ( jj, iprms ( 6 ) )
		                      rdata ( jj, iprms ( 15 ) ) =
     +		                              rdata ( jj, iprms ( 7 ) )
				      rdata ( jj, iprms ( 19 ) ) = 
     +					      rdata ( jj, iprms ( 18 ) )
				      rdata ( jj, iprms ( 25 ) ) = 
     +					      rdata ( jj, iprms ( 24 ) )
			          END IF
			      END IF
			    END IF
			    IF ( .not. ERMISS ( visby ) )
     +		                 rdata ( jj, iprms ( 8+incr ) ) = visby
			    IF ( itype .eq. 3 ) 
     +		                 rdata ( jj, iprms ( 17 ) ) = curprb
			    IF ( .not. ERMISS ( wxnvc ) ) 
     +				 rdata ( jj, iprms ( 20+jncr ) ) = wxnvc
			    IF ( .not. ERMISS ( wshear ) )
     +				 rdata ( jj, iprms ( 22 ) ) = wshear
			    rdata ( jj, iprms ( 26 ) ) = float (ihhmm)
			END IF
		    END IF
C
		    IF ( nscflg .and. store ) THEN
C
C*			Store the previous cloud group for NSC.
C
			indxp = indxb	
			IF ( itype .eq. 2 ) indxp = indxp - 1
			rdata ( jj, iprms ( 5 + incr ) )  = 
     +				rdata ( indxp, iprms ( 5 ) ) 
			rdata ( jj, iprms ( 6 + incr ) )  = 
     +				rdata ( indxp, iprms ( 6 ) ) 
			rdata ( jj, iprms ( 7 + incr ) )  = 
     +				rdata ( indxp, iprms ( 7 ) ) 
			rdata ( jj, iprms (18 + jncr ) )  = 
     +				rdata ( indxp, iprms ( 18 ) ) 
			rdata ( jj, iprms (24 + jncr ) )  = 
     +				rdata ( indxp, iprms ( 24 ) ) 
		    END IF
C
		    IF ( nswflg .and. store ) THEN
C
C*			Set all weather groups to missing for NSW.
C
			rdata ( jj, iprms ( 4 + incr ) )  = RMISSD
			rdata ( jj, iprms ( 20 + jncr ) ) = RMISSD
C
C*			Do not let TEMPO or PROB NSW override prevailing
C*			weather.
C
			IF ( itype .le. 2 ) THEN
			    rdata ( jj, iprms ( 12 ) ) = RMISSD
			    rdata ( jj, iprms ( 21 ) ) = RMISSD
			END IF
		    END IF
		END DO
		IF ( ( indxb .ge. lastdx ) .and.
     +		     ( itype .le. 2 ) ) lastdx = indxb
		IF ( itype .eq. 2 ) lastdx = lastdx - 1
	     END IF
           END IF

	   IF ( ii .gt. num ) THEN
		done = .true.
	     ELSE IF ( itype .eq. 0 ) THEN
		itype = -1
	   END IF
	END DO
	jvehr = ivehr
C*
	RETURN
	END
