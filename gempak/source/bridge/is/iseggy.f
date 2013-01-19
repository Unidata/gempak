	SUBROUTINE IS_EGGY ( report, lenr, iotarr, icorr, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, ifeggy,
     +			     origin, iret )
C************************************************************************
C* IS_EGGY 								*
C*									*
C* This subroutine decodes a single EGGY, RJAA, NTAA, or MUHA		*
C* international sigmet report.						*
C*                                                                      *
C* IS_EGGY ( REPORT, LENR, IOTARR, ICORR, TISSUE, GEMFIL, STNTBL,       *
C*	     IADSTN, MAXTIM, IFEGGY, ORIGIN, IRET )                   	*
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		International sigmet report     *
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	ICORR		INTEGER		Correction flag                 *
C*	TISSUE		CHAR*  		Bull. issue time, GEMPAK format *
C*	GEMFIL		CHAR*		Output file name template       *
C*	STNTBL		CHAR*		Station table                   *
C*	IADSTN		INTEGER		Number of additional stations   *
C*	MAXTIM 		INTEGER		Number of hours prior to CURTIM *
C*	IFEGGY		INTEGER		Country ID:  1 if EGGY; 2 if	*
C*					RJAA; 3 if NTAA; 4 if MUHA	*
C*      ORIGIN		CHAR*		Originating station id		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not a sigmet    *
C*					 -2 = format error in preamble  *
C*					 -3 = no phenomenon found       *
C*					 -4 = bad area definition       *
C*					 -5 = missing phenom terminator	*
C*					-10 = no storm center found     *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 1/00	Converted ISDECD for EGGY		*
C* F. J. Yen/NCEP	 2/00	Added RJAA				*
C* F. J. Yen/NCEP 	 4/00	Added MTW & replaced isegtb with isegtm	*
C* F. J. Yen/NCEP	 5/00	Distinquished between EGGY & RJAA CNL	*
C* F. J. Yen/NCEP	 5/01	Added NTAA.				*
C* F. J. Yen/NCEP	 7/01	Allow more than 1 phenom in an NTAA rec.*
C* F. J. Yen/NCEP	 8/01	Expanded test for phenomenon delimeter	*
C* F. J. Yen/NCEP	10/01	Added MUHA				*
C* F. J. Yen/NCEP	11/01	Added CB				*
C* F. J. Yen/NCEP	 1/02	Increased string length search for NTAA	*
C*				second phenom.				*
C* F. J. Yen/NCEP	 6/02	Added NTAA multi-area for same phenom.  *
C*				Changed length of phen2 and phenom2.	*
C* A. Hardy/NCEp 	 9/12	Added origin parameter			*
C* F. J. Yen/NCEP	10/03	Updated call to IS_PHEN for CSC.	*
C* F. J. Yen/NCEP	12/03	Replaced call to IS_HURC with IS_TCHU.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, origin
	INTEGER		iotarr (*)
C*
	PARAMETER	( MAXPTS = 20 )
	CHARACTER	parms (MMPARM)*4, filnam*132, stidnt*50,
     +			locid (4)*8, mwoid*8, phenom*2, remark*500,
     +			endpr*5, phen2*2, phenom2*2, phenm*2
	INTEGER		iflvl (2), jflvl (2)
	REAL		rlat (MAXPTS), rlon (MAXPTS)
	LOGICAL		proces, multar
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
C*	Process the preamble section.
C
	CALL IS_EGPR ( report, lenr, iotarr, ifeggy, locid, nloc,
     +                 stidnt, mwoid, icancl, iret )
	IF ( iret .ge. 0 ) THEN
C
C*	    Make a file name from the template and the time.
C*	    Open the file as part of the open file list.
C
            iflsrc = 2
 	    CALL FL_MNAM ( tissue, gemfil, filnam, ier )
 	    CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, 
     +	                   maxtim, lunf, nparm, parms, ier )
	    proces = .true.
	  ELSE
	    RETURN
	END IF
C
	phenm = '  '
	phenom = '  '
	jflflg = IMISSD
	jflvl (1) = IMISSD
	jflvl (2) = IMISSD
	IF ( icancl .eq. 1 ) THEN
C 
C*	    Report is a sigmet cancellation.  Save remarks, if any.
C
	    ibeg = INDEX ( report ( :lenr ), 'UTC' ) + 4
	    IF ( ibeg .eq. 4 ) ibeg = INDEX ( report (:lenr ), '-' ) + 2
	    iend = INDEX ( report ( ibeg:lenr ), '.' ) + ibeg - 1
	    IF ( iend .gt. ibeg ) remark = report ( ibeg:iend )
	    CALL IS_OUT ( lunf, 'CN', stidnt, origin, icorr,
     +			  iflflg, iflvl, idir, ispd,
     +			  ' ', IMISSD, rlat, rlon, 0, iret )
	    proces = .false.
	  ELSE

C
C*	    Continue processing sigmet.
C
	    endpr = mwoid ( :4 ) // '-'
	    ibeg  = INDEX ( report ( :lenr ), endpr )
	    IF ( ibeg .eq. 0 ) THEN
	        iret   = -2
	        proces = .false.
	    END IF
	END IF
C
C*	Check if possibly more than one phenomenon if RJAA or NTAA
C
	ilast = INDEX ( report ( :lenr), "=" )
	IF ( ilast .eq. 0 ) THEN
	    ilast = lenr
	END IF
	IF ( ifeggy .ge. 2 .and. ifeggy .le. 3 ) THEN
     	    CALL IS_EGFD ( report, lenr, ifeggy, ibnxt, iret )
	    IF ( ibnxt .eq. 0 ) THEN
	        ibnxt = ilast
	      ELSE
		IF ( ifeggy .eq. 3 ) THEN
		    IF ( ibnxt + 1 .eq. ilast ) ibnxt = ilast
		END IF
	    END IF
	  ELSE
	    ibnxt = ilast
	END IF	
	ifirst = 1
C
C*	Look for the phenomenon being reported.
C
	DO WHILE ( proces )
	    multar = .false.
	    IF ( ifirst .eq. 1) THEN
		ifirst = 0
	        ibeg = ibeg + 5
	        lens = ibnxt - ibeg + 1
	      ELSE
     		ibeg = ibnxt + 1
		len = lenr - ibeg + 1
     	        CALL IS_EGFD ( report (ibeg:lenr), len, ifeggy, ibnxt,
     +			       iret )
	        IF ( ibnxt .eq. 0 ) THEN
		    IF ( ifeggy .eq. 3 ) THEN
		        ibnxt = INDEX ( report ( ibeg+15:lenr), ")" )
		        IF ( ibnxt .eq. 0 ) THEN
			    ibnxt = ilast
		          ELSE
			    IF ( ibnxt .gt. 1) ibnxt = ibnxt - 1 +
     +						       ibeg + 15
		        END IF
		      ELSE
		        ibnxt = ilast
		    END IF
	          ELSE
		    ibnxt = ibnxt + ibeg - 1
	        END IF
		lens = ibnxt - ibeg + 1
	    END IF
	    IF ( phenm .eq. '  ') phenm = phenom
	    CALL IS_PHEN ( report ( ibeg:lenr ), lens, 120, phenom,
     +			   iptr, iret )
	    IF ( ifeggy .eq. 3 .and. phenom .eq. '  ' ) THEN
C
C*		For NTAA, if no phenomenon is found then a multi-area
C*		report so use current phenomenon and set multar.
C
		multar = .true.
     	        phenom = phenm
	        iret = 0
	    END IF
	    IF ( iret .ge. 0 ) THEN
C
C*		Check for possibly missing phenom terminator
C
		ibg2 = ibeg + iptr
		CALL IS_PHEN ( report ( ibg2:lenr ), lens, 120,
     +			       phenom2, iptr2, iret )
		IF ( iret .eq. 0 .and. iptr2 .ne. 0 .and.
     +			(iptr2+ibg2-ibeg) .le. lens ) THEN 
C
C*		    Exclude from error if not NTAA
C
		    IF ( ifeggy .ne. 3 ) THEN
			iret = -5
			RETURN
		    END IF
		END IF
C
C*	        Continue decoding based on type of phenomenon.
C
	        IF ( phenom .eq. 'TS' .or. phenom .eq. 'CB' ) THEN
C
C*	            Get and save flight level in case NTAA multi-area.
C
	            IF ( ifeggy .eq. 3 ) THEN
			CALL IS_FLVL (report ( ibeg:lenr ), lens,
     +			    iflflg, iflvl, iptr, ier )
              	        IF ( jflflg .eq. IMISSD .and.
     +				iflflg .ne. IMISSD ) THEN
			    jflflg = iflflg
			    jflvl (1) = iflvl (1)
			    jflvl (2) = iflvl (2)
			END IF
		      ELSE
		        jflflg = IMISSD
		    END IF
		    len = lens
		    iptr = 1
		    iptrsav = iptr
	            CALL IS_EGTS ( report(ibeg:lenr), len, 1, iptr,
     +			MAXPTS, stidnt, icorr, lunf, phenom, clat, clon,
     +			multar, jflflg, jflvl, ifeggy, origin, iret )
	    	    IF ( iptr .ne. iptrsav ) ibnxt = ilast
	          ELSE IF ( phenom .eq. 'TB' .or.
     +				phenom .eq. 'MW' ) THEN
		    len = lens
		    iptr = 1
	            CALL IS_EGTM ( report(ibeg:lenr), len, 1, iptr,
     +				MAXPTS, stidnt, icorr, lunf, phenom, 
     +				ifeggy, origin, iret )
	          ELSE IF ( ( phenom .eq. 'HU' ) .or.
     +			    ( phenom .eq. 'TC' ) .or.
     +			    ( phenom .eq. 'TR' ) .or.
     +			    ( phenom .eq. 'TD' ) ) THEN
	            CALL IS_TCHU ( report, lenr, ibeg, iptr, phenom,
     +				   stidnt, icorr, lunf, origin, iptout,
     +				   clat, clon, iret )
C
C*	            Check for thunderstorm data following tropical data.
C
	            ibeg = iptout
	            lens = lenr - ibeg + 1
	            CALL IS_PHEN ( report ( ibeg:lenr ), lens, 120, 
     +				   phen2, iptr, iret )
	            IF ( iret .lt. 0 ) THEN
		        iret = 0
	              ELSE
			IF ( phen2 .eq. 'TS' ) THEN
			    jflflg = IMISSD
			    jflvl (1) = IMISSD
			    jflvl (2) = IMISSD
	                    CALL IS_EGTS ( report(ibeg:lenr), lenr,
     +					 ibe, iptr, MAXPTS, stidnt,
     +					 icorr, lunf, 'TS', clat,
     +					 clon, multar, jflflg, jflvl,
     +					 ifeggy, origin, iret )
		        END IF
	            END IF
	          ELSE
		    iret = -3
		END IF
	    END IF
	    IF ( ibnxt .ge. ilast ) proces = .false.
	END DO
C
 	CALL DC_FCLS ( ier )
C*
	RETURN
	END
