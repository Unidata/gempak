	SUBROUTINE IS_CNDA ( report, lenr, iotarr, icorr, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, ifeggy,
     +			     origin, iret )
C************************************************************************
C* IS_CNDA 								*
C*									*
C* This subroutine decodes a single Canadian SIGMET.			*
C*                                                                      *
C* IS_CNDA ( REPORT, LENR, IOTARR, ICORR, TISSUE, GEMFIL, STNTBL,       *
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
C*	IFEGGY		INTEGER		Country ID: 10 if Canada	*
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
C* F. J. Yen/NCEP	10/03	Converted IS_DECD for Canadian SIGMETs	*
C* F. J. Yen/NCEP	12/03	Replaced IS_HURC and IS_TC with IS_TCHU.*
C* F. J. Yen/NCEP	12/03	Updated call to IS_CNAR with origin.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, origin
	INTEGER		iotarr (*)
C*
	PARAMETER	( MAXPTS = 20 )
	CHARACTER	parms (MMPARM)*4, filnam*132, stidnt*50,
     +			locid (4)*8, mwoid*8, phenom*2, remark*500,
     +			phen2*2, phenm*2
	INTEGER		iflvl (2), jflvl (2)
	REAL		rlat (MAXPTS), rlon (MAXPTS)
	LOGICAL		proces, trop
C------------------------------------------------------------------------
	iret  = 0
	trop = .false.
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
C*	Process the preamble section.
C
	CALL IS_CNPR ( report, lenr, iotarr, origin, locid, nloc,
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
C*	Stop search if ' END/' found
C
	nend = INDEX ( report ( :lenr ), ' END/' )
	IF ( nend .ne. 0 ) THEN
	    nlen = MIN ( lenr, nend )
	  ELSE
	    nlen = lenr
	END IF
	phenm = '  '
	phenom = '  '
	jflflg = IMISSD
	jflvl (1) = IMISSD
	jflvl (2) = IMISSD
	IF ( icancl .eq. 1 ) THEN
C 
C*	    Report is a sigmet cancellation.  Save remarks, if any.
C
	    ibeg = INDEX ( report ( :nlen ), 'UTC' ) + 4
	    IF ( ibeg .eq. 4 ) ibeg = INDEX ( report (:nlen ), '-' ) + 2
	    iend = INDEX ( report ( ibeg:nlen ), '.' ) + ibeg - 1
	    IF ( iend .gt. ibeg ) remark = report ( ibeg:iend )
	    CALL IS_OUT ( lunf, 'CN', stidnt, origin, icorr,
     +			  iflflg, iflvl, idir, ispd,
     +			  ' ', IMISSD, rlat, rlon, 0, iret )
	    proces = .false.
	  ELSE

C
C*	    Continue processing sigmet.
C
	    ibeg  = INDEX ( report ( :nlen ), '-' )
	    ibegds = ibeg
	    IF ( ibeg .eq. 0 ) THEN
	        iret   = -2
	        proces = .false.
	    END IF
	END IF
C
C*	Look for the phenomenon being reported.
C
        IF ( proces ) THEN
C
C*	    Look for the area
C
	    ibeg = ibeg + 1
	    lena = nlen - ibeg + 1
 	    CALL IS_CNAR ( report(ibeg:nlen), lena, MAXPTS, origin, npt,
     +		rlat, rlon, irad, iptr, ier )
	    IF ( ier .ne. 0 ) THEN
		iret = -4
 		CALL DC_FCLS ( ier )
		RETURN
	    END IF
            ibeg = ibeg + 4
	    ibeg = ibeg + iptr - 1
            lens = nlen - ibeg + 1
            CALL IS_PHEN ( report ( ibeg:nlen ), lens, 450, phenom,
     +                     iptr, iret )
            IF ( iret .ge. 0 ) THEN
C
C*              Continue decoding based on type of phenomenon.
C
                IF ( phenom .eq. 'TS' .or. phenom .eq. 'TB' .or.
     +                  phenom .eq. 'CT' .or. phenom .eq. 'CB' .or.
     +                  phenom .eq. 'MW' .or. phenom .eq. 'WS' .or.
     +			phenom .eq. 'IC' .or. phenom .eq. 'GR' .or.
     +			phenom .eq. 'SQ' .or. phenom .eq. 'DS' .or.
     +			phenom .eq. 'SS' ) THEN
                    CALL IS_CNPH ( report, nlen, ibeg, ibegds, iptr,
     +                  npt, rlat, rlon, stidnt, icorr, lunf, phenom,
     +                  clat, clon, origin, irad, iret )
                  ELSE IF ( ( phenom .eq. 'HU' ) .or.
     +                      ( phenom .eq. 'TC' ) .or.
     +                      ( phenom .eq. 'TD' ) .or.
     +                      ( phenom .eq. 'TR' ) ) THEN
                    CALL IS_TCHU ( report, nlen, ibeg, iptr, phenom,
     +                             stidnt, icorr, lunf, origin, iptout,
     +                             clat, clon, iret )
                    IF ( iret .ne. 0 ) THEN
                        trop = .false.
                      ELSE
                        trop = .true.
                    END IF
                  ELSE IF ( phenom .eq. 'VA' ) THEN
                    CALL IS_VA ( report, nlen, ibeg, iptr, MAXPTS,
     +                           stidnt, icorr, lunf, origin, iret )
                  ELSE
                    iret = -3
                END IF
C
C*                  Check for thunderstorm data following tropical data.
C
                IF ( trop ) THEN
                    ibeg = iptout
                    lens = nlen - ibeg + 1
                    CALL IS_PHEN ( report ( ibeg:nlen ), lens, 450,
     +                             phen2, iptr, iret )
                    IF ( iret .lt. 0 ) THEN
                        iret = 0
                      ELSE
                        IF ( phen2 .eq. 'TS' ) THEN
                            CALL IS_CNPH ( report, nlen, ibeg, ibegds,
     +                          iptr, npt, rlat, rlon, stidnt, icorr,
     +				lunf, 'TS', clat, clon, origin, irad,
     +				iret )
                        END IF
                    END IF
                END IF
            END IF
        END IF
C
 	CALL DC_FCLS ( ier )
C*
	RETURN
	END
