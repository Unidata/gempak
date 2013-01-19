	SUBROUTINE IS_DECD ( report, lenr, iotarr, icorr, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, origin,
     +			     iret )
C************************************************************************
C* IS_DECD 								*
C*									*
C* This subroutine decodes a single international sigmet report from	*
C* the U. S.							        *
C*                                                                      *
C* IS_DECD ( REPORT, LENR, IOTARR, ICORR, TISSUE, GEMFIL, STNTBL,       *
C*	     IADSTN, MAXTIM, ORIGIN, IRET )                             *
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
C*	ORIGIN		CHAR*		Originating station id.		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not a sigmet    *
C*					 -2 = format error in preamble  *
C*					 -3 = no phenomenon found       *
C*					 -4 = bad area definition       *
C*					-10 = no storm center found     *
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	10/99	                                        *
C* D. Kidwell/NCEP	10/99	Included center values in arg. lists    *
C* D. Kidwell/NCEP	11/99	Added trop. depression, trop. storm     *
C* F. J. Yen/NCEP	 4/00	Added volcanic ash.			*
C* A. Hardy/GSC          7/00   Added tropical cyclone- 'TC'		*
C* A. Hardy/GSC          9/00   Changed call sequence to ISTC           *
C* D. Kidwell/NCEP	10/00	Corrected pointers for TS after TC      *
C* A. Hardy/GSC         12/00   Added TD for IS_TC; moved IS_HURC       *
C* F. J. Yen/NCEP	11/01	Changed prologue for U. S. only.	*
C*				Added decoding of 'CT'			*
C* F. J. Yen/NCEP	 6/02	Changed length of phen2 from 4 to 2	*
C* A. Hardy/NCEP	 9/02   Added originating station id		*
C* F. J. Yen/NCEP	 9/03	Added decoding of 'CB'.  Added check	*
C*				for return code from IS_TC and IS_HURC.	*
C* F. J. Yen/NCEP	11/03	Updated for CSC change in IS_PHEN.	*
C* F. J. Yen/NCEP	12/03	Replaced IS_HURC and IS_TC with IS_TCHU.*
C* F. J. Yen/NCEP	 2/04	Added decoding of 'IC', 'SQ', and 'MW'.	*
C* T. Piper/SAIC	10/05	If origin = KKCI, change to locid	*
C* J. Lewis/AWC          8/07   Increment icorr based on sequence number*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, origin
	INTEGER		iotarr (*)
C*
	PARAMETER	( MAXPTS = 20 )
	CHARACTER	parms (MMPARM)*4, filnam*132, stidnt*50,
     +			locid (4)*8, mwoid*8, phenom*2, remark*500,
     +			endpr*5, phen2*2, carr (6)*20
	INTEGER		iflvl (1)
	REAL		rlat (MAXPTS), rlon (MAXPTS)
	LOGICAL		proces, trop
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that report is upper case.
C
        trop = .false.
	CALL ST_LCUC ( report, report, ier )
C
C*	Process the preamble section.
C
	CALL IS_PRMB ( report, lenr, iotarr, locid, nloc, stidnt,
     +		       mwoid, icancl, iret )
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
	IF ( origin .eq. 'KKCI' )  origin = locid(1)
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
C*	    Check for an update
C
	    CALL ST_CLST ( stidnt, '|', ' ', 4, carr, num, ier )
	    IF ( num .EQ. 4 ) THEN
	        CALL ST_LSTR ( carr ( num ), lens, ier )
	        CALL ST_INTG ( carr ( num ) ( :lens ), iseqnum, ier )
	        IF ( iseqnum .GT. 1 ) icorr = icorr + 1
	    ELSE
	      RETURN
	    ENDIF
		
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
C*	Look for the phenomenon being reported.
C
	IF ( proces ) THEN
	    ibeg = ibeg + 5
	    lens = lenr - ibeg + 1
	    CALL IS_PHEN ( report ( ibeg:lenr ), lens, 120, phenom,
     +			   iptr, iret )
	    IF ( iret .ge. 0 ) THEN
C
C*	        Continue decoding based on type of phenomenon.
C
	        IF ( phenom .eq. 'TS' .or. phenom .eq. 'CT' .or.
     +			phenom .eq. 'CB' ) THEN
	            CALL IS_TS ( report, lenr, ibeg, iptr, MAXPTS, 
     +				 stidnt, icorr, lunf, phenom, clat,
     +				 clon, origin, iret )
	          ELSE IF ( ( phenom .eq. 'TB' ) .or.
     +			    ( phenom .eq. 'SQ' ) .or.
     +			    ( phenom .eq. 'IC' ) .or.
     +			    ( phenom .eq. 'MW' ) ) THEN
	            CALL IS_TURB ( report, lenr, ibeg, iptr, MAXPTS,
     +			    stidnt, icorr, lunf, phenom, origin, iret )
	          ELSE IF ( ( phenom .eq. 'HU' ) .or.
     +                      ( phenom .eq. 'TC' ) .or.
     +                      ( phenom .eq. 'TD' ) .or.
     +			    ( phenom .eq. 'TR' ) ) THEN
	            CALL IS_TCHU ( report, lenr, ibeg, iptr, phenom,
     +				   stidnt, icorr, lunf, origin, iptout,
     +				   clat, clon, iret )
		    IF ( iret .ne. 0 ) THEN
			trop = .false.
		      ELSE
			trop = .true.
		    END IF
	          ELSE IF ( phenom .eq. 'VA' ) THEN
	            CALL IS_VA ( report, lenr, ibeg, iptr, MAXPTS, 
     +				 stidnt, icorr, lunf, origin, iret )
	          ELSE
		    iret = -3
		END IF
C
C*	            Check for thunderstorm data following tropical data.
C
                IF ( trop ) THEN
	            ibeg = iptout
	            lens = lenr - ibeg + 1
	            CALL IS_PHEN ( report ( ibeg:lenr ), lens, 120, 
     +				   phen2, iptr, iret )
	            IF ( iret .lt. 0 ) THEN
		        iret = 0
	              ELSE
	                IF ( phen2 .eq. 'TS' ) THEN
	                    CALL IS_TS ( report, lenr, ibeg, iptr, 
     +					 MAXPTS, stidnt, icorr, lunf,
     +					 'TS', clat, clon, origin, 
     +                                    iret )
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
