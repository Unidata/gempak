	SUBROUTINE IS_OTHR ( report, lenr, iotarr, icorr, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, origin,
     +			     astid, slat, slon, nstn, iret )
C************************************************************************
C* IS_OTHR 								*
C*									*
C* This subroutine decodes a single generic international sigmet report.*
C* Generic reports are those that are neither decoded by IS_DECD nor	*
C* IS_EGGY.								*
C*                                                                      *
C* IS_OTHR ( REPORT, LENR, IOTARR, ICORR, TISSUE, GEMFIL, STNTBL,       *
C*	     IADSTN, MAXTIM, ORIGIN, ASTID, SLAT, SLON, NSTN, IRET )    *
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
C*	ORIGIN		CHAR*		Originating station		*
C*	ASTID (*)	CHAR*		Station names from stntbl	*
C*	SLAT (*)	REAL		Latitudes from stntbl		*
C*	SLON (*)	REAL		Longitudes from stntbl		*
C*	NSTN		INTEGER		Number of table entries		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = report is not a sigmet    *
C*					 -2 = format error in preamble  *
C*					 -3 = no phenomenon found       *
C*					 -8 = no hyphen	after preamble	*
C*					 -9 = mwo id not found in table *
C*					-10 = no storm center found     *
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	11/01	Created from IS_DECD			*
C* F. J. Yen/NCEP	12/01	Fixed starting index for station search *
C* F. J. Yen/NCEP	 2/02	Use origin lat/lon if prfxid not in tbl *
C* A. Hardy/NCEP	 9/02   Added origin to IS_OUT			*
C* F. J. Yen/NCEP	10/03	Updated call to IS_PHEN for CSC.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, origin
	CHARACTER*(*)	astid (*)
	INTEGER		iotarr (*)
	REAL		slat (*), slon (*)
C*
	CHARACTER	parms (MMPARM)*4, filnam*132, stidnt*55,
     +			locid (4)*8, mwoid*8, phenom*2, remark*500,
     +			endpr*6, prfxid*4
	LOGICAL		proces, found, hyphen
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
C*	Process the preamble section.
C
	CALL IS_OTPR ( report, lenr, iotarr, origin, locid, nloc,
     +		       stidnt, mwoid, hyphen, prfxid, icancl, iret )
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
	IF ( icancl .eq. 1 ) THEN
C 
C*	    Report is a sigmet cancellation.  Save remarks, if any.
C
	    ibeg = INDEX ( report ( :lenr ), 'UTC' ) + 4
	    IF ( ibeg .eq. 4 ) ibeg = INDEX ( report (:lenr ), '-' ) + 2
	    iend = INDEX ( report ( ibeg:lenr ), '.' ) + ibeg - 1
	    IF ( iend .gt. ibeg ) remark = report ( ibeg:iend )
	    CALL IS_OUT ( lunf, 'CN', stidnt, origin, icorr,
     +			  IMISSD, IMISSD, IMISSD, IMISSD,
     +			  ' ', IMISSD, rlat, rlon, 0, iret )
	    proces = .false.
	  ELSE

C
C*	    Continue processing sigmet.
C
	    endpr = mwoid ( :4 ) // '-'
	    lenpr = min ( lenr, 60 )
	    ibeg  = INDEX ( report ( :lenpr ), endpr ( :5 ) )
  	    IF ( ibeg .eq. 0 ) THEN
  		endpr = mwoid ( :4 ) // ' -'
  		ibeg = INDEX ( report ( :lenpr ), endpr ( :6 ) )
		IF ( ibeg .eq. 0 ) THEN
	            IF ( .not. hyphen ) THEN
		        endpr = mwoid ( :4 )
		        ibeg = INDEX ( report ( :lenpr ), endpr ( :4 ) )
		        IF ( ibeg .eq. 0 ) THEN
	                    iret   = -2
	                    proces = .false.
			END IF
		    END IF
		END IF
  	    END IF
	END IF
C
C*	Look for the phenomenon being reported.
C
	IF ( proces ) THEN
	    ibeg = ibeg + 5
	    lens = lenr - ibeg + 1
C
C*	    Set lens to negative value since "other" rpt.
C
	    lens = - lens
	    CALL IS_PHEN ( report ( ibeg:lenr ), lens, 120, phenom,
     +			   iptr, iret )
	    IF ( iret .ge. 0 ) THEN
C
C*		Get lat/lon for prfxid which has the MWO ID or 
C*		originating station if MWO ID is missing.
C
		ij = 0
		found = .false.
		DO WHILE ( .not. found .and. ij .lt. nstn )
		    ij = ij + 1
		    IF ( prfxid .eq. astid (ij) ) THEN
			rlat = slat (ij)
			rlon = slon (ij)
			found = .true.
		    END IF 
		END DO
		npt = 1
C
C*		Get lat/lon for origin (originating station) if
C*		prfxid is not in stntbl 
C
		IF ( .not. found .and. prfxid .ne. origin ) THEN
		    ij = 0
		    DO WHILE ( .not. found .and. ij .lt. nstn )
		        ij = ij + 1
			IF ( origin .eq. astid (ij) ) THEN
			    rlat = slat (ij)
			    rlon = slon (ij)
			    found = .true.
			END IF
		    END DO
		END IF	
		IF ( .not. found ) THEN
		    iret = -9	    
		  ELSE
C
C*	            Write out data for generic report
C
	            CALL IS_OUT ( lunf, phenom, stidnt, origin, icorr,
     +			  IMISSD, IMISSD, IMISSD, IMISSD,
     +			  'OTHER', IMISSD, rlat, rlon, npt, ier )
		END IF
	    END IF
	END IF
C
 	CALL DC_FCLS ( ier )
C*
	RETURN
	END
