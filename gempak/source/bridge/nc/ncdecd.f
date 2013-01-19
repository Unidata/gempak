	SUBROUTINE NC_DECD ( report, lenr, iotarr, icorr, itest, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, org, iret )
C************************************************************************
C* NC_DECD 								*
C*									*
C* This subroutine processes a single non-convective sigmet bulletin.   *
C*                                                                      *
C* NC_DECD ( REPORT, LENR, IOTARR, ICORR, ITEST, TISSUE, GEMFIL, STNTBL,*
C*	     IADSTN, MAXTIM, ORG, IRET )                                *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Sigmet bulletin                 *
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	ICORR		INTEGER		Correction flag                 *
C*	ITEST		INTEGER		Test flag                       *
C*	TISSUE		CHAR*  		Bull. issue time, GEMPAK format *
C*	GEMFIL		CHAR*		Output file name template       *
C*	STNTBL		CHAR*		Station table                   *
C*	IADSTN		INTEGER		Number of additional stations   *
C*	MAXTIM 		INTEGER		Number of hours prior to CURTIM *
C*	ORG		CHAR*    	Originating station             *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 8/00	                                        *
C* D. Kidwell/NCEP	 4/03	Added check for ierr = 1 from NC_PHEN	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl, org
	INTEGER		iotarr (*)
C*
	PARAMETER	( MAXPTS = 40 )
	CHARACTER	parms (MMPARM)*4, filnam*132,
     +			bbb*3, strtim*20, stptim*20, updt*2,
     +			strbuf*160, string*(DCMXBF), type*2,
     +			flvl (2)*4, sname*12
	LOGICAL		good
	REAL		rlat (MAXPTS), rlon (MAXPTS)
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
	strbuf = report ( :160 )
        iflsrc = 2
	CALL NC_HDR ( strbuf, org, iotarr, strtim, stptim, updt,
     +		      sname, bbb, iptr, ierr )
	IF ( ierr .eq. 0 ) THEN
	    IF ( icorr .eq. 0 ) THEN
                IF ( ( bbb  .eq. 'COR' ) .or.
     +               ( bbb ( :2 ) .eq. 'CC' ) ) THEN
                    icor = 1
                  ELSE IF ( bbb .eq. 'AMD' ) THEN
                    icor = 2
		  ELSE
		    icor = 0
                END IF
	      ELSE
	        icor = icorr
	    END IF
C
	  ELSE
	    CALL DC_WLOG ( 2, 'DCNCON', ierr, strbuf ( :72 ), ier )
	    RETURN
        END IF
C
	good = .true.
	IF ( bbb ( :2 ) .ne. 'CN' ) THEN
C
C*	    Get sigmet bounds.
C
	    ibeg   = iptr + 4
	    string = report ( ibeg: )
	    CALL NC_BNDS ( string, MAXPTS, npt, rlat, rlon, iptr, ier )
	    IF ( ier .eq. 0 ) THEN
	        icor   = icor + itest
	        string = string ( iptr: )
	        CALL ST_LSTR ( string, lens, ier )
C
C*		Get sigmet type.
C
		CALL NC_PHEN ( string, lens, type, iptr, ierr )
		IF ( ierr .eq. 0 ) THEN
	    	    iend   = MIN ( iptr + 159, lens )
	    	    strbuf = string ( iptr:iend )
C
C*		    Get flight level(s).
C
 		    CALL AM_FLVL ( strbuf, flvl, jptr, ier )
		  ELSE IF ( ierr .eq. 1 ) THEN
		    flvl ( 1 ) = ' '
		    flvl ( 2 ) = ' '
	          ELSE
		    good = .false.
		    ier  = ierr
		END IF
	      ELSE
		good = .false.
	    END IF
	  ELSE
C
C*	    This is a cancellation.
C
	    flvl ( 1 ) = ' '
	    flvl ( 2 ) = ' '
	    type       = 'CN'
	    npt        = 0
        END IF
C
	IF ( good ) THEN
C
C*	    Make a file name from the template and the 
C*	    time.  Open the file as part of the open 
C*	    file list.
C
 	    CALL FL_MNAM ( tissue, gemfil, filnam, ier )
 	    CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, maxtim,
     +	                   lunf, nparm, parms, ier )
	    CALL NC_OUT ( lunf, type, strtim, stptim, sname,
     +			  flvl, icor, rlat, rlon, npt, ier )
	  ELSE
	    CALL DC_WLOG ( 2, 'DCNCON', ier, string ( :72 ), ier1 )
	END IF
C
 	CALL DC_FCLS ( ier )
C*
	RETURN
	END
