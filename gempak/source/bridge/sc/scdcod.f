	SUBROUTINE SC_DCOD  ( curtim, gemfil, stntbl, prmfil, 
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* SC_DCOD								*
C*									*
C* This subroutine decodes SCD bulletins and writes the data to a       *
C* GEMPAK surface file.							*
C*									*
C* SC_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*            IRET )		 					*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter packing table		*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	CIVALS(ICSTID)  CHAR*		Station id                      *
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 3/97	Based on MT_DCOD                        *
C* A. Hardy/GSC         12/97   Modified to use new interface           *
C* D. Kidwell/NCEP	 6/98	Added CIVALS to prologue; cleaned up    *
C* A. Hardy/GSC         10/98   Added sets for SLAT, SLON, SELV, CORN   *
C* D. Kidwell/NCEP	10/98	Added intf mnemonics to call sequences  *
C* A. Hardy/GSC          3/99   Added priority parameter to SF_QSTN     *
C* A. Hardy/GSC          3/99   Removed ispri = 0                       *
C* F. J. Yen/NCEP	 8/99	Replaced RA_RTIM with DC_GTIM		*
C* F. J. Yen/NCEP	11/99	Changed error code group 'RA' to 'DC'	*
C* D. Kidwell/NCEP	 9/02	Replaced SC_DTTM with BR_DTTM           *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C* S. Jacobs/NCEP	 8/04	Fixed declaration of station state	*
C* D. Kidwell/NCEP	11/04	Removed hardwired check for CXUS3       *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'sccmn.cmn'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	bultin*(DCMXBF), report*(DCMXBF),
     +			sysdt*12, dattmp*12, filnam*132, errstr*80,
     +			dattim*15, stid*8, sttmp*8, stat*2,
     +			parms(MMPARM)*4, strnew*80, ctmerr*8,
     +			bultim*12, bbb*8, wmohdr*8, oristn*8,
     +			seqnum*4, countr*2, cprms(MMPARM)*4,
     +			rimnem(NRIMN)*8, cimnem(NCIMN)*8 
	INTEGER		imnem (MMPARM),	istarr (5), irtarr (5)
	INTEGER		itype
        REAL		slat, slon, selv
	LOGICAL		more, good, corflg, addstn, cirflg, datflg,
     +			corbul, badtim, offtim
C*
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the pointers for the interface arrays.
C
	CALL SC_IFSP ( rimnem, cimnem, ierfsp )
	IF ( ierfsp .ne. 0 ) THEN
	    RETURN
	END IF
C
C*	Initalize the GEMPAK parameter array and set up links to 
C*	the interface mnemonics.
C
	CALL SC_INTF ( rimnem, cimnem, cprms, imnem, numprm, iret )
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.
C
	maxfil = 2
	iftype = 1
	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
	addstn = .true.
	cirflg = .false.
	iflsrc = 2 + MFTEXT 
C
C*	Loop until a timeout occurs.
C
	iperr = 0
	DO WHILE ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
C
C*		Parse the header info from the bulletin.
C
		more = .true.
		CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +		               oristn, bultim, bbb, nchar, ierr )
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin(:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCSCD', 2, errstr, ier )
		    more = .false.
		END IF
C
C*		Check for a correction as part of bulletin header.
C
		IF ( bbb ( 1:1 ) .eq. 'C' ) THEN
		    corbul = .true.
		  ELSE
		    corbul = .false.
		END IF
C
C*		Set pointer to skip over header.
C
		CALL ST_UNPR ( bultin(:nchar), nchar, strnew, ibpnt,ier)
C
C*		Parse bulletin time.
C
		CALL ST_LSTR ( bultim, lentim, ier )
		IF ( bultim ( lentim:lentim ) .ne. 'Z' ) 
     +		     lentim = lentim + 1
		CALL BR_DTTM ( bultim, lentim, kda, khr, kmin, ierr )
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'SC', ierr, bultim, ier )
		    errstr = wmohdr // oristn // bultim
		    CALL DC_WLOG ( 2, 'DCSCD', 3, errstr, ier )
		    more = .false.
		END IF
C
C*		Get the system time, and make a standard GEMPAK time
C*		from the "current" time.  Array istarr will be used as
C*		the receipt time.
C
		itype = 1
		CALL CSS_GTIM ( itype, sysdt, ier )
		IF ( curtim .eq. 'SYSTEM' )  THEN
		    dattmp = sysdt
		  ELSE
		    CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		END IF
		CALL TI_CTOI ( dattmp, istarr, ier )
C 
		IF ( ( nchar + 6 ) .gt. lenbul ) THEN
		    more = .false.
                  ELSE
                    ibpnt  = ibpnt + 9
		END IF
C
C*		Remove control characters from entire bulletin.
C
		CALL ST_UNPR ( bultin, lenbul, bultin, lenb, iret )
C
C*		Loop through the reports.
C
		DO WHILE ( more )
C
C*		    Get next report.
C
		    CALL SC_GRPT ( bultin, lenb, ibpnt, stid, report,
     +				   lenr, iret )
		    good  = .true.
		    corflg = corbul 
C
C*		    Check to see if a report was found.
C 
		    IF ( iret .ne. 0 ) THEN
			good = .false.
			IF ( iret .eq. ( -2 ) ) THEN
C
C*                          There are no more reports in this bulletin.
C
			    more = .false.
			END IF
		      ELSE
C
C*			Get the report time.
C
			CALL SC_RPTM ( report, irpnt, irday, irhour,
     +                                 irmin, iret )
		    END IF
C
C*		    Initialize report parameters to missing.
C
		    IF ( good ) THEN
		        CALL SC_INIT ( iret )
C
C*			Set off-time flag to false.
C
			offtim = .false.
C
C*		        Get the time to assign to this bulletin.
C*		    	Get observation time.  Array irtarr will be 
C*			used as the report time.  Note that hour
C*			returned in irtarr has been rounded up if
C*			irmin is greater than 44.
C
		    	CALL DC_GTIM  ( istarr, bultim, irhour, irmin,
     +					offtim, irtarr, dattim, ier1 )
			ihhmm = irhour * 100 + irmin
			badtim = .false.
C
			IF ( ier1 .ne. 0 ) THEN
			    CALL DC_WLOG ( 2, 'DC', ier1, ' ', ier )
			    badtim = .true.
			    CALL ST_INCH ( ihhmm, ctmerr, ier )
			  ELSE
C
C*		    	    Compute the difference between observation
C*		    	    and system times.
C
			    CALL TI_MDIF ( irtarr, istarr, imdif, ier2 )
C
C*		    	    Check that the time is within NHOURS before
C*		    	    the system time for GEMPAK.
C
 			    IF ( ( ier2 .ne. 0 ) .or. ( imdif .gt. 60 )
     +		  	         .or. ( imdif .lt. ((-60)*nhours) ) ) THEN
                 	        badtim = .true.
			        CALL ST_INCH ( imdif, ctmerr, ier )
			    END IF
			END IF
C
C*			Write an error message if the time is invalid.
C
			IF ( badtim ) THEN
			    good   = .false.
			    errstr = wmohdr // oristn // bultim
			    CALL DC_WLOG ( 2, 'DCSCD', 3, errstr, ier )
			    errstr = dattim // dattmp // ctmerr
			    CALL DC_WLOG ( 2, 'DCSCD', 4, errstr, ier )
			END IF
		    END IF
C
C*		    Open the output file.
C
		    IF ( good ) THEN
C
C*		        Make a file name from the template and the time.
C*		        Open the file as part of the open file list.
C
		        CALL FL_MNAM ( dattim, gemfil, filnam, ier )
		        CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn, 
     +			              maxtim, lunf, nparm, parms, ierr )
C
C*			Check that the file was opened properly.
C
			IF ( ierr .ne. 0 ) THEN
C
C*			    If not, write an error to the decoder
C*			    log file.
C
			    CALL DC_WLOG ( 0, 'SF', ierr, filnam, iret )
			END IF
		    END IF
C
C*		    Check for correction flag within report.
C
		    IF ( good .and. ( .not. corflg ) ) THEN
		        indxcr = INDEX ( report, ' COR ' )
			IF ( ( indxcr .gt. 7) .and. (indxcr .lt. 15 ) ) 
     +      		    corflg = .true.
		    END IF
C
C*		    Set the station and time in the output file.
C
		    IF ( good ) THEN
C
C*			Strip off leading 'K' for US report.
C
 			CALL ST_LSTR ( stid, lenid, ier )
 			IF ( lenid .eq. 4 .and. stid (1:1) .eq. 'K' )
     +			     THEN
      			    stid = stid( 2:4 )
			    civals ( icstid ) = stid
			END IF
			CALL RA_TMST ( lunf, dattim, stid, addstn,
     +				       cirflg, datflg, iret )
                        sttmp = stid
			CALL SF_QSTN  ( lunf, sttmp, istnm, slat,
     +				        slon, selv, ispri, stat, countr,
     +					 ier )
			IF ( ier .eq. 0 ) THEN
			    rivals ( irslat ) = slat
			    rivals ( irslon ) = slon
			    rivals ( irselv ) = selv
                        END IF
C
C*			Check for an error.
C
			IF ( iret .ne. 0 ) THEN
			    good = .false.
			    errstr = ' '
			    IF ( iret .eq. -4 ) errstr = dattim
			    IF ( iret .eq. -5 ) errstr = stid
			    CALL DC_WLOG ( 2, 'RA', iret, errstr, ier )
C
C*			    If the data has already been decoded and
C*			    this is not a correction, do not decode
C*			    again for GEMPAK.
C
			  ELSE IF ( datflg .and. ( .not. corflg )) THEN
			    good = .false.
			END IF
		    END IF
C
C*		    Decode the data and write to the surface file.
C
		    IF ( good ) THEN
                        IF ( corflg ) THEN
                            rivals ( ircorn ) = 1.
                          ELSE
                            rivals ( ircorn ) = 0.
                        END IF 
C
			CALL SC_DECD ( report, lenr, irpnt, iret )
			IF ( iret .eq. 0 ) THEN
C
C*			    Write decoded values to the decoder log.
C
 			    CALL SC_IFPT ( lenr, report, ihhmm, 
     +					   rimnem, cimnem, ier )
C
C*			    Write report to GEMPAK file.
C
			    IF ( good ) THEN 
			        CALL SC_GEMP ( lunf, ihhmm, cprms,
     +				     parms, nparm, numprm, imnem, jret )
				CALL SF_WSTR ( lunf, ihhmm, report, ier)
			    END IF
			END IF
		    END IF
		END DO
	      ELSE
C
C*		Write an error to the decoder log file.
C
		CALL DC_WLOG ( 0, 'DC', iperr, ' ', iret )
	    END IF
	END DO
C
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
