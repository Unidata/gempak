	SUBROUTINE WO_DCOD  ( curtim, gemfil, stntbl, prmfil, iadstn,
     +			      maxtim, nhours, iret )
C************************************************************************
C* WO_DCOD								*
C*									*
C* This subroutine decodes watch outline update reports, and writes the *
C* data to an ASCII file.						*
C*									*
C* WO_DCOD  ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,   *
C*	      IRET )    						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter packing table         *
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 2/03	Added a second check for 'TEST'		*
C* A. Hardy/NCEP	 3/03	Modified the second 'TEST' chk		*
C* B. Yin/SAIC           3/04  	Changed SS_GTIM to CSS_GTIM     	*
C* F. J. Yen/NCEP	 9/05	Quick fix to check for 'TEST...'	*
C* F. J. Yen/NCEP	10/06	Checked for false test ') TEST MESSAGE'	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	bultin*(DCMXBF),
     +			seqnum*4, wmohdr*8, oristn*8, bultim*12,
     +			bbb*8, errstr*80, work*(DCMXBF), origin*4,
     +			sysdt*12, dattmp*12, tissue*20
        CHARACTER       adstn(LLSTFL)*8, stnnam(LLSTFL)*32,
     +                  stat(LLSTFL)*2, coun(LLSTFL)*2,
     +                  tbchrs(LLSTFL)*20
        INTEGER         istnm(LLSTFL), ispri(LLSTFL), itype
        REAL            adlat(LLSTFL), adlon(LLSTFL), selv(LLSTFL)
	INTEGER		istarr (5), iotarr (5)
	LOGICAL		good
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.
C
	maxfil = 2
	iftype = 6
 	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
C
C*      Read the county table.
C
        CALL DC_STNS  ( stntbl, adstn, istnm, stnnam, stat, coun, adlat,
     +          adlon, selv, ispri, tbchrs, nade, ier )
        IF  ( ier .ne. 0 )  THEN
            RETURN
        END IF
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
		good = .true.
		CALL DC_TEST ( bultin, lenbul, itest, ier )
		IF ( itest .eq. 2 ) itest = 3
C
C*		Parse the header info from the bulletin.
C
		IF ( ifdtyp .eq. 0 ) THEN
		    CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +		                   oristn, bultim, bbb, nchar, ierr )
		  ELSE
		    ierr = -12
		END IF
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin (:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCWOU', 1, errstr, ier )
		    good = .false.
		END IF
C
		IF ( good ) THEN
		    origin = oristn ( :4 )
 	            IF ( ( bbb ( :3 ) .ne. 'COR' ) .and.
     +			 ( bbb ( :2 ) .ne. 'CC' ) ) THEN
			icorr = 0
		      ELSE
			icorr = 1
		    END IF
C
C*		    Set the bulletin pointer to one character past
C*		    the end of the WMO header.
C
		    ibpnt = nchar + 1
		    lenorg = lenbul
		    lenbul = lenbul - nchar
C
C*		    Remove unprintable characters.
C
		    CALL ST_UNPR ( bultin ( ibpnt:lenorg ), lenbul, 
     +				   work ( :lenbul ), lenb, ier )
C
C*		    DC_TEST will flag as a test if it finds 2 'TEST's.
C*		    The following will check other possibilities
C*		    that could be a test even if only one found.
C
		    IF ( itest .lt. 2 ) THEN
                        inx = (INDEX ( work(:lenb), '...TEST' ) )
                        inx2 = (INDEX ( work(:lenb), 'TEST ' ) )
		        IF ( inx2 .gt. 0 ) THEN
C
C*			    Check for the possible garbled data
C*			    with string ') TEST MESSAGE' 
C
			    indf = MAX ( 1, inx2 - 2)
			    IF ( INDEX (work(indf:lenb),
     +					') TEST MESSAGE') .ne. 0 )
     +				inx2 = 0
		        END IF
                        inx3 = (INDEX ( work(:lenb), 'TEST...' ) )
                        IF ( inx + inx2 + inx3 .gt. 0 ) itest = 3
		    END IF
                     
C
C*		    Get the system time, and make a standard GEMPAK time
C*		    from the "current" time.
C
		    itype = 1
		    CALL CSS_GTIM ( itype, sysdt, ier )
		    IF ( curtim .eq. 'SYSTEM' ) THEN
			dattmp = sysdt
		      ELSE
			CALL TI_STAN ( curtim, sysdt, dattmp, ier )
		    END IF
		    CALL TI_CTOI ( dattmp, istarr, ier )
C
C*		    Use the system time and the bulletin time to make a
C*		    Gempak time.
C
		    CALL ST_LSTR ( bultim, lens, ier )
		    IF ( lens .eq. 6 ) THEN
		        CALL ST_INTG ( bultim ( :6), issue, ierr )
			IF ( ierr .ne. 0 ) good = .false.
		      ELSE
			good = .false.
		    END IF
C
		    IF ( good ) THEN
	                irday  = issue / 10000
                        irhour = mod ( issue, 10000 ) / 100
                        irmin  = mod ( mod ( issue, 10000 ), 100 )
                        CALL DC_ITIM ( istarr, irday, irhour, irmin, 
     +				       iotarr, ier )
	                CALL TI_ITOC ( iotarr, tissue, ier )
		      ELSE
			CALL DC_WLOG ( 2, 'DC', -16, ' ', ier )
		    END IF
	        END IF
C
	        IF ( good ) THEN
C
C*		    Decode the watch outline update report.
C
		    CALL WO_DECD ( work ( :lenb ), lenb, iotarr,
     +				   icorr, itest, tissue, gemfil, stntbl,
     +				   iadstn, maxtim, origin, adstn, istnm,
     +                                    stnnam, stat, coun, adlat,
     +                                    adlon, selv, ispri, tbchrs,
     +                                    nade, ier )
		END IF
	    END IF
	END DO
C*
	RETURN
	END
