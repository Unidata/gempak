	SUBROUTINE FA_DCOD  ( curtim, gemfil, stntbl, stntb2, prmfil,
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* FA_DCOD								*
C*									*
C* This subroutine decodes flash flood watch reports, and writes the    *
C* data to an ASCII file.						*
C*									*
C* FA_DCOD  ( CURTIM, GEMFIL, STNTBL, STNTB2, PRMFIL, IADSTN, MAXTIM,   *
C*	      NHOURS, IRET )						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	STNTB2		CHAR*		Second station table		*
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
C* H. Zeng/SAIC		07/05	Initial coding				*
C* F. J. Yen/NCEP	04/06   Write log only if AWIPS ID of FFA.    	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, stntb2, prmfil
C*
	CHARACTER	bultin*(DCMXBF),
     +			seqnum*4, wmohdr*8, oristn*8, bultim*12,
     +			bbb*8, errstr*80, work*(DCMXBF), origin*4,
     +			sysdt*12, dattmp*12, tissue*20
        CHARACTER       adstn(LLSTFL)*8, stnnam(LLSTFL)*32,
     +                  stat(LLSTFL)*2, coun(LLSTFL)*2,
     +                  tbchrs(LLSTFL)*20, parms(MMPARM)*4
        CHARACTER       adstn2(LLSTFL)*8, stnna2(LLSTFL)*32,
     +                  stat2(LLSTFL)*2, coun2(LLSTFL)*2,
     +                  tbchr2(LLSTFL)*20, filnam*132
        INTEGER         istnm(LLSTFL), ispri(LLSTFL)
        INTEGER         istnm2(LLSTFL), ispri2(LLSTFL)
        REAL            adlat(LLSTFL), adlon(LLSTFL),
     +			selv(LLSTFL)
        REAL            adlat2(LLSTFL), adlon2(LLSTFL),
     +			selv2(LLSTFL)
	INTEGER		istarr (5), iotarr (5), itype
	LOGICAL		good, isffa
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file and file source.
C
	maxfil = 2
	iftype = 6
 	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
	iflsrc = 2 
C
C*      Read the zone table.
C
        CALL DC_STNS  ( stntbl, adstn, istnm, stnnam, stat, coun, adlat,
     +          adlon, selv, ispri, tbchrs, nade, ier )
        IF  ( ier .ne. 0 )  THEN
            RETURN
        END IF
C
C*      Read the county table.
C
        CALL DC_STNS  ( stntb2, adstn2, istnm2, stnna2, stat2, coun2, 
     +	                adlat2, adlon2, selv2, ispri2, tbchr2, nade2, 
     +			ier )
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
		isffa = .true.
C
C*		Parse the header info from the bulletin.
C
		IF ( ifdtyp .eq. 0 ) THEN
		    CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +		                   oristn, bultim, bbb, nchar, ierr )
		ELSE
		    ierr = -12
		END IF
C
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin (:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCFFA', 1, errstr, ier )
		    good = .false.
		ELSE
C
 	            IF ( ( bultin( nchar+1:nchar+3 ) .ne. 'FFA' ) .or.
     +			 ( bultin( nchar+1:nchar+6 ) .eq. 'FFASPN' )
     +			   ) THEN
		        good = .false.
		        isffa = .false.
		    END IF
C
		END IF
C
		IF ( good ) THEN
		    origin = oristn ( :4 )
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
		    IF ( lens .ne. 6 ) THEN
			IF ( curtim .eq. 'SYSTEM' ) THEN
			    bultim = dattmp ( 5:6 ) // dattmp ( 8:11 )
			ELSE
			    good = .false.
			END IF
C
		    END IF
C
	        END IF
C
		IF ( good ) THEN
		    CALL ST_INTG ( bultim ( :6), issue, ierr )
	            irday  = issue / 10000
                    irhour = mod ( issue, 10000 ) / 100
                    irmin  = mod ( mod ( issue, 10000 ), 100 )
                    CALL DC_ITIM ( istarr, irday, irhour, irmin, 
     +				   iotarr, ier )
	            CALL TI_ITOC ( iotarr, tissue, ier )
		ELSE
		    IF ( isffa ) THEN
		        CALL DC_WLOG ( 2, 'DC', -16, ' ', ier )
		    END IF
		END IF
C
C
		IF ( good ) THEN 
C
C*		    Set bultim from tissue, in case original
C*		    value was invalid.
C
		    bultim = tissue ( 5:6 ) // tissue ( 8:11 )
C
C*		    Make a file name from the template and the
C*	            time.  Open the file as part of the open
C*		    file list.
C
 		    CALL FL_MNAM ( tissue, gemfil, filnam, ier )
 		    CALL DC_FCYL ( filnam, iflsrc, stntbl,
     +				   iadstn, maxtim, lunf, nparm,
     +				   parms, ierr )
C
C*	            Check that the file was opened properly.
C
		    IF ( ierr .ne. 0 ) THEN
C
C*	    	        If not, write an error to the decoder 
C*			log file.
C
	    	        CALL DC_WLOG ( 0, 'DC', ierr, filnam,
     +				       ier )
                        good = .false.
		    END IF
C
		END IF
C
	        IF ( good ) THEN
C
C*		    Decode the flash flood watch report.
C	
		    ier = 0	
		    CALL FA_DECD ( work(:lenb), tissue, origin,
     +				   adstn, adstn2, istnm, istnm2, 
     +				   stnnam, stnna2, stat, stat2, 
     +				   coun, coun2, adlat, adlat2, 
     +			           adlon, adlon2, selv, selv2, 
     +				   ispri, ispri2, tbchrs, tbchr2,
     +                             nade, nade2, lunf, bultim, ier )
		END IF
 	        CALL DC_FCLS ( ier )
	    END IF
	END DO
C*
	RETURN
	END
