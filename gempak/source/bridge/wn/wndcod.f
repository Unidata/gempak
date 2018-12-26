	SUBROUTINE WN_DCOD ( curtim, gemfil, stntbl, prmfil,
     +			      iadstn, maxtim, nhours, iret )
C************************************************************************
C* WN_DCOD								*
C*									*
C* This subroutine decodes tornado, thunderstorm, snow squall, and      * 
C* flash flood warning bulletins and writes the data to an ASCII file.  *
C*									*
C* WN_DCOD ( CURTIM, GEMFIL, STNTBL, PRMFIL, IADSTN, MAXTIM, NHOURS,	*
C*		IRET )							*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	STNTBL		CHAR*		Station table			*
C*	PRMFIL		CHAR*		Parameter table file name	*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					 -4 = Unable to get bultim	*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          5/99						*
C* A. Hardy/GSC          5/99	Added error check for dattim		*
C* A. Hardy/GSC          5/99	Added wtype to if statements		*
C* A. Hardy/GSC          6/99	Reworked and cleaned up			*
C* A. Hardy/GSC          2/00	Increased cnties 65->450		*
C* A. Hardy/GSC          2/00	Added check for header error		*
C* D. Kidwell/NCEP	 3/00	Added check for TEST bulletins		*
C* A. Hardy/GSC          3/00	Using bultim to create filename		*
C* A. Hardy/GSC          5/00	Added DC_GHDR call			*
C* A. Hardy/GSC         11/00   Changed FFLD,TORN,STSM -> STFS  	*
C* A. Hardy/GSC         12/00   Cleaned up ierr values			*
C* F. J. Yen/NCEP	 1/01	Replaced WN_RDTB with DC_STNS		*
C* F. J. Yen/NCEP	 3/02	Checked issue time & added DC_WLOG call.*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM		*
C* D. Kidwell/NCEP	11/05	Improved error checks			*
C* T. Piper/SAIC	06/07	Extract warning polygon from bulletin	*
C* T. Piper/SAIC	08/07	Format change; ignore TIME...MOT...LOC	*
C* F. J. Yen/NCEP	 3/08	Added ETN; made polygon search robust;  *
C*				added correction letter to cor/test flag*
C* S. Guan/NCEP         04/15   Increased sections (starry) from 5 to 15*
C* S. Guan/NCEP         11/17   Modified to add snow squall warn (SQW)  *
C* S. Guan/NCEP         05/18   Initialized bultin                      *   
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	curtim, gemfil, prmfil, stntbl
C*
	CHARACTER	bultin*(DCMXBF), parms(MMPARM)*4, errstr*80,
     +			starry(15)*2800, filnam*132, wtype*3,
     +			wfrom*3, dattim*11, bultim*12, wmohdr*8,
     +			oristn*8, seqnum*4, cnties*700, edttim*6,
     +			sysdt*12, dattmp*12, county(LLSTFL)*6,
     +			strtim*11, stptim*11, bbb*4, poly*512, etn*4
	CHARACTER	adstn(LLSTFL)*8, stnnam(LLSTFL)*32,
     +			stat(LLSTFL)*2, coun(LLSTFL)*2,
     +			tbchrs(LLSTFL)*20
	INTEGER	istarr (15), irdtar (15)
	INTEGER	istnm(LLSTFL), ispri(LLSTFL), itype
	REAL		adlat(LLSTFL), adlon(LLSTFL), selv(LLSTFL)
	LOGICAL		more
C------------------------------------------------------------------------
	iret = 0
C
C*  Initialize open file lists. Set the max number of open files.
C*  Set the type of output file and file source.
C
	maxfil = 2
	iftype = 6
	CALL DC_FINT ( maxfil, iftype, prmfil, ier )
	iflsrc = 2 
C
C*  Read the county table.
C
	CALL DC_STNS  ( stntbl, adstn, istnm, stnnam, stat, coun,
     +			adlat, adlon, selv, ispri, tbchrs, nade, ier )
	IF  ( ier .ne. 0 )  THEN
	    RETURN
	END IF
C
C*  Loop until a timeout occurs.
C
	iperr = 0
	DO WHILE ( iperr .eq. 0 )
	    icor = 0
C
C* Initialize bultin
C
            DO I = 1, DCMXBF
               bultin(I:I) =' '
            END DO 
C
C*  Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
	    IF ( iperr .eq. 0 ) THEN
		more = .true.
		CALL DC_TEST ( bultin, lenbul, itest, ier )
C
C*  Break bulletin into 15 strings.
C
		CALL ST_CLSL ( bultin, '*', ' ', 15, starry, inumb,
     +								ierr )
C
C*  Parse the header info from the bulletin.
C
		CALL DC_GHDR ( bultin, lenbul, seqnum, wmohdr,
     +			  oristn, bultim, bbb, nchar, ierr )
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    more = .false.
		END IF
C
C*              To handle partial cancellation with two VETC lines     
C*              nchar indicates whether there is more than one vtec exist
C
                nchar = 1
                iierr = 0
                DO WHILE ( ( nchar .gt. 0 ) .and. ( iierr .eq. 0 ) )
                  IF ( nchar .gt. 1) THEN
                     islash = INDEX (bultin, '$$')
                     bultin = bultin(islash:lenbul)
                     CALL ST_CLSL ( bultin, '*', ' ', 15, starry,
     +                                               inumb, ierr )
                  END IF
		  IF ( more ) THEN
		    CALL WN_GHDR ( starry(1), icor, wtype, wfrom,
     +			           cnties, edttim, nchar, etn, iierr )
		    IF (  bbb .eq. 'COR'  ) THEN
			icor = 1
		      ELSE IF ( bbb(1:2) .eq. 'CC' ) THEN
C 
C*			bbb(3:3) can be from 'A' through 'Z'.
C*			So icor will be from 65 through 90.
C
			icor = ichar (bbb(3:3))
		    END IF
		    IF ( ierr .lt. 0 ) THEN
			CALL ST_UNPR ( bultin(:80), 80, errstr, len1,
     +				       ier )
      			CALL DC_WLOG ( 2, 'DCWARN', ierr,
     +					errstr(:len1), ier )
			more = .false.
		    END IF
		    IF ( inumb .ge. 1 .and. inumb.le. 15 ) THEN
C*
C*			The polygon should be in the fifth (last),
C*			section.  If there is no fifth section, then
C*			search for the polygon in the last section. 
C*			 
		        CALL ST_LSTR(starry(inumb), lens, ier)
		        CALL ST_UNPR(starry(inumb), lens, starry(inumb),
     +				     lenout, ier)
		        ibeg = INDEX(starry(inumb), 'LAT...LON')
		        IF ( ibeg .gt. 0 )  THEN
			    ibeg = ibeg + 10
			    iend = INDEX(starry(inumb)(ibeg:), '$$')
			    IF ( iend .gt. 0 )  THEN
			        iend = ibeg + iend - 3
			        itim = INDEX(starry(inumb)(ibeg:iend),
     +					'TIME...MOT...LOC')
			        IF (itim .gt. 0) iend = ibeg + itim - 3
			        poly = starry(inumb)(ibeg:iend)
			    ELSE
				ierr = 4
				mnln = MIN (lens,48)
			        CALL DC_WLOG ( 2, 'DCWARN', ierr,
     +					starry(inumb)(:mnln), ier )
			        more = .false.
			    END IF
		        ELSE
			    ierr = 4
			    mnln = MIN (lens,48)
			    CALL DC_WLOG ( 2, 'DCWARN', ierr,
     +					starry(inumb)(:mnln), ier )
			    more = .false.
		        END IF
		    END IF
		  END IF
C
		IF ( more ) THEN
C
C*		   For icor values .GE. 65, then the correction letter in
C*		   the bbb field was used in icor, so add 256 to it if it
C*		   is a test, else add 2 to make it compatible with the
C*		   old way that didn't use the bbb field.
C
		    IF ( itest .eq. 2 ) THEN
			IF ( icor .ge. 65 ) THEN
		            itest = 256
			END IF
		    END IF
		    icor = icor + itest
C
C*  Get the system time, and make a standard GEMPAK time from the
C*  "current" time.
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
C*  Set the GEMPAK date/time.  Use bultim to set the output file name.
C
		    CALL ST_LSTR ( bultim, len, ier )
		    IF ( len .ne. 6 ) THEN
			IF ( curtim .eq. 'SYSTEM' ) THEN
			    bultim = dattmp( 5:6 ) // dattmp( 8:11 )
			ELSE
			    iret = -4
			    CALL ST_UNPR ( bultin (:80), 80, errstr,
     +					   len1, ier )
			    CALL DC_WLOG ( 2, 'DCWARN', iret,
     +					   errstr(:len1), ier )
			    more = .false.
			END IF
		    END IF
		    IF ( more ) THEN
			CALL ST_NUMB ( bultim, issue, ier )
			irday  = issue / 10000
			irhour = mod ( issue, 10000 ) / 100
			irmin  = mod ( mod ( issue, 10000 ), 100 )
			CALL DC_ITIM ( istarr, irday, irhour, irmin,
     +				   irdtar, ier )
C
C*  If invalid values in issue time, use system or current time.
C
		        CALL TI_DAYM ( irdtar(1), irdtar(2), ndays,
     +				ier )
			IF ( irday  .lt. 1 .or. irday  .gt. ndays .or. 
     +			     irhour .lt. 0 .or. irhour .gt. 23    .or.
     +			     irmin  .lt. 0 .or. irmin  .gt. 59 )  THEN
			    irday  = istarr(3)
			    irhour = istarr(4)
			    irmin  = istarr(5)
			END IF
			CALL DC_ITIM ( istarr, irday, irhour, irmin,
     +                             irdtar, ier )
			CALL TI_ITOC ( irdtar, dattim, ier )
			IF ( ier .lt. 0 ) more = .false.
C
C*  Remove control characters from entire bulletin.
C
		        IF ( more ) THEN 
C
C*  Set bultim from dattim, in case original value was invalid.
C
			    bultim = dattim( 5:6 ) // dattim( 8:11 )
			    CALL ST_UNPR ( bultin, lenbul, bultin, 
     +					lenb, ier )
C
C*  Make a file name from the template and the time.  Open the file as
C*  part of the open file list.
C
			    CALL FL_MNAM ( dattim, gemfil, filnam, ier )
			    CALL DC_FCYL ( filnam, iflsrc, stntbl,
     +				    iadstn, maxtim, lunf, nparm,
     +				    parms, ierr )
C
C*  Check that the file was opened properly.
C
			    IF ( ierr .ne. 0 ) THEN
C
C*  If not, write an error to the decoder log file.
C
				CALL DC_WLOG ( 0, 'SF', ierr, filnam,
     +					ier )
				more = .false.
			    END IF
C
			    IF ( more ) THEN
				IF ( ( wtype .eq. 'SVR' ) .or.
     +					( wtype .eq. 'TOR' ) .or.
     +                                  ( wtype .eq. 'SQW' ) .or.
     +					( wtype .eq. 'FFW' ) ) THEN
				    IF ( wtype .eq. 'FFW' ) THEN
					minalw = 480
					mindif = 360
				    ELSE
					minalw = 240
					mindif = 120
				    END IF
				    CALL WN_STFS ( bultim, istarr,
     +					      edttim, cnties, minalw,
     +					      mindif, bultin, 'DCWARN',
     +					      strtim, stptim, county,
     +					      numcty, iret )
				ELSE
				    more = .false.
				END IF
				IF ( ( wtype  .eq. ' ' ) .or.
     +				     ( strtim .eq. ' ' ) .or.
     +				( stptim .eq. ' ' ) )
     +				more = .false.
				IF ( more ) THEN
				   CALL WN_OUT (lunf, wtype, strtim,
     +					  stptim, oristn, icor, etn,
     +					  poly, county, numcty, adstn,
     +					  istnm, stnnam, stat, coun,
     +					  adlat, adlon, selv, ispri,
     +					  tbchrs, nade, iret )
				END IF
			    END IF
			END IF
		    END IF
		END IF 
              END DO
	      CALL DC_FCLS ( ier )
	    END IF
	END DO
C
	RETURN
	END
