	SUBROUTINE RD_DCOD ( curtim, gemfil, prmfil, stntbl, iadstn,
     +			     maxtim, nhours, iret )
C************************************************************************
C* RD_DCOD								*
C*									*
C* This routine will decode the Regional Digital Forecast (RDF) product *
C* to a GEMPAK surface file.						*
C*									*
C* RD_DCOD ( CURTIM, GEMFIL, PRMFIL, STNTBL, IADSTN, MAXTIM,		*
C*	    NHOURS, IRET )						*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	PRMFIL		CHAR*		Parameter packing table		*
C*	STNTBL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* F. J. Yen/NCEP	 9/02						*
C* F. J. Yen/NCEP	11/02	Removed unused date; check return from	*
C*				second call to decode forecast times.	*
C*				Cleaned up; added new parameters; if no *
C*				rept issue time use bulletin issue time;*
C*				enhanced log messages.			*
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	PARAMETER       ( IFCSTM = 41 )
	PARAMETER       ( NUMNAM = 29 )
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
C*
	CHARACTER	parms (MMPARM)*4, gemftm (IFCSTM)*15,
     +			gemstm (IFCSTM)*15 
	CHARACTER	bultin*(DCMXBF), segmnt*(DCMXBF),
     +			seqnum*4, wmorpt*8, oristn*8, btime*12, bbb*8,
     +			sysdt*12, dattmp*12, filnam*132, errstr*80
	CHARACTER	adstn (LLSTFL)*8, stnnam(LLSTFL)*32,
     +                  stat(LLSTFL)*2, coun(LLSTFL)*2,
     +                  tbchrs(LLSTFL)*20
	CHARACTER	zonecd (LLSTFL)*6, dttmsn*15, dattim*15,
     +			bdtmsn*15

	INTEGER		istnm (LLSTFL), ispri (LLSTFL)
	REAL            adlat(LLSTFL), adlon(LLSTFL), selv(LLSTFL)
    	REAL		rdata (MMPARM,IFCSTM), adata (MMPARM)
	INTEGER		idtarr (5), isstar (5), istarr (5)
	INTEGER		ibstar (5), itype
	INTEGER		lfchr (IFCSTM), kpshr (IFCSTM), ps12tb (IFCSTM)
	LOGICAL		good, goodsg, more, addstn, cirflg, datflg,
     +			moresg, mss, bultm
C*
	PARAMETER	( NUMPRM = 17 )
	PARAMETER	( NUMEXT = MMPARM - NUMPRM )
    	CHARACTER	cprms (MMPARM)*4, cpnm (NUMNAM)*10
	INTEGER		iprms (MMPARM)
	INTEGER		lnpnm (NUMNAM), nxtyp ( 6 ), mapprm (NUMNAM)
        DATA            cprms / 'QP12', 'QPX2', 'SN12', 'PP12',
     +                          'TNTF', 'TDYF', 'TMPF', 'DWPF',
     +                          'RELH', 'SMPH', 'WCEQ', 'HEAT',
     +                          'DRCT', 'OVIS', 'CFRT', 'WNUM',
     +                          'WXPB',
     +				NUMEXT * ' ' /
C
C*	Must group the 12-hourly parameters together at the beginning,
C*	of cpnm, so that when there is a group of 3 values for TEMP, it
C*	will be known where the positions of other existing 12-hourly
C*	parameters are.  Also, the precip phenomena must be in order of
C*	highest priority (TSTMS) to lowest priority (DRIZZLE).  Also,
C*	SNOW SHWRS must come before SNOW.  Otherwise, if 'SNOW SHWRS'
C*	were the phenomenon, it would find "SNOW" first. Same is true
C*	for RAIN SHW and RAIN.  The duplicate 'SNOWSHWRS' is inserted
C*	before the other precip phenomena and is handled as a special
C*	case in DC_DCPM.
C
	DATA		cpnm / 'QPF 12HR  ', 'MAX QPF   ', 'SNOW 12HR ',
     +			       'POP 12HR  ', 'TEMP      ', 'DEWPT     ',
     +			       'RH        ', 'WIND SPD  ', 'WIND CHILL',
     +			       'HEAT INDEX', 'WIND DIR  ', 'PWIND DIR ',
     +			       'OBVIS     ', 'AVG CLOUDS', 'CLOUDS    ',
     +			       'SNOWSHWRS ', 'TSTMS     ', 'SLEET     ',
     +			       'FRZNG RAIN', 'FRZNG DRZL', 'SNOW SHWRS',
     +			       'FLURRIES  ', 'RAIN SHW  ', 'SNOW      ',
     +			       'RAIN      ', 'SPRINKLES ', 'DRIZZLE   ',
     +			       'MX/MN     ', 'MN/MX     '/
C*	lnpnm is the length of the strings in cpnm.
	DATA		lnpnm /  8,  7,  9,  8,  4,  5,  2,  8, 10,
     +				10,  8,  9,  5, 10,  6,  9,  5,  5,
     +				10, 10, 10,  8,  8,  4,  4,  9,  7,
     +				 5,  5 /
C*	Array nxtyp contains the indices of the first parameter in array
C*	cpnm of a particular type.  For example, cpnm (nxtyp(1)) thru
C*	cpnm (nxtyp(2) - 1) are range data and cpnm (nxtyp(2)) thru
C*	cpnm (nxtyp(3) - 1) are single integer data.
	DATA		nxtyp / 1, 4, 11, 16, 28, 30 /
	DATA		mapprm /  1,  2,  3,  4,  7,  8,  9, 10, 11, 12,
     +				 13, 13, 14, 15, 15, 16, 16, 16, 16, 16,
     +				 16, 16, 16, 16, 16, 16, 16,  5,  6 /
C------------------------------------------------------------------------
	iret = 0
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file to 1 for surface.
C
	maxfil = 2
	iftype = 1
	CALL DC_FINT ( maxfil, iftype, prmfil, iperr )
C
C*	Read the zone table
C
        IF ( iperr .eq. 0 ) CALL DC_STNS  ( stntbl, adstn, istnm,
     +		stnnam, stat, coun, adlat, adlon, selv, ispri, tbchrs,
     +		nade, iperr )

	addstn = .true.
	cirflg = .false.
C
C*	Loop until a timeout occurs.
C
	DO WHILE ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenb, ifdtyp, iperr )
C
	    IF ( iperr .ne. 0 ) THEN
C
C*		Write an error to the decoder log file.
C
		CALL DC_WLOG ( 0, 'DC', iperr, ' ', ier )
	      ELSE
C
C*		Parse the header from the bulletin.
C
		good = .true.
		CALL DC_GHDR ( bultin, lenb, seqnum, wmorpt,
     +			       oristn, btime, bbb, nchar, ierr )
		ibpnt = nchar + 1
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin (:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCRDF', 2, errstr, ier )
		    good = .false.
		END IF
C
		IF ( good ) THEN
C
C*		    Get the system time and make a standard GEMPAK
C*		    time from the "current" time.
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
		    CALL ST_LCUC ( bultin, bultin, ier )
C
C*		    Get bulletin issue time
C
		    itpnt = ibpnt + 20
		    CALL RD_GITM ( bultin, lenb, itpnt, ibstar,
     +           	           bdtmsn, ierbtm )
C
		    moresg = .true.
  		    DO WHILE ( moresg )
C
C*			Get a segment
C
		        goodsg = .true.
			CALL RD_GSGT ( bultin, lenb, ibpnt, segmnt,
     +				lens, ier )
			IF ( ier .ne. 0 ) THEN
			    moresg = .false.
			    goodsg = .false.
			  ELSE
			    IF ( lens .lt. 80 ) goodsg = .false.
			END IF
			IF ( goodsg ) THEN
C
C*			    Decode the zone list
C
			    CALL RD_GZON ( segmnt, lens, ispnt, zonecd,
     +					nzone, ier )
			    IF ( ier .ne. 0 ) THEN
				ndxex1 = INDEX ( segmnt ( 1:lens ),
     +				         'EXPLANATION OF THIS PRODUCT' )
				ndxex2 = INDEX ( segmnt ( 1:lens ),
     +					 'THE FOLLOWING PARAMETERS' )
				ndxexp = ndxex1 + ndxex2
				IF ( ndxexp .eq. 0 ) THEN
		    		    CALL ST_UNPR ( segmnt (:72), 72,
     +					    errstr, len1, ierr)
				    errstr = oristn(:4) // ':  ' //
     +					    errstr (:72)
		    		    CALL DC_WLOG ( 2, 'DCRDF', -7,
     +					    errstr, ierr)
				END IF
		    		goodsg  = .false.
			    END IF

			    IF ( goodsg ) THEN
C
C*				Get the issue time and convert to
C*				synoptic time for assigning to
C*				this segment
C
				bultm = .false.
				CALL RD_GITM ( segmnt, lens, ispnt,
     +					isstar, dttmsn, ier )
				IF ( ier .ne. 0 ) THEN
				    IF ( ierbtm .ne. 0 ) THEN
		    		        CALL ST_UNPR ( segmnt (:72), 72,
     +					        errstr, len1, ierr)
				        errstr = oristn(:4) // ':  ' //
     +						errstr (:72)
		    		        CALL DC_WLOG ( 2, 'DCRDF', -8,
     +					        errstr, ierr)
		    		        goodsg  = .false.
				      ELSE
C
C*					Use bulletin issue date/time
C
					bultm = .true.
					dttmsn = bdtmsn
					DO ib = 1, 5
					    isstar (ib) = ibstar (ib)
					END DO
				    END IF
				END IF
				IF ( goodsg) THEN
				    ihhmm = isstar ( 4 ) * 100 +
     +						isstar ( 5 )
C
C*				    Compute difference between
C*				    issue and system times
C
				    CALL TI_MDIF ( isstar, istarr,
     +					    imdif, ier2 )
C
C*				    Check that the time is within
C*				    NHOURS before the system time
C
				    IF ( ( ier2 .ne. 0 ) .or.
     +					 ( imdif .lt. ((-60)*nhours) ) .or.
     +					 ( imdif .gt. 60 )) THEN
					goodsg = .false.
C
C*				        Write an error message if the
C*					time is invalid.
C
					errstr = 'INVALID BULLETIN: ' 
     +					      // wmorpt // oristn // 
     +					      btime
					CALL DC_WLOG ( 2, 'DCRDF', 2,
     +					      errstr, ier )
					CALL TI_ITOC ( isstar, dattim,
     +					      ier )
					WRITE (errstr, 1000) 'INVALID ',
     +					      'DATTIM: ', dattim,
     +					      dattmp, imdif
1000					FORMAT ( A, A, A, A, I6 )
					CALL DC_WLOG ( 2, 'DCRDF', 2,
     +					      errstr, ier )
				    END IF
				END IF
			    END IF

			    IF ( goodsg ) THEN
C
C*				Get the first date in the dateline
C
				IF ( bultm ) ispnt = -ispnt
			        CALL RD_GDTE ( segmnt, lens, ispnt,
     +					idtarr, idp, ier )
				IF ( ier .ne. 0 ) THEN
		    		    CALL ST_UNPR ( segmnt (:72), 72,
     +					     errstr, len1, ierr)
				    errstr = oristn(:4) // ':  ' //
     +					     errstr (:72)
		    		    CALL DC_WLOG ( 2, 'DCRDF', -9,
     +					     errstr, ierr )
		    		    goodsg  = .false.
			        END IF
			    END IF
C
C*			    Open the output file.
C
			    IF ( goodsg ) THEN
C
C*				Make a file name from the template and
C*				the time.  Open the file as part of the
C*				open file list.
C
				CALL FL_MNAM ( dttmsn, gemfil, filnam,
     +					ier )
        			iflsrc = MFUNKN
				CALL DC_FCYL ( filnam, iflsrc, stntbl,
     +					iadstn, maxtim, lunf, nparm,
     +					parms, ierr )
C
C*				Check that the file was opened properly.
C
				IF ( ierr .ne. 0 ) THEN
C
C*				    If not, write error to the decoder
C*				    log file.
C
				    CALL DC_WLOG ( 0, 'SF', ierr,
     +					    filnam, ier )
				    goodsg = .false.
				  ELSE
C
C*				    Check for the parameters in the list
C
				    DO j = 1, NUMPRM
					CALL ST_FIND ( cprms (j), parms,
     +						nparm, iprms (j), ier )
					IF ( iprms (j) .eq. 0 ) THEN
					    iprms (j) = nparm + 1
					    errstr = oristn(:4) // ':  '
     +						    // cprms (j)
					    CALL DC_WLOG  ( 2, 'DCRDF',
     +						    -3, errstr, ier )
					END IF
				    END DO
				END IF
			    END IF
C
C*			    Get forecast hours from the first time string
C
			    IF ( goodsg ) THEN
			        mxfctm = 22
			        CALL RD_FCTM ( segmnt, lens, isstar,
     +					idtarr, idp, mxfctm, ispnt,
     +					gemftm, lfchr(1), kpshr(1),
     +					jftmst, jftmen, ierr )
			        IF ( ierr .eq. 0 ) THEN
				    DO jj = 1, IFCSTM
				      DO ii = 1, MMPARM
				        rdata ( ii, jj ) = RMISSD
				      END DO
				    END DO
C
C*				    Decodes the parameter lines for the
C*				    first section of segment.
C
				    CALL RD_DCPM ( segmnt, lens, NUMNAM,
     +					  cpnm, lnpnm, nxtyp, kpshr,
     +					  jftmst, jftmen, iprms, mapprm,
     +					  lfchr, idtarr, oristn, ispnt,
     +					  rdata, more, ierr )
				    IF ( more ) THEN
C
C*				      Get forecast hours for the
C*				      the extended section.
C
				      CALL TI_CTOI (gemftm ( jftmen ),
     +					      idtarr, ierr )
				      idp = - lfchr (jftmen)
				      mxfctm = 18
				      CALL RD_FCTM ( segmnt, lens,
     +					    isstar, idtarr, idp,
     +					    mxfctm, ispnt,
     +					    gemftm ( jftmen + 1 ),
     +					    lfchr  ( jftmen + 1 ),
     +					    kpshr  ( jftmen + 1 ),
     +					    kftmst, kftmen, ierr )
				      IF ( ierr .eq. 0 ) THEN
C
C*					Decodes the parameter lines for
C*					the extended section of segment.
C
				        CALL RD_DCPM ( segmnt, lens,
     +					      NUMNAM, cpnm, lnpnm,
     +					      nxtyp, kpshr( jftmen + 1),
     +					      kftmst, kftmen, iprms, 
     +					      mapprm, lfchr(jftmen + 1),
     +					      idtarr, oristn, ispnt,
     +					      rdata ( 1, jftmen + 1),
     +					      more, ier )
				        IF ( more ) THEN
					  goodsg = .false.	
			    	         ELSE IF ( ier .eq. 0 ) THEN
					  jf6 = jftmen + 1
					  jftmen = jftmen + kftmen
				         ELSE
					  goodsg = .false.
				        END IF
				       ELSE
					goodsg = .false.
				      END IF
				     ELSE
C
C*				      No extended part
C
				      jf6 = 0
				    END IF
			          ELSE
		    		    CALL ST_UNPR ( segmnt (:72), 72,
     +					    errstr, len1, ierr)
				    errstr = oristn(:4) // ':  ' //
     +					    errstr (:72)
		    		    CALL DC_WLOG ( 2, 'DCRDF', -6,
     +					    errstr, ierr )
		    		    goodsg  = .false.
			        END IF
			    END IF

C
C*			    Convert GEMPAK date/time into synoptic times
C
			    IF ( goodsg ) THEN
				CALL RD_SYNT ( gemftm, jf6, jftmst,
     +					jftmen, rdata, gemstm,
     +					ps12tb, ierr )
				CALL RD_MV12 ( ps12tb, iprms, rdata,
     +					ierr)
				DO iz = 1, nzone
				    DO m = jftmst, jftmen
C
C*				      Check for all missing data
C*				      for a particular forecast hour
C
				      mss = .true.
				      j = 1
				      DO WHILE ( mss .and. 
     +						j .le. MMPARM )
					IF ( rdata ( j, m ) .ne.
     +						    RMISSD )
     +						    mss = .false.
					j = j + 1
                                      END DO     
C
C*					Set the zone and time in
C*					output file
C
				      IF ( .not. mss ) THEN
					CALL RA_TMST (lunf, gemstm (m ),
     +					      zonecd (iz), addstn,
     +					      cirflg, datflg, ierr )    
C
C*					    Get time to assign to text
C*					    string.
C
C
C*                                      Check for an error.
C
                                        IF ( ierr .ne. 0 ) THEN
                                            goodsg   = .false.
                                            errstr = ' '
                                            IF ( ierr .eq. -4 ) THEN
                                                errstr = gemftm ( m )
                                              ELSE IF ( ierr .eq. -5 )
     +							THEN
                                                errstr = zonecd (iz)
                                            END IF
                                            CALL DC_WLOG ( 2, 'RA',
     +						    ierr, errstr, ier )
C
C*                                          If the data has already
C*					    been decoded, write data
C*					    regardless.
C
                                          ELSE
                                            goodsg = .true.
                                        END IF
C
C*                                      Write the data to the output
C*					file.  First load rdata into a
C*				        one-dimensional array so that
C*				        it can be passed into SF_WDAT.
C
                                        IF ( goodsg ) THEN
                                            DO j = 1, MMPARM
                                                adata ( j ) =
     +						        rdata ( j, m )
                                            END DO     
                                            CALL SF_WDAT ( lunf, ihhmm,
     +					            adata, ierr )
                                            IF ( ierr .ne. 0 ) THEN
                                                CALL DC_WLOG ( 2, 'SF',
     +						        ierr, ' ', ier )
                                            END IF
                                        END IF
				      END IF
				    END DO
                                    goodsg = .true.
			        END DO
			    END IF
			END IF
		    END DO
		END IF
	    END IF
	END DO
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
