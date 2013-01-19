	SUBROUTINE DCGFS ( curtim, gemfil, prmfil, stntbl, iadstn,
     +			   maxtim, nhours, txtflg, iret )
C************************************************************************
C* DCGFS								*
C*									*
C* This routine will decode a GFS MOS bulletin and write the data       *
C* to a GEMPAK surface file.						*
C*									*
C* DCGFS ( CURTIM, GEMFIL, PRMFIL, STNTBL, IADSTN, MAXTIM,		*
C*         NHOURS, TXTFLG, IRET )					*
C*									*
C* Input parameters:							*
C*	CURTIM		CHAR*		Current time for input data	*
C*	GEMFIL		CHAR*		Output file name template	*
C*	PRMFIL		CHAR*		Parameter packing table		*
C*	STNTBL		CHAR*		Station table			*
C*	IADSTN		INTEGER		Number of additional stations	*
C*	MAXTIM		INTEGER		Number of times allowed		*
C*	NHOURS		INTEGER		Number of hours prior to CURTIM	*
C*	TXTFLG		LOGICAL		Flag to save undecoded text	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* m.gamazaychikov/SAIC	11/03	Copied from DCAVNM                      *
C* B. Yin/SAIC           3/04   Changed SS_GTIM to CSS_GTIM             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	PARAMETER       ( IFCSTM = 21 )
	PARAMETER       ( NUMNAM = 19 )
C*
	CHARACTER*(*)	curtim, gemfil, stntbl, prmfil
	LOGICAL		txtflg
C*
	CHARACTER	parms (MMPARM)*4, gemftm (IFCSTM)*15, txttim*15
	CHARACTER	bultin*(DCMXBF), report*(DCMXBF),
     +			seqnum*4, wmorpt*8, oristn*8, btime*12, bbb*8,
     +			sysdt*12, dattmp*12, filnam*132, errstr*80,
     +			dattim*15, stid*8, date*10, carr (8)*10, time*4
	REAL		rdata (IFCSTM,MMPARM), adata (MMPARM)
	INTEGER		istarr (5), irtarr (5), itype
	LOGICAL		more, good, addstn, cirflg, datflg,
     +			gettim, getid
C*
	PARAMETER	( NUMPRM = 22 )
	PARAMETER	( NUMEXT = MMPARM - NUMPRM )
	CHARACTER	cprms (MMPARM)*4, cpnam (NUMNAM)*3
	INTEGER		iprms (MMPARM), istart (NUMNAM)
        DATA            cprms / 'TNTF', 'TDYF', 'TMPF', 'DWPF',
     +                          'CLCT', 'DRCT', 'SKNT', 'PP06',
     +                          'PP12', 'QP06', 'QP12', 'TS06',
     +                          'TC06', 'TS12', 'TC12', 'PCPT',
     +                          'POZP', 'POSN', 'SN24', 'FCIG',
     +                          'FVSA', 'OVIS',
     +				NUMEXT * ' ' /
	DATA		cpnam / './.', 'TMP', 'DPT', 'CLD', 'WDR',
     +				'WSP', 'P06', 'P12', 'T06', 'T12',
     +				'POZ', 'POS', 'TYP', 'Q06', 'Q12',
     +				'SNW', 'CIG', 'VIS', 'OBV' /
C------------------------------------------------------------------------
	iret = 0
C
C*	If the save text flag is set to true, set the file source to
C*	save both the decoded data and the undecoded text.
C
	IF ( txtflg ) THEN
	    iflsrc = MFUNKN + MFTEXT
	  ELSE
	    iflsrc = MFUNKN
	END IF
C
C*	Initialize open file lists. Set the max number of open files.
C*	Set the type of output file to 1 for surface.
C
	maxfil = 2
	iftype = 1
	CALL DC_FINT ( maxfil, iftype, prmfil, iperr )
	addstn = .true.
	cirflg = .false.
C
C*	Loop until a timeout occurs.
C
	DO WHILE ( iperr .eq. 0 )
C
C*	    Get the bulletin.
C
	    CALL DC_GBUL ( bultin, lenbul, ifdtyp, iperr )
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
		CALL DC_GHDR ( bultin, lenbul, seqnum, wmorpt,
     +			       oristn, btime, bbb, nchar, ierr )
		ibpnt = nchar + 1
		IF ( ierr .ne. 0 ) THEN
		    CALL DC_WLOG ( 2, 'DC', ierr, ' ', ier )
		    CALL ST_UNPR ( bultin (:72), 72, errstr, len1, ier )
		    CALL DC_WLOG ( 2, 'DCMOS', 2, errstr, ier )
		    good = .false.
		END IF
C
		IF ( good ) THEN
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
C*		    Get the date and station id from the title line.
C
                    CALL MS_DATE ( bultin, ibpnt, date, irhour, irmin,
     +                             stid, ierr )
                    IF ( ierr .eq. 0 ) THEN
C
C*			Get the forecast times. 
C
			CALL MS_FCTM ( date, irhour, IFCSTM, gemftm,
     +				       ierr )
			ihhmm = irhour * 100 + irmin
		    END IF
		    IF  ( ierr .ne. 0 )  THEN
		        good = .false.
		        CALL DC_WLOG ( 2, 'MS', ierr, ' ', ier )
		    END IF
	    	END IF
		IF ( good ) THEN
C
C*		    Get the time to assign to this bulletin.  First,
C*    		    get observation time.
C
		    CALL DC_GTIM ( istarr, btime, irhour, irmin,
     +				   .false., irtarr, dattim, ier1 )
		    IF ( ier1 .ne. 0 ) THEN
		        CALL DC_WLOG ( 2, 'DC', ier1, ' ', ier )
		    END IF
C
C*    		    Compute difference between observation and
C*    		    system times.
C
		    CALL TI_MDIF ( irtarr, istarr, imdif, ier2 )
C
C*    		    Check that the time is within NHOURS before
C*    		    the system time.
C
		    IF ( ( ier1 .ne. 0 ) .or. ( ier2 .ne. 0 ) .or.
     +		         ( imdif .gt. 60 ) .or.
     +	      	         ( imdif .lt. ((-60)*nhours) ) ) THEN
	    	        good = .false.
C
C*	                Write an error message if the time is invalid.
C
	    	        errstr = 'INVALID BULLETIN: ' //
     +	        	         wmorpt // oristn // btime
	    	        CALL DC_WLOG ( 2, 'DCMOS', 2, errstr, ier)
	    	        WRITE (errstr,1000) 'INVALID DATTIM: ',
     +          		        dattim, dattmp, imdif
1000	    	        FORMAT ( A, A, A, I6 )
	    	        CALL DC_WLOG ( 2, 'DCMOS', 2, errstr, ier )
		    END IF
		END IF
		more  = good
		getid = .false.
C
C*		Loop through the reports.
C
		DO WHILE ( more )
C
C*		    Get the station id if we don't already have it.
C
		    good = .true.
		    IF ( getid ) THEN
		        CALL MS_STID ( bultin, ibpnt, stid, carr, num,
     +				       ier )
			IF ( ier .ne. 0 ) THEN
			    good = .false.
			    more = .false.
			END IF
		      ELSE
			getid = .true.
		    END IF
C
C*		    Get data portion of the next report.
C
		    IF ( good ) THEN
 	    		CALL MS_GRPT ( bultin, lenbul, ibpnt, report, 
     +		 	 	       lenr, ierr )
		        IF ( ierr .ne. 0 ) THEN
			    good = .false.
			    more = .false.
		          ELSE
C*			    Find the starting point of each line in the
C*			    report.
C
 		            CALL MS_GLIN ( report, lenr, NUMNAM, cpnam,
     +				           istart, ierr )
			END IF
		    END IF
C
C*		    Open the output file.
C
		    IF ( good ) THEN
C
C*			Make a file name from the template and the time.
C*			Open the file as part of the open file list.
C
			CALL FL_MNAM ( dattim, gemfil, filnam, ier )
			CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn,
     +				      maxtim, lunf, nparm, parms, ierr )
C
C*			Check that the file was opened properly.
C
			IF ( ierr .ne. 0 ) THEN
C
C*			    If not, write error to the decoder log file.
C
			    CALL DC_WLOG ( 0, 'SF', ierr, filnam, ier )
			    good = .false.
			  ELSE
C
C*			    Check for the parameters in the list.
C
			    DO j = 1, NUMPRM
				CALL ST_FIND ( cprms (j), parms, nparm,
     +					       iprms (j), ier )
				IF ( iprms (j) .eq. 0 ) THEN
				    iprms (j) = nparm + 1
				    CALL DC_WLOG  ( 2, 'DCMOS',
     +						    -3, cprms (j), ier )
				END IF
			    END DO
			END IF
		    END IF
		    IF ( good ) THEN
C
C*			Decode the data and write to the surface file.
C 
		       	CALL MS_DCDA ( report, istart, iprms, IFCSTM,
     +				       irhour, rdata, ierr )
			IF ( ierr .eq. 0 ) THEN
			    gettim = .true.
			    DO i = 1, IFCSTM
C
C*				Set the station and time in output file.
C
				CALL RA_TMST ( lunf, gemftm (i), stid, 
     +					       addstn, cirflg, datflg, 
     +					       ierr )
				IF ( gettim ) THEN
C
C*				    Get time to assign to text string.
C
				    IF ( ierr .ne. -4 ) THEN
					txttim = gemftm ( i )
					gettim = .false.
				    END IF
				END IF
C
C*				Check for an error.
C
				IF ( ierr .ne. 0 ) THEN
				    good   = .false.
				    errstr = ' '
				    IF ( ierr .eq. -4 ) THEN 
      					errstr = gemftm ( i )
				      ELSE IF ( ierr .eq. -5 ) THEN  
      					errstr = stid
				    END IF
				    CALL DC_WLOG ( 2, 'RA', ierr,
     +						   errstr, ier )
C
C*				    If the data has already been decoded,
C*				    write data regardless.
C
				  ELSE
				    good = .true. 
				END IF
C
C*				Write the data to the output file. First
C*				load rdata into a one-dimensional array
C*				so that it can be passed into SF_WDAT.
C
				IF ( good ) THEN
				    DO j = 1, MMPARM
				    	adata ( j ) = rdata ( i, j )
				    END DO	
				    CALL SF_WDAT ( lunf, ihhmm, adata,
     +						   ierr )
				    IF ( ierr .ne. 0 ) THEN
				    	CALL DC_WLOG ( 2, 'SF', ierr,
     +						       ' ', ier )
				    END IF
				END IF
			    END DO
			    good = .true.
			    IF ( gettim ) txttim = dattim
                            CALL RA_TMST ( lunf, txttim, stid, addstn,
     +					   cirflg, datflg, ierr )
			    IF ( ierr .ne. 0 ) THEN
				good   = .false.
				errstr = ' '
				IF ( ierr .eq. -4 ) THEN
				    errstr = txttim
				  ELSE IF ( ierr .eq. -5 ) THEN
				    errstr = stid
				END IF
				CALL DC_WLOG ( 2, 'RA', ierr, errstr,
     +					       ier )
			    END IF
C
C*			    Write the text data to the output file.
C
			    IF ( good ) THEN
				CALL ST_UNP1 ( report, lenr, report,
     +					       lenrnw, ierr )
				CALL ST_INCH ( ihhmm, time, ierr )
				IF ( time .eq. '0   ' ) time = '0000'
				report = stid // date // ' ' // time //
     +					 ' UTC' // CHLF //
     +					 report ( :lenrnw )
                              	CALL SF_WSTR ( lunf, ihhmm, report,
     +					       ierr )
                            	IF ( ierr .ne. 0 ) THEN
                                    CALL DC_WLOG ( 2, 'SF', ierr, ' ',
     +						   ier )
			    	END IF
			    END IF
			END IF
		    END IF
		END DO
	    END IF
	END DO
C
	CALL DC_FCLS ( ier )
C*
	RETURN
	END
