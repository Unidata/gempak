	SUBROUTINE AM_DECD ( report, lenr, iotarr, icorr, itest, tissue,
     +			     gemfil, stntbl, iadstn, maxtim, iret )
C************************************************************************
C* AM_DECD 								*
C*									*
C* This subroutine processes a single airmet bulletin.                  *
C*                                                                      *
C* AM_DECD ( REPORT, LENR, IOTARR, ICORR, ITEST, TISSUE, GEMFIL, STNTBL,*
C*	     IADSTN, MAXTIM, IRET )        	                        *
C*									*
C* Input parameters:							*
C*	REPORT		CHAR*		Airmet bulletin                 *
C*	LENR		INTEGER		Length of bulletin              *
C*	IOTARR (5)	INTEGER		Bull. time - YYYY,MM,DD,HH,MM   *
C*	ICORR		INTEGER		Correction flag                 *
C*	ITEST		INTEGER		Test flag                       *
C*	TISSUE		CHAR*  		Bull. issue time, GEMPAK format *
C*	GEMFIL		CHAR*		Output file name template       *
C*	STNTBL		CHAR*		Station table                   *
C*	IADSTN		INTEGER		Number of additional stations   *
C*	MAXTIM 		INTEGER		Number of hours prior to CURTIM *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 7/00	                                        *
C* D. Kidwell/NCEP	 7/00	End time 15 min. earlier; incr MAXPTS   *
C* D. Kidwell/NCEP	 5/02	Improved checks for cancel, 'AIRMET' use*
C* m.gamazaychikov/SAIC 01/04   Added check for type SW                 *
C* m.gamazaychikov/SAIC 07/04   Added ISSTIM, changed STRTIM assignment	*
C*				Removed adjusting STPTIM by 15 backwards*
C* J. Lewis/AWC		05/05 	CSC to remove org; CSC for AM_HDR call; *
C* 				org -> reg for AM_OUT                   *
C* D. Kidwell/NCEP	 7/05	Check for NEW and assign higher seq. no.*
C* J. Lewis		02/07	Added check for LLWS			*
C* L. Hinson            01/22   Fix for missing LLWS                    *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	report, tissue, gemfil, stntbl
	INTEGER		iotarr (*)
C*
	PARAMETER	( MAXPTS = 40, MAXNEW = 10 )
	CHARACTER	parms (MMPARM)*4, filnam*132, reg*3,
     +			bbb*3, type*2, strtim*20, stptim*20, updt*2,
     +			strbuf*160, string*(DCMXBF), stype*2,
     +			flvl (2)*4, hhmm*4, isstim*20, bvdtim*20,
     +			svstyp (MAXNEW)*2, svstrt (MAXNEW)*20,
     +			svflvl (2,MAXNEW)*4
	INTEGER		itarr (5), isvnpt (MAXNEW)
	REAL		rlat (MAXPTS), rlon (MAXPTS),
     +			svrlat (MAXPTS, MAXNEW), svrlon (MAXPTS, MAXNEW)
	LOGICAL		done, new
C------------------------------------------------------------------------
	iret  = 0
C
C*	Ensure that report is upper case.
C
	CALL ST_LCUC ( report, report, ier )
C
	done   = .false.
	strbuf = report ( :160 )
	ibeg   = 1
        iflsrc = 2
C
	DO WHILE ( .not. done )
	    CALL AM_HDR ( strbuf, iotarr, reg, isstim, stptim, updt,
     +		          type, bbb, iptr, ierr )
	    IF ( ierr .eq. 0 ) THEN
	        IF ( icorr .eq. 0 ) THEN
                    IF ( ( bbb  .eq. 'COR' ) .or.
     +                   ( bbb ( :2 ) .eq. 'CC' ) ) THEN
                        icor = 1
                      ELSE IF ( bbb .eq. 'AMD' ) THEN
                        icor = 2
		      ELSE
		        icor = 0
                    END IF
	          ELSE
		    icor = icorr
	        END IF
	       	icor   = icor + itest
	        ibeg   = ibeg + iptr - 1
	        string = report ( ibeg: )
	        CALL ST_LSTR ( string, lens, ier )
C
		hhmm = stptim ( 8:11 )
		IF ( ( hhmm .eq. '0800' ) .or. 
     +		     ( hhmm .eq. '0900' ) ) THEN
		    iseq = 0
		  ELSE IF ( ( hhmm .eq. '1400' ) .or.
     +			    ( hhmm .eq. '1500' ) ) THEN
		    iseq = 20
		  ELSE IF ( ( hhmm .eq. '2000' ) .or.
     +			    ( hhmm .eq. '2100' ) ) THEN
		    iseq = 40
		  ELSE IF ( ( hhmm .eq. '0200' ) .or.
     +			    ( hhmm .eq. '0300' ) ) THEN
		    iseq = 60
		  ELSE
		    iseq = 80
		END IF
C
	      ELSE
		done = .true.
		CALL DC_WLOG ( 2, 'DCAIRM', ierr, strbuf ( :72 ), ier )
	    END IF
C
	    inew = 0
	    DO WHILE ( ( ierr .eq. 0 ) .and. ( .not. done ) ) 
		indx = 0
 		iair = INDEX ( string ( :lens ), 'AIRMET ' )
		llws = INDEX ( string ( :lens ), 'LLWS POTENTIAL' )
		IF ( iair .gt. 0 ) THEN
		    indx = iair
		    incr =  5
		  ELSE IF ( llws .gt. 0 ) THEN
		    indx = llws - 1
		    incr =  0
		ENDIF
	 	IF ( indx .gt. 0 ) THEN
		    strbuf = string ( indx:indx + 15 )
		    isierr = INDEX ( strbuf ( :16 ), 'SIERRA' )
		    itango = INDEX ( strbuf ( :16 ), 'TANGO' )
		    izulu  = INDEX ( strbuf ( :16 ), 'ZULU' )
		    IF ( ( isierr .eq. 0 ) .and. ( itango .eq. 0 ) .and.
     +			 ( izulu .eq. 0 ) ) THEN
C
C*			Get airmet subtype and bounds.
C
			ibeg   = ibeg + indx + incr
			string = report ( ibeg: )
			CALL AM_BNDS ( string, type, MAXPTS, stype,
     +				       npt, rlat, rlon, iptr, ier )
			IF ( ier .eq. 0 ) THEN
			    iseq   = iseq + 1
		            CALL ST_LSTR ( string, lens, ier )
			    iend   = MIN ( iptr + 159, lens )
		            strbuf = string ( iptr:iend )
C
C*			    Check for a cancellation.
C
			    icancl = INDEX ( strbuf ( :20),
     +					     'CANCEL AIRMET' )
			    IF ( icancl .eq. 0 )  icancl = 
     +			         INDEX ( strbuf ( :20), 'CNCL AIRMET' )
C
C*			    Check for a new airmet in an amendment.
C
			    IF ( icancl .eq. 0 )  THEN
			        CALL ST_LSTR ( strbuf, lenbuf, ier )
				new = ( INDEX ( strbuf ( :lenbuf),
     +				        '..NEW' ) .gt. 0 )  .or.
     +				      ( INDEX ( strbuf ( :lenbuf),
     +                                  ' NEW AREA' ) .gt. 0 )  .or.
     +				      ( INDEX ( strbuf ( :lenbuf),
     +                                  ' NEW AIRMET' ) .gt. 0 )
			      ELSE
				new = .false.
			    END IF 
			    flvl ( 1 ) = ' '
			    flvl ( 2 ) = ' '
			    IF ( ( icancl .eq. 0 )   .and.
     +			         ( type  .ne. 'IR' ) .and. 
     +			         ( stype .ne. 'SW' ) .and. 
     +			         ( stype .ne. 'WS' ) ) THEN
C
C*				Get flight level(s) for TB or IC.
C
 				CALL AM_FLVL ( strbuf, flvl, jptr, ier )
				iptr = iptr + jptr
			      ELSE IF ( icancl .gt. 0 ) THEN
				icancl = 1
			    END IF
C
C*                          Find strtim, based on isstim and stptim.
C
	                    CALL TI_CTOI ( stptim, itarr, ier )
	                    CALL TI_SUBM ( itarr, 360, itarr, ier )
	                    CALL TI_ITOC ( itarr, bvdtim, ier )
                            CALL TI_DIFF ( isstim, bvdtim, nmin, ier)
                            IF ( ( icor .ne. 0 .and. icor .ne. 3 ) .or.
     +                           ( icancl .ne. 0 ) ) THEN
C
C*                              For corrected, amended or cancelled
C*                              report
C
                                IF ( nmin .ge. 0 ) strtim = isstim
                                IF ( nmin .lt. 0 ) strtim = bvdtim
		              ELSE
C
C*                              For regular or test report
C
                                strtim = bvdtim
                            END IF
C
C*			    If this is not a new area included in an
C*			    amendment, write it out now.  Make a file
C*	    		    name from the template and the time.  Open
C*			    the file as part of the open file list.
C
			    IF ( .not. new ) THEN
 	    		      CALL FL_MNAM ( tissue, gemfil, filnam, 
     +					     ier )
 	    		      CALL DC_FCYL ( filnam, iflsrc, stntbl, 
     +	                                     iadstn, maxtim, lunf, 
     +					     nparm, parms, ier )
			      CALL AM_OUT ( lunf, stype, isstim, strtim, 
     +                                      stptim, reg, iseq, updt, 
     +					    flvl, icor, icancl, rlat, 
     +					    rlon, npt, ier )
			     ELSE
C
C*			      Save new values to write later, assigning
C*			      higher sequence numbers.
C
			      iseq = iseq - 1
			      inew = inew + 1
			      IF ( inew .le. MAXNEW ) THEN
			          svstyp ( inew )    = stype
				  svstrt ( inew )    = strtim
				  svflvl ( 1, inew ) = flvl ( 1 ) 
				  svflvl ( 2, inew ) = flvl ( 2 )
				  isvnpt ( inew )    = npt
			          DO ii = 1, npt
				    svrlat ( ii, inew ) = rlat ( ii )
				    svrlon ( ii, inew ) = rlon ( ii )
				  END DO
			      END IF
			    END IF
			  ELSE
			    CALL DC_WLOG ( 2, 'DCAIRM', ier,
     +					   report(ibeg-6:ibeg+33), ier1)
			END IF
C
			ibeg = ibeg + iptr - 1
		      ELSE
C
C*			There was no decodable data for this airmet 
C*			type.
C
			ierr = 1
			ibeg = ibeg + indx - 20
		    END IF
		    string = report ( ibeg: )
	            CALL ST_LSTR ( string, lens, ier )
		    iend   = MIN ( indx + 159, lens )
		    strbuf = string ( :iend )
		  ELSE
		    done = .true.
		END IF
	    END DO
C
C*	    Check for 'NEW' reports to be written.
C
	    IF ( inew .gt. 0 ) THEN
		icancl = 0
	        DO ii = 1, inew
		    iseq = iseq + 1
 	    	    CALL FL_MNAM ( tissue, gemfil, filnam, ier )
 	    	    CALL DC_FCYL ( filnam, iflsrc, stntbl, iadstn,
     +	                           maxtim, lunf, nparm, parms, ier )
		    CALL AM_OUT ( lunf, svstyp ( ii ), isstim, 
     +				  svstrt ( ii ), stptim, reg, iseq, 
     +				  updt, svflvl ( 1, ii ), icor, icancl, 
     +				  svrlat ( 1, ii ), svrlon ( 1, ii ), 
     +				  isvnpt ( ii ), ier )
		END DO
	    END IF
	    IF ( ierr .lt. 0 ) done = .true.
	END DO
C
 	CALL DC_FCLS ( ier )
C*
	RETURN
	END
