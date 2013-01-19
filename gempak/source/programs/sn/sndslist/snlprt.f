	SUBROUTINE SNLPRT  ( isnfln, nlun, lun, times, ntime, nparts,
     +			     iret )
C************************************************************************
C* SNLPRT								*
C*									*
C* This subroutine processes and writes text data.			*
C*									*
C* SNLPRT  ( ISNFLN, NLUN, LUN, TIMES, NTIME, NPARTS, IRET )		*
C*									*
C* Input parameters:							*
C* 	ISNFLN		INTEGER		File number			*
C*	NLUN		INTEGER		Number of output devices	*
C*	LUN   (NLUN)	INTEGER		LUNs of output devices		*
C*	TIMES (*)	CHAR*		Times to process		*
C*	NTIME		INTEGER		Number of times			*
C*	NPARTS		INTEGER		Parts flag                      *
C*									*
C* Output parameters:							*
C*	IRET 		INTEGER		Error code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* D. Kidwell/NCEP       2/01                                           *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		lun (*)
	CHARACTER*(*)	times (*)
C*
	CHARACTER	stid*8, part (4)*4
	CHARACTER	string*4000
	LOGICAL		done, prnt
C*
	DATA		part / 'TXTA', 'TXTB', 'TXPB', 'TXTC' /
C------------------------------------------------------------------------
	iret = 0
	IF  ( ( nparts .lt. 1 ) .or. ( nparts .gt. 4 ) ) RETURN
C
C*	Determine which parts to print.
C
	nprt = nparts
	incr = 1
	IF  ( nparts .eq. 2 ) THEN
	    nprt = 4
	    incr = 3
	END IF
C
C*	Loop through times.
C
	DO  itim = 1, ntime
C
C*	    Set the requested time.
C
	    CALL SN_STIM  ( isnfln, times ( itim ), ier )
C
C*	    Loop through file looking for stations.
C	
	    prnt = .false.
	    done = .false.
	    DO WHILE  ( .not. done )
C
C*	        Get next station.
C
	        CALL SN_SNXT  ( isnfln, stid, istnm, rlat, rlon, elev, 
     +		                ier)
C
C*	        Set flag if there are no more stations.
C
	        IF  ( ier .ne. 0 )  THEN
	            done = .true.
C
C*	            Otherwise, get data.
C
	          ELSE
		    DO ii = 1, nprt, incr
		        CALL SN_RSTR  ( isnfln, part ( ii ), string,
     +				        ihhmm, lenstr, ier ) 
C
C*			Format string for display.
C
			string = ' ' // string
			lenstr = lenstr + 1
		        IF ( lenstr .gt. 1248 ) lenstr = 1248 
C
C*		        Write data to output units.
C
		        IF  ( ier .eq. 0 )  THEN
			    prnt = .true.
			    DO  j = 1, nlun
				istart = 1
				iend   = MIN0 ( 78, lenstr )
				DO WHILE ( istart .le. lenstr )
			            WRITE ( lun (j), 1000 )
     +					    string ( istart:iend )
				    istart = iend + 1
				    iend   = MIN0 ( istart+77, lenstr )
				END DO
			    END DO
 1000			    FORMAT ( A )
		        END IF
		    END DO
	        END IF	        
	    END DO
	    IF  ( prnt )  THEN
		DO  j = 1, nlun
		    WRITE ( lun (j), * ) ' '
		END DO
	    END IF
	END DO
C*
	RETURN 
	END
