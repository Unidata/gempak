	SUBROUTINE TG_FIND  ( dattim, ntimin, timlst, timout, 
     +			      ntime, timfnd, iret )
C************************************************************************
C* TG_FIND								*
C*									*
C* This subroutine converts the user input for DATTIM into a list	*
C* of times.  The times may be entered as a list or range of times.	*
C* The requested times are returned in TIMFND.  The times in the file	*
C* are input in TIMLST, where the times must be in the standard		*
C* GEMPAK format and must be sorted from earliest to latest.  This	*
C* subroutine will write error messages for any error encountered.	*
C*									*
C* TG_FIND  ( DATTIM, NTIMIN, TIMLST, TIMOUT, NTIME, TIMFND,		*
C*            IRET )							*
C*									*
C* Input parameters:							*
C*	DATTIM		CHAR*		User input time			*
C*	NTIMIN		INTEGER		Number of data set times 	*
C*	TIMLST (NTIMIN)	CHAR*		Data set times			*
C*									*
C* Output parameters:							*
C*	TIMOUT		CHAR*		User input time 		*
C*	NTIME		INTEGER		Number of times requested	*
C*	TIMFND (NTIME)	CHAR*		Requested times			*
C*	IRET		INTEGER		Return code			*
C*					 +2 = time not in data set	*
C*					 +1 = EXIT entered		*
C*					  0 = normal return		*
C*					 -1 = DATTIM is invalid		*
C*					 -5 = no valid times entered	*
C*					-16 = no times in range		*
C*					-17 = cannot list times		*
C*					 -4 = data set has no times	*
C**									*
C* Log:									*
C* M. Goodman/RDS	10/84						*
C* M. desJardins/GSFC	 4/86	Changed call to ST_LIST to ST_CLST	*
C* M. desJardins/GSFC	11/87	GEMPAK4 version				*
C* I. Graffman/RDS	12/87	Return character times			*
C* M. desJardins/GSFC	 4/90   Add time with increment			*
C* S. Schotz/GSC	 5/90	Get value of rspnd via IP_RESP		*
C* K. Brill/NMC          9/90   Return only times in data set for time  *
C*                              with increment				*
C* G. Krueger/EAI	 8/93	Modified TI_FIND to TG_FIND		*
C* D.W.Plummer/NCEP	11/96	Added range within list capability	*
C* D.W.Plummer/NCEP	12/96	Added FALL capabilities for fcst times	*
C* D.W.Plummer/NCEP	 1/97	Simplified range processing		*
C* D.W.Plummer/NCEP	 9/98	Added explicit check for ALLFfff	*
C* T. Lee/GSC		 4/99	Removed NEXP, replaced with LLMXGT	*
C* T. Lee/GSC		 7/99	Selected cycle for grid time		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)  	dattim, timlst (*), timout, timfnd (*)
	LOGICAL		respnd, chkdat, chkcyc, chkfhr, gtype
C*
	CHARACTER*20	stntim, inc, timrng(LLMXGT)
	CHARACTER*64	list (LLMXGT)
	CHARACTER*20	lastim, firtim
	CHARACTER	time*80, date*6, cycle*4, fhr*3, ymdh*6
	INTEGER		rngtyp
C--------------------------------------------------------------------------
	ntime = 0
C
C*	No times in list is an error.
C*	Otherwise, initialize output variables and convert input to
C*	upper case.
C
	IF  ( ntimin .le. 0 )  THEN
	    iret = -4
	    CALL ER_WMSG  ( 'TG', iret, ' ', ier )
	    RETURN
	  ELSE 
	    iret   = 0
	    timout = dattim
	END IF
	CALL ST_LCUC  ( dattim, time, ier )
C
C*	Check for 'LIST' to list all times in file.
C
	IF  ( time .eq. 'LIST' )  THEN
C
C*	    Only list time if RESPOND flag is set.
C
	    CALL IP_RESP ( respnd, ier )
	    IF  ( respnd )  THEN
		CALL TI_DSPL  ( ntimin, timlst, timout, ier )
		IF  ( ier .eq. 1 )  THEN
		    timout = dattim
		    iret   = 1
		    RETURN
		  ELSE
		    CALL ST_LCUC  ( timout, time, ier )
		END IF
	      ELSE
		iret   = -17
		CALL ER_WMSG  ( 'TG', iret, ' ', ier )
		RETURN
	    END IF
	END IF
C
C*	Check for special cases where all times are requested.
C
	IF  ( time .eq. 'ALL' )  THEN
	    DO  i = 1, ntimin
	        timfnd (i) = timlst (i)
	    END DO
	    ntime = ntimin
	    RETURN
	END IF
C
C*	Break user input into a list of times.
C
	CALL ST_CLST  ( time, ';', ' ', LLMXGT, list, nlist, ier )
C
C*	Exit if there are no times in list.
C
	IF  ( nlist .eq. 0 )  THEN
	    iret = -18
	    CALL ER_WMSG  ( 'TG', iret, ' ', ier )
	    RETURN
	END IF
C
C*	Loop through all elements in list; check each for range.
C
	DO  nl = 1, nlist
C
	    iposa  = INDEX  ( list(nl),  'ALL' )
C
	    IF  ( iposa .gt. 0 )  THEN
C
	        iposas = INDEX  ( list(nl), 'ALL/' )
	        iposaf = INDEX  ( list(nl), 'ALLF' )
	        iposf  = INDEX  ( list(nl),    'F' )
	        iposfa = INDEX  ( list(nl), 'FALL' )
	        iposs  = INDEX  ( list(nl),    '/' )
C
C*	        Check for all times
C
		chkdat = .false.
		chkcyc = .false.
		chkfhr = .false.
C
		IF ( list(nl) .eq. 'ALL' )  THEN
C
C*	        Check for all dates with a particular forecast time.
C
	        ELSE IF  ( iposaf .gt. 0 )  THEN
C
		    chkfhr = .true.
		    IF ( iposf .gt. 0 )  THEN
		        fhr = list(nl) ( iposf+1 : ) // "000"
		        CALL ST_LSTR( list(nl), lt, iret )
		        IF ( (lt-iposf) .eq. 2 )  fhr =  "0" // fhr
		        IF ( (lt-iposf) .eq. 1 )  fhr = "00" // fhr
		        fhr = fhr ( 1: 3)
		    END IF
C
C*	        Check for all days with a particular forecast time.
C
	        ELSE IF  ( iposas .gt. 0 .or. iposaf .gt. 0 )  THEN
C
		    IF ( iposf .gt. 0 )  THEN
		        fhr = list(nl) ( iposf+1 : ) // "000"
		        CALL ST_LSTR( list(nl), lt, iret )
		        IF ( (lt-iposf) .eq. 2 )  fhr =  "0" // fhr
		        IF ( (lt-iposf) .eq. 1 )  fhr = "00" // fhr
		        fhr = fhr ( 1: 3)
		        chkfhr = .true.
		    END IF
		    IF ( (iposf-iposs) .gt. 1 )  THEN
		        cycle  = list(nl) (iposs+1:iposf-1) // "00" 
		        IF ( (iposf-iposs) .eq. 2 )  
     +			    cycle = "0" // cycle
		        cycle  = cycle (1:4)
		        chkcyc = .true.
		    END IF
C
C*	        Check for all forecast times for one cycle requested.
C
	        ELSE IF  ( iposfa .gt. 0 )  THEN
C
		    chkdat = .true.
		    chkcyc = .true.
	            IF  ( list(nl) (1:iposs-1) .eq. 'LAST' .or.
     +		          iposs .eq. 1 )  THEN
		        stntim = timlst (ntimin)
		        date   = stntim (1:6)
		        IF ( (iposfa-iposs) .gt. 1 )  THEN
		            cycle  = list(nl) (iposs+1:iposfa-1) // "00" 
			    IF ( (iposfa-iposs) .eq. 2 )  
     +				    cycle = "0" // cycle
		            cycle  = cycle (1:4)
		        ELSE
		            cycle  = stntim (8:11)
		        END IF
		      ELSE
                        CALL TG_FULL ( list(nl) ( 1:iposfa ), 
     +				       timlst (1), timlst (ntimin), 
     +				       stntim, ier )
                        IF  ( ier .ne. 0 )  THEN
                            iret = -18
                            CALL ER_WMSG  ( 'TG', iret, list(nl), ier )
                            RETURN
                          ELSE
                            date  = stntim (1: 6)
                            cycle = stntim (8:11)
                        END IF
		    END IF
C
C*	        Check for all times on one day requested.
C
	        ELSE IF  ( iposs .gt. 0 )  THEN
C
C*	            Get date using last time to fill in missing parts.
C
		    chkdat = .true.
		    ipossf = INDEX ( list(nl), '/ALLF' )
		    IF ( ipossf .ne. 0 .and. 
     +			list(nl)(ipossf+5:ipossf+7) .ne. 'ALL' )  THEN
                        fhr = list(nl) ( iposf+1 : ) // "000"
                        CALL ST_LSTR( list(nl), lt, iret )
                        IF ( (lt-iposf) .eq. 2 )  fhr =  "0" // fhr
                        IF ( (lt-iposf) .eq. 1 )  fhr = "00" // fhr
                        fhr = fhr ( 1: 3)
     			chkfhr = .true. 
		    END IF
	            IF  ( list(nl) (1:iposs-1) .eq. 'FIRST' )  THEN
		        stntim = timlst (1)
		        date   = stntim (1:6)
	              ELSE
		        CALL TG_FULL ( list(nl) ( 1:iposs ), timlst (1),
     +		    	               timlst (ntimin), stntim, ier )
		        IF  ( ier .ne. 0 )  THEN
		            iret = -18
		            CALL ER_WMSG  ( 'TG', iret, list(nl), ier )
		            RETURN
		          ELSE
		            date = stntim ( 1: 6 )
		        END IF
	            END IF
	        END IF
C
C*	        Find the times.
C
	        DO  i = 1, ntimin
		    IF  ( .not. chkdat .or. ( chkdat .and. 
     +		        ( date  .eq. timlst (i) ( 1: 6) ) ) )  THEN
		      IF  ( .not. chkcyc .or. ( chkcyc .and. 
     +		          ( cycle .eq. timlst (i) ( 8:11) ) ) )  THEN
			IF  ( .not. chkfhr .or. ( chkfhr .and. 
     +			    ( fhr   .eq. timlst (i) (13:15) ) ) )  THEN
			    ntime = ntime + 1
			    timfnd (ntime) = timlst (i)
			END IF
		      END IF
		    END IF
	        END DO
C
	        IF  ( ntime .eq. 0 )  THEN
	            iret = -5
		    CALL ER_WMSG  ( 'TG', iret, ' ', ier )
	        END IF
C	        RETURN
C
	    ELSE
C
	        CALL ST_RANG  ( list(nl), firtim, lastim, inc, 
     +			        itype, ier )
C
C*		If user input time is FIRST, get the most recent cycle.
C
		IF  ( list (nl) (1:5) .eq. 'FIRST' )  THEN
		    CALL TG_QRNG  ( list (nl), rngtyp, gtype, iret )
		    IF  ( rngtyp .lt. 2 )  THEN
			ymdh = timlst (ntimin) (1:6)
			nfs = 0
			DO i = 1, ntimin
			    IF  ( INDEX ( timlst (i), ymdh ).ne.0 ) THEN
				nfs = nfs + 1
				timlst (nfs) = timlst (i)
			    END IF
			END DO
			ntimin = nfs
		    END IF
		END IF
C
	        IF  ( itype .eq. 0 )  THEN
C
C*	            Put each time in the list into a standard format.
C
		    IF  ( list (nl) .eq. 'FIRST' )  THEN
		        stntim = timlst (1)
		        ier    = 0
		      ELSE
		        CALL TG_FULL ( list (nl), timlst (1),
     +				       timlst (ntimin), stntim, ier )
		        IF  ( ier .ne. 0 )  THEN
			    CALL ER_WMSG  ( 'TG', ier, list (nl), jer )
		        END IF
		    END IF
		    IF  ( ier .eq. 0 )  THEN
		        CALL ST_FIND  ( stntim, timlst, ntimin, 
     +			    indx, ier )
		        IF  ( indx .ne. 0 )  THEN
			    ntime = ntime + 1
		    	    timfnd (ntime) = timlst (indx)
		          ELSE
			    CALL ER_WMSG  ( 'TG',  +2, stntim, ier )
		        END IF
		    END IF
C
C*	            Check for ranges, with or without increment.
C
	          ELSE IF  ( itype .ge. 1 )  THEN
C
	            CALL TG_RANG ( list(nl), ntimin, timlst,
     +			    	   ntm, timrng, iret )
	            CALL ER_WMSG ( 'TG', iret, dattim, ier )
C
C*	            Keep only those times that are in the data set.
C*	            nntim used only for range error checking.
C
		    nntim = 0
	            DO itmr = 1, ntm
	                DO itmds = 1, ntimin
	                    IF ( timlst(itmds) .eq. timrng(itmr) )  THEN
	                        nntim = nntim + 1
	                        ntime = ntime + 1
	                        timfnd ( ntime ) = timlst ( itmds )
	                    END IF
	                END DO
	            END DO
	            IF ( nntim .eq. 0 ) THEN
	                iret = -16
		        CALL ER_WMSG  ( 'TG', iret, ' ', ier )
	            END IF
	        END IF
C
	    END IF
C
	END DO
C
C*	    Check that at least one time was found.
C
	IF  ( ntime .eq. 0 )  THEN
	    iret = -5
	    CALL ER_WMSG  ( 'TG', iret, ' ', ier )
	END IF
C*
	RETURN
	END		
