	SUBROUTINE WS_HDLN ( hstrng, lenh, wtype, wthr, icancl, 
     +			     endtim, iret )
C************************************************************************
C* WS_HDLN 								*
C*									*
C* This subroutine gets the message type (warning, watch or advisory)	*
C* and, if given, the expiration time or cancelation information.	*
C*                                                                      *
C* WS_HDLN ( HSTRNG, LENH, WTYPE, WTHR, ICANCL, ENDTIM, IRET )          *
C*									*
C* Input parameters:	                                                *
C*  	HSTRNG          CHAR*           Headline string			*
C*	LENH  	  	INTEGER	  	Length of string		*
C*									*
C* Output parameters:							*
C*  	WTYPE           CHAR*           Winter storm message type	*
C*	WTHR            CHAR*           Weather type			*
C*	ICANCL          INTEGER         Cancellation flag		*
C*	ENDTIM          CHAR*           Expiration time (local)		*
C*	IRET  	  	INTEGER	 	Return code			*
C*					1 = expired at original time	*
C*					0 = normal return		*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		08/02						*
C* M. Li/SAIC		10/02	Added more options for output		*
C* D. Kidwell/NCEP	10/02	Added cancel options, improved endtim   *
C* D. Kidwell/NCEP	11/02	Added DOWNGRADE,cancel words; chg endtim*
C* D. Kidwell/NCEP	11/02	Allowed endtim change only if cancell'n *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	hstrng, wtype, wthr, endtim 
C*
	CHARACTER	strng(15)*30, type(15)*4, last*1, hhmm*7,
     +			tmptim*20
C*
	DATA strng / 'BLIZZARD WARNING', 'LAKE EFFECT SNOW WARNING',
     +		     'WINTER STORM WARNING', 'HEAVY SNOW WARNING',
     +		     'ICE STORM WARNING', 'SLEET WARNING',
     +		     'WINTER STORM WATCH', 'LAKE EFFECT SNOW WATCH',
     +		     'WINTER WEATHER ADVISOR', 'BLOWING SNOW ADVISOR',
     +		     'FREEZING RAIN ADVISOR',
     +		     'FREEZING DRIZZLE ADVISOR',
     +		     'LAKE EFFECT SNOW ADVISOR', 
     +		     'SLEET ADVISOR', 'SNOW ADVISOR' /
	DATA type / 'BLIZ', 'LESN', ' ', '+SN', 'IC', 'SL', ' ', 'LESN',
     +		    ' ', 'BLSN', 'FZRA', 'FZDZ', 'LESN', 'SL', 'SN' /
C------------------------------------------------------------------------
	iret  = 0
	wtype = ' '
	wthr = ' '
	ii = 1
	DO WHILE ( ii .ne. 0 )
C
C*	    If UPGRADED TO or DOWNGRADED TO is found, check the followed
C*	    string only.
C
	    iupg = INDEX ( hstrng ( :lenh ), 'UPGRADED TO' )
	    IF ( iupg .eq. 0 ) iupg = INDEX ( hstrng ( :lenh),
     +					      'DOWNGRADED TO' )
	    CALL ST_LSTR ( strng(ii), lenw, ier )
	    IF ( iupg .gt. 0 ) THEN
		iy = INDEX ( hstrng(iupg:lenh), strng(ii)(:lenw) )
	    ELSE
	     	iy = INDEX ( hstrng(:lenh), strng(ii)(:lenw) )
	    END IF
C
C*	    Set wtype and wthr.
C
	    IF ( iy .gt. 0 ) THEN
		wthr = type(ii)
		ix = INDEX ( strng(ii), 'WARNING' )
        	IF ( ix .gt. 0 ) wtype = 'WRN' 
C
		ix = INDEX ( strng(ii), 'WATCH' )
        	IF ( ix .gt. 0 ) wtype = 'WTC'
C
C*		Allow either ADVISORY or ADVISORIES.
C
		ix = INDEX ( strng(ii), 'ADVISOR' )
        	IF ( ix .gt. 0 ) wtype = 'ADV'
		ii = 0
	    ELSE
		ii = ii + 1
	    END IF
	    IF ( ii .gt. 15 ) ii = 0
	END DO
C
C* 	Get cancellation and endtim.
C
	icancl = 0
	endtim = ' '
	ix = INDEX ( hstrng ( :lenh ), 'CANCEL' )
	iy = INDEX ( hstrng ( :lenh ), 'DISCONTINUE' )
	iz = INDEX ( hstrng ( :lenh ), 'NO LONGER IN EFFECT' )
	iw = INDEX ( hstrng ( :lenh ), 'HAS ENDED' )
	iv = INDEX ( hstrng ( :9 ), '.ENDED ' )
	iu = INDEX ( hstrng ( :lenh ), 'LIFTED' )
	IF ( ( ix + iy + iz + iw + iv + iu ) .gt. 0 ) THEN
	    icancl = 1 
	  ELSE
C
C*	    If 'EXPIRE' is found, icancl = 1.
C
	    ix = INDEX ( hstrng ( :lenh ), 'EXPIRE' )
	    IF ( ix .gt. 0 ) THEN
		icancl = 1
C
C*		If AT or AS OF follows EXPIRE, get the text which
C*		follows and store the string in ENDTIM.
C
		IF ( hstrng (ix+6:ix+6) .eq. ' ' ) THEN
		    jx = ix + 7
		  ELSE
		    jx = ix + 8
		END IF
		IF ( hstrng(jx:jx+4) .eq. 'AS OF' ) THEN
		    endtim = hstrng(jx+6:jx+17)
		  ELSE IF ( hstrng(jx:jx+1) .eq. 'AT') THEN
                    endtim = hstrng(jx+3:jx+14)
		END IF
C
C*              If no time string, check before EXPIRE for ALLOWED TO,  
C*              but not BEEN ALLOWED TO. If found, do nothing.   
C  
		IF ( endtim .eq. ' ' ) THEN
		    ia = INDEX ( hstrng(:ix), 'ALLOWED TO' )
		    ib = INDEX ( hstrng(:ix), 'BEEN' )
		    IF ( ia .gt. 0 .and. 
     +			 ( ib .le. 0 .or. ia-ib .ne. 5 ) ) THEN
			iret = 1
			RETURN
		    END IF			
		END IF
	      ELSE
C
C*		This is not a cancellation.  Check for an end time.
C
		iy = INDEX ( hstrng ( :lenh ), 'THROUGH ' )
		iz = INDEX ( hstrng ( :lenh ), 'UNTIL ' )
		IF ( iy .gt. 0 ) THEN
		    endtim = hstrng ( iy+8:iy+19 )
		  ELSE IF ( iz .gt. 0 ) THEN 
		    endtim = hstrng ( iz+6:iz+17 )
		END IF
C
C*		Until we have more examples of end times for 
C*		non-cancelled reports, do not try to change end time.
C
		endtim = ' '
	    END IF
	END IF
C
	IF ( endtim .ne. ' ' ) THEN
C
C*	    Try to construct a well-formed time string, e.g. 200 PM EST.
C
	    ii = 12
	    ie = 0
	    DO WHILE ( ii .gt. 0 )
C
C*		Look for ending letter of AM, PM, NOON, MIDNIGHT or a
C*		time zone (ending in T) as a possible end of the time 
C*		string.
C
		last = endtim ( ii:ii )
		IF ( ( last .eq. 'T' ) .or. ( last .eq. 'M' ) .or.
     +		     ( last .eq. 'N' ) ) THEN
		    ie = ii
		    ii = 0
		  ELSE
		    ii = ii - 1
		END IF
	    END DO
C
	    IF ( ie .lt. 4 ) THEN
		endtim = ' '
	      ELSE
C
C*		Look for AM, PM, NOON or MIDNIGHT.
C
		iam   = INDEX ( endtim ( :ie ), 'AM' )
		ipm   = INDEX ( endtim ( :ie ), 'PM' )
		inoon = INDEX ( endtim ( :ie ), 'NOON' )
		imid  = INDEX ( endtim ( :ie ), 'MIDNIGHT' )
		IF ( ( iam + ipm + inoon + imid ) .eq. 0 ) endtim = ' '
	    END IF
	END IF
C
	IF ( endtim .ne. ' ' ) THEN
	    tmptim = endtim ( :ie )
	    hhmm = ' '
	    ii   = 1
	    ipos = 1
	    DO WHILE ( ii .lt. ie ) 
C
C*		Look for the hour and minutes.
C
		last = tmptim ( ii:ii )
		CALL ST_ALNM ( last, nn, ier )
		IF ( nn .eq. 1 ) THEN
		    hhmm = hhmm ( :ipos ) // last
		    ii   = ii + 1
		    ipos = ipos + 1
		  ELSE
		    IF ( ii .eq. 1 ) THEN
C
C*			Check for noon or midnight.
C
			IF ( ( inoon .gt. 0 ) .and. 
     +			     ( last .eq. 'N' ) ) THEN
			    hhmm = '1200 PM'
			    ipos = 6
			  ELSE IF ( ( imid .gt. 0 ) .and.
     +			            ( last .eq. 'M' ) ) THEN
			    hhmm = '1200 AM'
			    ipos = 10
			END IF
			ii = ie
		      ELSE IF ( last .eq. ':' ) THEN
			ii = ii + 1
		      ELSE
			ii = ie
		    END IF
		END IF
	    END DO
C
	    IF ( ( ipos .ge. 2 ) .and. ( ipos .lt. 6 ) ) THEN
		IF ( ipos .le. 3 ) THEN
		    hhmm = hhmm ( :ipos ) // '00'
		    ipos = ipos + 2
		END IF
C
C*		Add AM or PM to the string.
C
		IF ( iam .ne. 0 ) THEN
		    hhmm = hhmm ( 2:ipos ) // ' AM'
		  ELSE IF ( ipm .ne. 0 ) THEN
		    hhmm = hhmm ( 2:ipos ) // ' PM'
		  ELSE
		    hhmm = ' '
		END IF
		ipos = MAX ( iam, ipm ) + 3
		IF ( ipos .eq. 3 ) THEN
		    IF ( inoon .ne. 0 ) THEN
			ipos = inoon + 5
		      ELSE IF ( imid .ne. 0 ) THEN
			ipos = imid + 9
		    END IF
		END IF
	    END IF
C
	    IF ( hhmm .ne. ' ' ) THEN
		IF ( ( ie .ge. ( ipos + 2 ) ) .and.
     +		     ( tmptim ( ipos + 2:ipos + 2 ) .eq. 'T' ) ) THEN
C
C*		    Add the time zone to the string.
C
		    CALL ST_LSTR ( hhmm, lent, ier )
		    endtim = hhmm ( :lent ) // ' ' // 
     +			     tmptim ( ipos:ipos + 2 )
		  ELSE
		    endtim = hhmm
		END IF
	      ELSE
		endtim = ' '
	    END IF
	END IF
C*
	RETURN
	END
