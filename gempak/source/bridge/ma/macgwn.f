       SUBROUTINE MA_CGWN ( cgrpt, mszrpt, ipt, iret )
C************************************************************************
C* MA_CGWN                                                              *
C*                                                                      *
C* This subroutine decodes the wind data in one Coast Guard report.  It	*
C* separates the wind direction and wind speed from the input string	*
C* and calls separate subroutines to decode and store the wind dir. and	*
C* speed.  The values are stored in common /rintfv/.			*
C*                                                                      *
C* MA_CGWN  ( CGRPT, MSZRPT, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      CGRPT           CHAR*           Report array                    *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to start of field.      *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRSKNT)  REAL            wind speed (kts)                *
C*      RIVALS(IRDRCT)  REAL            wind direction                  *
C*      RIVALS(IRSUWS)  REAL            source units for wind speed     *
C*      RIVALS(IRGUST)  REAL            max. wind speed (gust) (kts)    *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       -1 = No / found in rest of rpt *
C*                                       -2 = Bad wind speed            *
C*                                       -3 = Error rtn from MA_CGBG    *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01	Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_WIND.	*
C*				Corrected errno.  Changed prologue.	*
C* F. J. Yen/NCEP	 7/01	Included GEMPRM.PRM.(S.Chiswell/Unidata)*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'macmn.cmn'
	CHARACTER*(*)	cgrpt
C*
	CHARACTER*100	stwind, outst 
	CHARACTER*100	wdstrg, wsstrg, gststr
	CHARACTER*4	parm
	LOGICAL		found
	DATA		parm/'WIND'/
C------------------------------------------------------------------------
	iret = 0
	i = ipt
	found = .false.
C
C*	Look for '/' character.  Stop looping when the '/' we found is
C*	the field separator.                                            
C
	DO WHILE ( .not. found )
	    IF ( i .le. mszrpt ) THEN
		islash = index(cgrpt(i:i),'/')
		IF ( islash .eq. 0 ) THEN
		    i = i + 1
		  ELSE
		    iendwd = i - 1
		    found = .true.
		END IF
	      ELSE
		iret = -1
		RETURN
	    END IF
	END DO
C
C*	Wind field lies between ipt and iendwd (inclusive).  
C*	Compress blanks out of wind field.
C
	stwind = cgrpt(ipt:iendwd)
	CALL ST_RMBL ( stwind, outst, length, kret )
	IF ( length .gt. 1 ) THEN
C
C*	    Split wind group into 'like-type'groups to facilitate decoding.
C
	    iretbg = 0
	    CALL MA_CGBG ( outst, iretbg )
	    IF ( iretbg .ne. 0 ) THEN
		iret = -3
		RETURN
	    END IF
	    IF ( nflds .eq. 1 ) THEN
		IF ( itypsf(1) .eq. ALPHA ) THEN
		    IF ( fields(1) .eq. 'CALM' ) THEN
			rivals(irdrct) = 0.
			rivals(irsknt) = 0.
			rivals(irsuws) = 4.
		      ELSE
C
C*               	Field is either wind direction only or is garbage.
C
			logmsg = ' Missing wind speed or bad format!'
			CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
			ipt  = iendwd + 2
			RETURN
		    END IF
		  ELSE
C
C*                  Wind direction missing, so just skip this field.
C
		    logmsg = ' Missing wind direction!'
		    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		    ipt  = iendwd + 2
		    RETURN
		END IF
	      ELSE IF ( nflds .eq. 2 .or. nflds .eq. 4 ) THEN
C
C*	        Decode wind direction.
C
		IF ( itypsf(1) .eq. ALPHA ) THEN
		    wdstrg = fields(1)
		    CALL MA_CGWD ( wdstrg, parm, lret )
		  ELSE
		    ipt  = iendwd + 2
		    RETURN
		END IF
C
C*		Decode wind speed if direction was successfully decoded.
C
		IF ( lret .eq. 0 ) THEN
		    wsstrg = fields(2)
		    CALL MA_CGWS ( wsstrg, lensf(2), mret )
		    IF ( mret .lt. 0 ) THEN
C
C*			Bad wind speed field - probable decode error
C*			(code is decoding the wrong field due to some
C*			format error), so stop decoding this entire
C*			report and go get the next report. 
C
			logmsg = 'Bad wind speed of ' //
     +					wsstrg(1:lensf(2))
			CALL DC_WLOG ( 2, 'MA', 10, logmsg, ierwlg )
			logmsg = ' Skip this report!'
			CALL DC_WLOG ( 2, 'MA', 1, logmsg, ierwlg )
			iret = -2
			RETURN
		    END IF 
C
C*		    Check to see if wdir was variable but speed is
C*		    greater than 6 kts (2 m/s).  If so, set wdir,
C*		    wspd, and suws to missing.
C
		    IF ( ( rivals(irdrct) .eq. -99. ) .and. 
     +			    ( ( rivals(irsuws) .eq. 4 .and. 
     +				rivals(irsknt) .gt. 6. ) .or. 
     +			    ( rivals(irsuws) .eq. 1 .and. 
     +				rivals(irsknt) .gt. 2 ) ) ) THEN
			rivals(irdrct) = RMISSD
			rivals(irsknt) = RMISSD
			rivals(irsuws) = RMISSD
		    END IF
		END IF
C
C*		Decode gust if present.
C
		IF ( nflds .eq. 4 ) THEN
		    IF ( itypsf(3) .eq. ALPHA .and.
     +				fields(3) .eq. 'G' ) THEN
			IF ( itypsf(4) .eq. NMR ) THEN
			    gststr = fields(4)
			    CALL ST_INTG ( gststr(1:lensf(4)),
     +					   ist1, ier )
			    IF ( ier .eq. 0 .and. ist1 .lt. 300 ) THEN
				rivals ( irgust ) = FLOAT ( ist1 )
			    END IF
			  ELSE
			    logmsg = 'Invalid (non-numeric) gust ' //
     +				     'field in wind column'        
			    CALL DC_WLOG ( 2, 'MA', 1, logmsg, ierwlg )
			    ipt  = iendwd + 2
			    RETURN
			END IF
		      ELSE
			logmsg = 'Invalid fields trailing wind speed'
			CALL DC_WLOG ( 2, 'MA', 1, logmsg, ierwlg )
			ipt  = iendwd + 2
			RETURN
		    END IF
		END IF
	    END IF
	  ELSE
C
C*	    Wind field is blank or missing
C
	    ipt = iendwd + 2
	    RETURN         
	END IF

	ipt = iendwd + 2 
C*
	RETURN
	END
