	SUBROUTINE MA_CGRM (fldnin, prsdon, rmkdon, fldnou, ier)
C************************************************************************
C* MA_CGRM                                                              *
C*                                                                      *
C* This subroutine decodes the character remarks fields in a single     *
C* Coast Guard report.  Parameters used which are not in the calling	*
C* sequence are found in macmn.cmn.					*
C*                                                                      *
C* MA_CGRM  ( FLDNIN, PRSDON, RMKDON, FLDNOU, IER )              	*
C*                                                                      *
C* Input parameters:                                                    *
C*      FIELDS          CHAR*(*)        Array of fields found in input  *
C*                                      string                          *
C*      LENSF           INTEGER         Array of lengths of fields      *
C*      FLDNIN          INTEGER         Number of field to work on.     *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      PRSDON          LOGICAL         if true, pressure field has been*
C*                                      decoded.                        *
C*      RMKDON          LOGICAL         if true, remarks fields have    *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRTERC)  REAL            tidal elev. relative to local   *
C*                                      chart (inches)                  *
C*      RIVALS(IRHOCB)  REAL            height of cloud base (meters)   *
C*      RIVALS(IRGUST)  REAL            max. wind speed (gust) (kts)    *
C*      RIVALS(IRMXWH)  REAL            maximum wave height (ft)        *
C*      RIVALS(IRCORN)  REAL            correction indicator            *
C*	FLDNOU		INTEGER		Number of next field to work on *
C*      IER             INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       non-zero = Problem             *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01   Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up, reformatted and renamed from*
C*				CG_RMKA.  Added additional check for  	*
C*				another numeric field. Added RAIN, HAZE *
C*				and FOG. Kept value of fldnin unchanged.*
C*				Changed prologue and sequence order of	*
C*				parameters.  Added more comments.	*
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	INTEGER		fldnin, fldnou
	LOGICAL		prsdon, rmkdon, MA_FIND
C*
	CHARACTER*5	swls (3)
	CHARACTER*3     dirtn (16)
C*
	DATA		swls  / 'SWELL', 'SWL', 'SWEL' /
	DATA		dirtn / 'SSW', 'WSW', 'WNW', 'NNW',
     +				'NNE', 'ENE', 'ESE', 'SSE',
     +				'SE',  'SW',  'NW',  'NE',
     +				'S',   'W',   'N',   'E'   /
C------------------------------------------------------------------------
	ier = 0
	i = fldnin
	fldnou = fldnin
	IF ( lensf(i) .eq. 5 ) THEN
	    IF ( fields(i) .eq. 'GUSTY' ) THEN
		fldnou = fldnin + 1
		rmkdon = .true.
		RETURN
	      ELSE IF ( fields(i) .eq. 'MINUS' .and. 
     +			i + 1 .le. nflds) THEN
C
C*	        Decode tidal elevation
C
		CALL ST_INTG ( fields(i+1)(1:lensf(i+1)), ist1, ier )
		IF ( ier .eq. 0 .and. ist1 .lt. 1000 ) THEN
		    rivals ( irterc ) = FLOAT( -ist1 )
	          ELSE
		    WRITE  ( UNIT = logmsg, FMT = '( A )' )
     +			  ' Invalid group/format error in char remarks'
		    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
	        END IF
		fldnou = fldnin + 2
	    END IF
	  ELSE IF ( lensf(i) .eq. 4 ) THEN
	    IF ( fields(i) .eq. 'CEIL' .and.
     +			i + 1 .le. nflds ) THEN
C
C*	        Decode ceiling (height of cloud base)
C
		IF ( itypsf(i+1) .eq. ALPHA ) THEN
		    IF ( fields(i+1) .eq. 'UNL' ) THEN
			fldnou = fldnin + 2
		      ELSE
			fldnou = fldnin + 1
		    END IF
		  ELSE IF ( itypsf(i+1) .eq. NMR ) THEN
		    IF ( lensf(i+1) .eq. 3 ) THEN
C
C*	                Ceiling is in hundreds of feet, so convert in
C*			feet, then to meters before saving into irhocb.
C
			CALL ST_INTG ( fields(i+1)(1:lensf(i+1)),
     +				       ist1, ier )
			IF ( ier .eq. 0 ) THEN
			    rivals ( irhocb(1) ) =
     +				    FLOAT( ist1 ) * 100./3.28
			    fldnou = fldnin + 2
			END IF 
		      ELSE
			fldnou = fldnin + 2
		    END IF 
		  ELSE
		    fldnou = fldnin + 2
		END IF
	      ELSE IF ( fields(i) .eq. 'GUST' .and.
     +			i + 1 .le. nflds ) THEN
		IF ( itypsf(i+1) .eq. NMR ) THEN
C
C*		    Decode maximum wind speed
C
		    CALL ST_INTG ( fields(i+1)(1:lensf(i+1)),
     +				   ist1, ier )
		    IF ( ier .eq. 0 .and. ist1 .lt. 300 ) THEN
			rivals ( irgust ) = FLOAT ( ist1 )
		    END IF
		    fldnou = fldnin + 2
		  ELSE
		    WRITE  ( UNIT = logmsg, FMT = '( A )' )
     +			' Invalid group/format error in char remarks'
		    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		    fldnou = fldnin + 1
		END IF
	      ELSE IF ( fields(i) .eq. 'PLUS' .and.
     +			i + 1 .le. nflds ) THEN
C
C*	        Decode tidal elevation
C
		CALL ST_INTG ( fields(i+1)(1:lensf(i+1)), ist1, ier )
		IF ( ier .eq. 0 .and. ist1 .lt. 1000 ) THEN
		    rivals ( irterc ) = FLOAT ( ist1 )
		  ELSE
		    WRITE  ( UNIT = logmsg, FMT = '( A )' )
     +			' Invalid group/format error in char remarks'
		    CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		END IF
		fldnou = fldnin + 2
	      ELSE IF ( fields(i) .eq. 'HAZE' .or. 
     +		       fields(i) .eq. 'RAIN' ) THEN
		IF ( iwxvln .lt. 20 ) THEN
C
C*		    Append haze or rain to the weather visibility
C*		    text string and redecode the weather phenomenon
C
		    wxvsav = wxvsav(1:iwxvln) // fields (i) (1:1)
		    iwxvln = iwxvln + 1
		    CALL MA_CGWX ( wxvsav(1:iwxvln), ier )
		END IF
		fldnou = fldnin + 1
	    END IF
	  ELSE IF ( lensf(i) .eq. 3 ) THEN 
	    IF ( fields(i) .eq. 'SCA' .or.
     +		 fields(i) .eq. 'UNL') THEN
		fldnou = fldnin + 1
		rmkdon = .true.
		RETURN
	      ELSE IF ( fields(i) .eq. 'COR' ) THEN
		rivals ( ircorn ) = 1.
		fldnou = fldnin + 1
	      ELSE IF ( fields(i) .eq. 'MAX' .and.
     +			i + 1 .le. nflds ) THEN
		IF ( itypsf(i+1) .eq. NMR ) THEN
C
C*	            Decode maximum wave height
C
		    CALL ST_INTG ( fields(i+1)(1:lensf(i+1)),
     +				   ist1, ier )
		    IF ( ier .eq. 0 .and. ist1 .lt. 200 ) THEN
			rivals ( irmxwh ) = FLOAT ( ist1 )
		    END IF
		    fldnou = fldnin + 3
		  ELSE
		    fldnou = fldnin + 1
		END IF
C
C*	      Test for presence of direction text strings and for
C*	      possible swell text strings  
C
	      ELSE IF ( MA_FIND ( fields(i), dirtn, 8 ) .and.
     +			i + 1 .le. nflds ) THEN
		IF ( MA_FIND ( fields(i+1), swls, 3 ) ) THEN 
C
C*	            Decode the swell direction
C
		    CALL MA_CGWD ( fields(i), swls(1), iret )
		    fldnou = fldnin + 2
		  ELSE
		    fldnou = fldnin + 1
		END IF
	      ELSE IF ( fields(i) .eq. 'FOG' ) THEN
		IF ( iwxvln .lt. 20 ) THEN
C
C*		    Append fog to the weather visibility
C*		    text string and redecode the weather phenomenon
C
		    wxvsav = wxvsav(1:iwxvln) // fields (i) (1:1)
		    iwxvln = iwxvln + 1
		    CALL MA_CGWX ( wxvsav(1:iwxvln), ier )
		END IF
		fldnou = fldnin + 1
	    END IF
	  ELSE IF ( lensf(i) .eq. 2 ) THEN
C
C*	    Test for presence of direction text strings and for
C*	    possible swell text strings  
C
	    IF ( MA_FIND ( fields(i), dirtn(9), 4 ) .and.
     +			i + 1 .le. nflds ) THEN
		IF ( MA_FIND ( fields(i+1), swls, 3 ) ) THEN 
C
C*	            Decode the swell direction
C
                    CALL MA_CGWD ( fields(i), swls(1), iret )
                    fldnou = fldnin + 2
                  ELSE
                    fldnou = fldnin + 1
                END IF
	      ELSE IF ( fields(i) .eq. 'MX' .and.
     +			i + 1 .le. nflds ) THEN
C
C*	        Decode maximum wave height
C
		IF ( itypsf(i+1) .eq. NMR ) THEN
		    CALL ST_INTG ( fields(i+1)(1:lensf(i+1)),
     +				   ist1, ier )
		    IF ( ier .eq. 0 .and. ist1 .lt. 200 ) THEN
			rivals ( irmxwh ) = FLOAT ( ist1 )
			fldnou = fldnin + 3
		      ELSE
			fldnou = fldnin + 3
		    END IF
		  ELSE
		    fldnou = fldnin + 1
		END IF
	    END IF
C
C*	  Test for presence of direction text strings and for
C*	  possible swell text strings  
C
	  ELSE IF ( lensf(i) .eq. 1 .and.
     +                  MA_FIND ( fields(i), dirtn(13), 4 ) .and.
     +			i + 1 .le. nflds ) THEN
	    IF ( MA_FIND ( fields(i+1), swls, 3) ) THEN 
C
C*	        Decode the swell direction
C
		CALL MA_CGWD ( fields(i), swls(1), iret )
		fldnou = fldnin + 2
              ELSE
		fldnou = fldnin + 1
	    END IF
	  ELSE IF ( lensf(i) .eq. 1 .and.
     +			( fields(i) .eq. 'G' ) .and.
     +			i + 1 .le. nflds ) THEN
	    IF ( itypsf(i+1) .eq. NMR ) THEN
C
C*	  	Decode maximum wind speed
C
		CALL ST_INTG ( fields(i+1)(1:lensf(i+1)), ist1, ier )
		IF ( ier .eq. 0 .and. ist1 .lt. 300 ) THEN
		    rivals ( irgust ) = FLOAT ( ist1 )
		END IF
		fldnou = fldnin + 2
	      ELSE
		fldnou = fldnin + 1
	    END IF
	END IF
	IF ( .not. rmkdon ) THEN
C
C*	    Check to see if next field (or initial field if no match
C*	    found) is alpha or numeric.  If numeric, it's more remarks,
C*	    else it's station name or a typo.
C		
	    IF ( itypsf(fldnou) .eq. NMR ) THEN
		IF ( fldnou .le. nflds ) THEN
		    infld = fldnou
		    CALL MA_CGPT ( infld, prsdon, rmkdon, fldnou, ier )
		  ELSE
		    rmkdon = .true.
		END IF
	      ELSE
		fldnou = fldnin
		rmkdon = .true.
	    END IF
	END IF
C*
	RETURN
	END
