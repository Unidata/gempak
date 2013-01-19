	SUBROUTINE MA_CGWA ( cgrpt, mszrpt, ipt, iret )
C************************************************************************
C* MA_CGWA                                                              *
C*                                                                      *
C* This subroutine decodes the wave data in one report.  The values are *
C* stored in common /rintfv/.						*
C*                                                                      *
C* MA_CGWA  ( CGRPT, MSZRPT, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      CGRPT           CHAR*           Report array                    *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to start of field.      *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRWHGT)  REAL            Wave height (meters)            *
C*      RIVALS(IRWPER)  REAL            Wave period (seconds)           *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       -1 = No / found in rest of rpt *
C*                                       -2 = Invalid field length      *
C*                                       -3 = Invalid field             *
C*                                       -4 = Too many fields           *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP  4/01	Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up, reformatted, and renamed	*
C*				from CG_WAVE.				*
C* F. J. Yen/NCEP	 7/01	Replaced uninitialized variable "length"*
C*				with lensf(1). (S. Chiswell/Unidata)	*
C* D. Kidwell/NCEP	 3/02	Converted whgt to meters, cleaned up    *
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	cgrpt
C*
	CHARACTER*100	stwave 
	LOGICAL		found
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
		    iendwv = i - 1
		    found = .true.
		END IF
     	      ELSE
		iret = -1
		RETURN
	    END IF
	END DO
C
C*	Wave field lies between ipt and iendwv (inclusive).  
C*	Compress blanks out of wave field.
C
	stwave = cgrpt(ipt:iendwv)
	CALL MA_CGBG ( stwave, kret )
	IF ( nflds .eq. 0) THEN
C
C*	    Wave field is blank (missing)
C
	  ELSE IF ( nflds .eq. 1 ) THEN
	    IF ( itypsf(1) .eq. ALPHA ) THEN
C
C*	        Wave field is chars. If CALM or FLAT, set whgt and wper,
C*		otherwise skip this field (may be missing ('M'), 'CHOP',
C*		or invalid chars).
C
		IF ( fields(1) .eq. 'CALM' .or.
     +		     fields(1) .eq. 'FLAT' ) THEN
		    rivals(irwhgt) = 0.
		    rivals(irwper) = 0.
		  ELSE IF ( fields(1) .eq. 'M' .or.
     +			   fields(1) .eq. 'CHOP' ) THEN
		  ELSE
		    logmsg = '(Non-fatal) '//cgrpt(ipt:iendwv)
		    CALL DC_WLOG  ( 2, 'MA', -5, logmsg, ierwlg )
		END IF
	      ELSE IF ( itypsf(1) .eq. NMR ) THEN
C
C*		Wave field is numbers.  If 0, set whgt and wper. Check
C*		length if non-zero.  If length is 1 or 3, skip this
C*		field (invalid length).  If length is 2 or 4, set whgt
C*		if 2, whgt and wper if 4. If length is gt 4, set error
C*		message and return (invalid field, possibly decode
C*		error).
C
		IF ( fields(1) .eq. '0' .or. fields(1) .eq. '00' .or.
     +		     fields(1) .eq. '000' .or. fields(1) .eq. '0000' )
     +		     THEN
		    rivals(irwhgt) = 0.
		    rivals(irwper) = 0.
		  ELSE
		    IF ( lensf(1) .eq. 1 .or. lensf(1) .eq. 3 ) THEN
		      ELSE IF ( lensf(1) .eq. 2 ) THEN
			CALL ST_INTG ( fields(1)(1:2), ist1, ier )
			rivals(irwhgt) = FLOAT(ist1)
		      ELSE IF ( lensf(1) .eq. 4 ) THEN
			CALL ST_INTG ( fields(1)(1:2), ist1, ier )
			CALL ST_INTG ( fields(1)(3:4), ist2, ier )
			rivals(irwhgt) = FLOAT(ist1)
			rivals(irwper) = FLOAT(ist2)
		      ELSE IF ( lensf(1) .gt. 4) THEN
			logmsg = cgrpt(ipt:iendwv)
			CALL DC_WLOG  ( 2, 'MA', -5, logmsg, ierwlg )
			iret = -2
			RETURN
		    END IF
		END IF
	      ELSE
C
C*		Wave field is neither alpha or numeric.
C*		Set error message and return (invalid field,
C*		possibly decode error).
C
		logmsg = cgrpt(ipt:iendwv)
		CALL DC_WLOG  ( 2, 'MA', -5, logmsg, ierwlg )
		iret = -3
		RETURN
	    END IF
	  ELSE IF ( nflds .eq. 2 ) THEN
C
C*	    Two fields, possibly whgt and wper separated by a blank.  If
C*	    both fields are numeric and length is less than 3, decode, 
C*	    otherwise set error message and return (invalid fields,
C*	    possibly decode error).
C
	    IF ( itypsf(1) .eq. NMR .and. itypsf(2) .eq. NMR .and.
     +		 lensf(1) .lt. 3 .and. lensf(2) .lt. 3 ) THEN
		CALL ST_INTG ( fields(1)(1:lensf(1)), ist1, ier )
		CALL ST_INTG ( fields(2)(1:lensf(2)), ist2, ier )
		rivals(irwhgt) = FLOAT(ist1)
		rivals(irwper) = FLOAT(ist2)
	      ELSE
		logmsg = cgrpt(ipt:iendwv)
		CALL DC_WLOG  ( 2, 'MA', -5, logmsg, ierwlg )
		iret = -3
		RETURN
	    END IF
	  ELSE
C
C*	    Too many fields to be valid wave field.
C*	    Set error message and return (invalid fields,
C*	    possibly decode error).
C
	    logmsg = cgrpt(ipt:iendwv)
	    CALL DC_WLOG  ( 2, 'MA', -5, logmsg, ierwlg )
	    iret = -4
	    RETURN
	END IF
C
	rivals(irwhgt) = PR_HGFM ( rivals(irwhgt) )
	ipt = iendwv + 2 
C*
	RETURN
	END
