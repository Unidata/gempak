       SUBROUTINE MA_CGPR ( cgrpt, mszrpt, ipt, iret )
C************************************************************************
C* MA_CGPR                                                              *
C*                                                                      *
C* This subroutine decodes the altimeter/pressure and remarks fields in *
C* a report.  It determines whether a field is alpha, numeric, or other *
C* and calls MA_CGPT to decode the numeric fields (altimeter, pressure, *
C* max/min temp.) and calls MA_CGRM to decode the alpha fields (tide    *
C* level, wind gusts, ceiling, max wave heights, swell direction).      *
C*                                                                      *
C* MA_CGPR  ( CGRPT, MSZRPT, IPT, IRET )                                *
C*                                                                      *
C* Input parameters:                                                    *
C*      CGRPT           CHAR*           Report array                    *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to start of field.      *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01	Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_PRSN.	*
C*				Allowed for max/min temp following "M"	*
C*				in Remark section.  Changed sequence	*
C*				order of parameters in calls to	MA_CGRM	*
C*				and MA_CGPT.				*
C************************************************************************
 	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	cgrpt
C*
	CHARACTER*100	stprsn, outst 
	LOGICAL		prsdon, rmkdon
	INTEGER		fldnin, fldnou
C------------------------------------------------------------------------
	iret = 0
C
C*	Pressure, remarks, station name fields lie between ipt and 
C*	iendwd (inclusive).  
C*	Compress extra blanks out of field so only single blanks
C*	separate each substring.
C
	prsdon = .false.
	rmkdon = .false.
	stprsn = cgrpt(ipt:mszrpt)
	CALL ST_RXBL ( stprsn, outst, length, kret )
	IF ( length .gt. 1 ) THEN
C
C*	    Split wind group into 'like-type' groups to facilitate
C*	    decoding.
C
	    CALL MA_CGBG ( outst, iretbg )
	    IF ( iretbg .ne. 0 ) THEN
		ipt = mszrpt
		RETURN
	    END IF
	    IF ( nflds .eq. 0 ) THEN
		ipt = mszrpt
		RETURN
	      ELSE
		IF ( itypsf(1) .eq. ALPHA ) THEN
		    prsdon = .true.
		    IF ( lensf(1) .eq. 1 .and. fields(1) .eq. 'M' ) THEN
C
C*			Pressure field is set to 'M' (missing).
C*			Check 2nd field to see if alpha or numeric
C*			(or other) and call the appropriate s/r
C*			(if other, skip this field).
C
			prsdon = .true.
			fldnin = 2
			IF ( itypsf(2) .eq. ALPHA ) THEN
			    CALL MA_CGRM ( fldnin, prsdon, rmkdon,
     +					   fldnou, ier ) 
			  ELSE IF ( itypsf(2) .eq. NMR ) THEN
			    CALL MA_CGPT ( fldnin, prsdon, rmkdon, 
     +					   fldnou, ier ) 
			END IF 
			ipt = mszrpt
			RETURN
		      ELSE
C
C*			First field is either Remarks or Station Name.
C*			Decode Remarks.
C
			fldnin = 1
			CALL MA_CGRM ( fldnin, prsdon, rmkdon,
     +				       fldnou, ier ) 
			ipt = mszrpt
			RETURN
		    END IF
		  ELSE IF ( itypsf(1) .eq. NMR ) THEN
C
C*		    First field is either pressure or max/min temp.
C*		    in Remarks.  Check 2nd field to see if alpha or
C*		    numeric (or other) and call the appropriate s/r
C*		    (if other, skip this field).
C
		    fldnin = 1
		    CALL MA_CGPT ( fldnin, prsdon, rmkdon, 
     +				   fldnou, ier ) 
		    fldnin = fldnou
		    IF ( .not. rmkdon ) THEN
			IF ( fields(fldnin) .eq. 'M' )
     +				fldnin = fldnin + 1
			IF ( itypsf(fldnin) .eq. ALPHA ) THEN
			    CALL MA_CGRM ( fldnin, prsdon, rmkdon,
     +					   fldnou, ier ) 
			  ELSE IF ( itypsf(fldnin) .eq. NMR ) THEN
		    	    CALL MA_CGPT ( fldnin, prsdon, rmkdon, 
     +				   	   fldnou, ier ) 
			  ELSE
C
C*			    Character in this field may be a slash
C*			    trailing the alti/pres field, so check the
C*			    next field to see if it's remarks or the
C*			    station name.
C
			    IF ( itypsf(fldnin+1) .eq. ALPHA ) THEN
				fldnin = fldnin + 1
				CALL MA_CGRM ( fldnin, prsdon, rmkdon,
     +					       fldnou, ier ) 
			      ELSE IF ( itypsf(fldnin+1) .eq. NMR ) THEN
				fldnin = fldnin + 1
		    	    	CALL MA_CGPT ( fldnin, prsdon, rmkdon, 
     +				   	       fldnou, ier ) 
			      ELSE
C
C*				Non alpha-numeric character.  Skip
C*				this field.
C
			    END IF
			END IF 
			ipt = mszrpt
			RETURN
		    END IF
		  ELSE
C
C*		    Character in this field may be a slash trailing
C*		    the alti/pres field, so check the next field
C*		    to see if it's remarks or the station name.
C
		    fldnin = 1
		    IF ( itypsf(fldnin+1) .eq. ALPHA ) THEN
			fldnin = fldnin + 1
			CALL MA_CGRM ( fldnin, prsdon, rmkdon, 
     +				       fldnou, ier ) 
		      ELSE IF ( itypsf(fldnin+1) .eq. NMR ) THEN
			fldnin = fldnin + 1
		    	CALL MA_CGPT ( fldnin, prsdon, rmkdon, 
     +				       fldnou, ier ) 
		      ELSE
C
C*		    Non alpha-numeric character.  Skip this field.  
C
		    END IF
		END IF
		ipt = mszrpt
		RETURN
	    END IF
	  ELSE
C
C*	    Pressure/remarks/station name fields are blank or missing
C
	    ipt = mszrpt
	    RETURN         
	END IF

	ipt = mszrpt     
C*
	RETURN
	END
