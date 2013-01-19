       SUBROUTINE MA_CGST ( cgrpt, mszrpt, ipt, iret )
C************************************************************************
C* MA_CGST                                                              *
C*                                                                      *
C* This subroutine decodes the sea surface temperature in one report.   *
C* The value is stored in common /rintfv/.				*
C*                                                                      *
C* MA_CGST   ( CGRPT, MSZRPT, IPT, IRET )                               *
C*                                                                      *
C* Input parameters:                                                    *
C*      CGRPT           CHAR*           Report array                    *
C*      MSZRPT          INTEGER         Length of report in bytes       *
C*                                                                      *
C* Input and Output parameters:                                         *
C*      IPT             INTEGER         Pointer to start of field.      *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRSSTF)  REAL            sea surface temp. (deg F)       *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       -1 = No / found in rest of rpt *
C*                                       -2 = Invalid field length      *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01	Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_SST.	*
C*				Corrected prologue.			*
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	cgrpt
C*
	CHARACTER*100	stsst, outst 
	LOGICAL		found
C------------------------------------------------------------------------
       iret = 0
       i = ipt
       found = .false.
C
C*     Look for '/' character.  Stop looping when the '/' we found is
C*     the field separator.                                            
C
	DO WHILE ( .not. found )
	    IF ( i .le. mszrpt ) THEN
		islash = index(cgrpt(i:i),'/')
		IF ( islash .eq. 0 ) THEN
		    i = i + 1
		  ELSE
		    iendst = i - 1
		    found = .true.
		END IF
	      ELSE
		iret = -1
		RETURN
	    END IF
	END DO
C
C*	SST field lies between ipt and iendst (inclusive).  
C*	Compress blanks out of sst field.
C
	stsst = cgrpt(ipt:iendst)
	CALL ST_RMBL ( stsst, outst, length, kret )
	IF ( length .ge. 1 .and. length .le. 3 ) THEN
	    IF ( outst .ne. 'M' ) THEN
		CALL ST_INTG ( outst(1:length), ist1, kret )
		IF ( kret .eq. 0 ) THEN
		    rivals(irsstf) = FLOAT(ist1) 
		  ELSE
C 
C*		    Invalid field/bad format for sst.
C 
		    logmsg = 'Invalid field/bad format ' //
     +                       outst(1:length) // ' in sst field'
		    CALL DC_WLOG ( 2, 'MA', 1, logmsg, ierwlg )
		END IF
	    END IF
	  ELSE IF ( length .gt. 3 ) THEN
C 
C*	    Bad format for SST.  Toss the whole report since we may be
C*	    off by a field in decoding.
C 
	    logmsg = '(in sst) '// outst(1:length)
	    CALL DC_WLOG ( 2, 'MA', -3, logmsg, ierwlg )
	    iret = -2
	    RETURN
	END IF
	ipt = iendst + 2
C*
	RETURN
	END
