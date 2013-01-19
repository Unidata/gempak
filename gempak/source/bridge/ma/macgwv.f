       SUBROUTINE MA_CGWV ( cgrpt, mszrpt, ipt, iret )
C************************************************************************
C* MA_CGWV                                                              *
C*                                                                      *
C* This subroutine decodes the sky cover (cloud), weather, and          *
C* visibility data in a report.  It breaks the field down into its      *
C* cloud, weather, and vis components, then calls MA_CGSC, MA_CGWX, and *
C* MA_CGVS to decode the respective components.                         *
C*                                                                      *
C* MA_CGWV ( CGRPT, MSZRPT, IPT, IRET )                                 *
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
C*                                       -1 = No / found in rest of rpt *
C*                                       -2 = Misplaced slash           *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01	Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_WXVS.	*
C************************************************************************
	INCLUDE       	'macmn.cmn'
C*
	CHARACTER*(*)	cgrpt
C*
	CHARACTER*100 	stwxvs, outst 
	LOGICAL       	found
C------------------------------------------------------------------------
	iret = 0
	i = ipt
	found = .false.
C
C*	Look for '/' character.  Stop looping when the '/' we found is
C*	the field separator and not a '/' embedded in the wxvsb field.
C
	DO WHILE ( .not. found )
	    IF ( i .le. mszrpt) THEN
	        islash = index(cgrpt(i:i),'/')
	        IF (  islash .eq. 0  ) THEN
		    i = i + 1
	          ELSE
C
C*                Check to see if slash is too far to the right of the
C*		  1st slash in the header (located immediately before
C*		  'WIND').  If so, report has a bad format so return.
C
		    IF ( i .gt. iwind + 3 ) THEN
		        logmsg = ' Probable report format error -' //
     +                           ' skip this report'
	                CALL DC_WLOG  ( 2, 'MA', 1, logmsg, ierwlg )
		        iret = -2
		        RETURN
		      ELSE
C
C*		        Check to see if char right after '/' is letter,
C*		        number, or blank.  if number, THEN we're still
C*			in wxvsb section (number is a part of the
C*			visibility), so look for next slash to be field
C*			separator.  If letter or blank, THEN we've
C*			found the field separator between wxvsb and
C*			next parm.
C
		        CALL ST_ALNM ( cgrpt(i+1:i+1), ityp, jret )
		        IF ( ityp .eq. 2 .or. ityp .eq. 0 ) THEN
			    iendwx = i - 1
		            found = .true.
		          ELSE
C
C*			    Check to see if char right after '/' is
C*			    letter, number, or blank.  if number, THEN
C*			    we're still	in wxvsb section (number is is
C*			    at least one column left of '/WIND' in
C*			    header, and it's not preceded by a blank,
C*			    then it's the slash in a fractional
C*			    visibility report.
C
			    IF ( i .le. (iwind - 1) .and.
     +				    cgrpt(i-1:i-1) .ne. ' ' ) THEN
			        i = i + 1
			      ELSE
			        iendwx = i - 1
			        found = .true.
			    END IF
		        END IF
		    END IF
	        END IF
	      ELSE
	        iret = -1
	        RETURN
	    END IF
	END DO
C
C*	Wxvsb field lies between ipt and iendwx.  
C*	Compress blanks out of wxvsb field.
C
	wxvsav = ' '
	iwxvln = 1
	stwxvs = cgrpt(ipt:iendwx)
	CALL ST_RMBL ( stwxvs, outst, length, kret )
	IF ( length .ge. 1 .and. outst(1:length) .ne. 'M' ) THEN
C
C*          Decode cloud cover.  ( SC = sky cover )
C
	    CALL MA_CGSC ( outst(1:length), lret )
C
C*          Decode weather.
C
            CALL MA_CGWX ( outst(1:length), mret )
	    wxvsav = outst(1:length)
	    iwxvln = length
C
C*          Decode visibility.
C
            CALL MA_CGVS ( outst(1:length), length, nret )
	  ELSE
C
C*          Wxvsb is blank or missing. Skip forward 2 columns in this
C*	    report and return to decode the next parm.
C
	    ipt = iendwx + 2
            RETURN         
	END IF
	ipt = iendwx + 2
C*
	RETURN
	END
