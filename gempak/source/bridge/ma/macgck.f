	SUBROUTINE MA_CGCK ( cgrpt, mszrpt, jret )
C************************************************************************
C* MA_CGCK                                                              *
C*                                                                      *
C* This subroutine checks the given Coast Guard report for strings to	*
C* indicate that it's either devoid of valid data or is not actually a	*
C* report line.  If any of the listed strings are found in the current	*
C* report, the return is set to 0.  If none of the listed strings are	*
C* found, the header line is assumed to be missing, so don't decode any *
C* of the reports in the bulletin.  The positions of the listed strings	*
C* are stored in common /block6/.					*
C*                                                                      *
C* MA_CGCK  ( CGRPT, MSZRPT, JRET )					*
C*                                                                      *
C* Input parameters:                                                    *
C*      CGRPT           CHAR*           Report to process               *
C*      MSZRPT          INTEGER         Length of report                *
C*                                                                      *
C* Output parameters:							*
C*      IWXVSB          INTEGER         indicates position of wx/vis    *
C*      IWIND           INTEGER         indicates position of wind      *
C*      IWAVE           INTEGER         indicates position of wave data *
C*      ISEA            INTEGER         indicates position of sst data  *
C*      IAIR            INTEGER         indicates position of air temp. *
C*      IPRES           INTEGER         indicates position of slp data  *
C*      IRMK            INTEGER         indicates position of remarks   *
C*      JRET            INTEGER         Return code                     *
C*                                        0 = normal return, i.e.       *
C*                                            report line               *
C*                                        1 = header line               *
C*                                       -1 = no header line found      *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01   New subroutine for Coast Guard data.    *
C* F. J. Yen/NCEP	 4/01	Cleaned up; rewrote IF statements to 	*
C*				save positions.  Renamed from CG_CKLN.	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE         'BRIDGE.PRM'
	INCLUDE         'macmn.cmn'
C*
	CHARACTER*(*)      cgrpt
C*
	CHARACTER*(DCMXBF) cgrpto
	LOGICAL		done
C-----------------------------------------------------------------------
C
C*	Check to see if gothdr was set to true previously in this s/r.
C*	If so, we've already found the header line, so no need to search
C*	for it again.  If gothdr isn't true, check to see if the current
C*	line is a valid header line or a line in a non-report bulletin.
C*	When checking to see if header line, only assume it's the header
C*	line if 4 or more of the header string were found in the line
C*	currently being checked.
C
	kret = 0
	IF ( gothdr ) THEN
	    jret = 0
	    RETURN
	  ELSE
	    jret = -1
C
C*          Look for header line.  Remove leading blanks, CHLF, & CHCR
C*          before checking.  Save positions of slashes. 
C
	    done = .false.
	    length = mszrpt
            i = 1
            DO WHILE ( .not. done .and. i .lt. mszrpt )
                IF ( cgrpt(i:i) .ne. CHLF .and. cgrpt(i:i) .ne. CHCR 
     +			.and. cgrpt(i:i) .ne. ' ' ) THEN
                    length = length - i
                    cgrpto(1:length) = cgrpt(i:mszrpt)
                    done = .true.
                END IF
                i = i + 1
            END DO
	    irmk = index(cgrpto(1:length),'/RMKS')
	    IF ( irmk .ne. 0)    kret = kret + 1
	    iwxvsb = index(cgrpto(1:length),'WXVSB')
	    IF ( iwxvsb .ne. 0)  kret = kret + 1
	    iwind = index(cgrpto(1:length),'/WIND')
	    IF ( iwind .ne. 0)   kret = kret + 1
	    ipres = index(cgrpto(1:length),'/PRES')
	    IF ( ipres .ne. 0)   kret = kret + 1
	    iwave = index(cgrpto(1:length),'/WAV') 
	    IF ( iwave .ne. 0)   kret = kret + 1
	    isea = index(cgrpto(1:length),'/SEA')
            IF ( isea .ne. 0)    kret = kret + 1
	    iair = index(cgrpto(1:length),'/AIR')
	    IF ( iair .ne. 0)    kret = kret + 1
C
C*	    Assume it's the header line if 4 or more of the header
C*	    strings  were found in the line currently being checked.
C*	    If 3 or less of the strings were found, reset the logicals
C*	    to false to prevent code from thinking it's found a header
C*	    in a future call (e.g. if the current bulletin is a
C*	    warning/restriction type bulletin, it's possible we could
C*	    find 4 of the strings in the whole bulletin, so resetting
C*	    the logicals to false if 3 or less were found will prevent
C*	    this s/r from falsely thinking it's found a header line).
C
	    IF ( kret .gt. 3 ) THEN
	        gothdr = .true.
	        jret = 1
	      ELSE
	        iwxvsb = 0
	        iwind  = 0
	        iwave  = 0
	        isea   = 0
	        iair   = 0
	        ipres  = 0
	        irmk   = 0
	    END IF
	END IF
C*
	RETURN
	END
