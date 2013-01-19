	SUBROUTINE HC_QUAD ( advise, sixty, fifty, thirty, seaft, 
     +			     fstype, iret )
C************************************************************************
C* HC_QUAD 								*
C*									*
C* This subroutine finds the wind speed quadrants and saves them to     *
C* individual strings.  Also the 12 foot sea information is saved, and  *
C* the storm type is determined from the wind speeds.  When decoding    *
C* JTWC typhoon advisories, the 64 kt wind parameters are used to store *
C* either the 100 kt quadrant wind speeds (for reports before 6/1/04)   *
C* or the 64 kt quadrant wind speeds (for reports on or after 6/1/04).  *
C*                                                                      *
C* HC_QUAD ( ADVISE, SIXTY, FIFTY, THIRTY, SEAFT, FSTYPE, IRET )	*
C*									*
C* Input parameters:							*
C*	ADVISE		CHAR*		Advisory wind string            *
C*									*
C* Output parameters:							*
C*	SIXTY		CHAR*		64 kt quadrant string		*
C*	FIFTY		CHAR*		50 kt quadrant string		*
C*	THIRTY		CHAR*		34 kt quadrant string		*
C*	SEAFT		CHAR*		12 ft seas quadrant string	*
C*	FSTYPE		CHAR*		Storm type                      *
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          5/00						*
C* D. Kidwell/NCEP	 7/01	Added storm type, check for zero winds  *
C* A. Hardy/NCEP        10/02   Modified check for 50 KT winds.		*
C* A. Hardy/NCEP	10/03	Added decoding JTWC 100, 50 & 34KT winds*
C* D. Kidwell/NCEP	 6/04	Added check for 64 KT JTWC winds        *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	advise, sixty, fifty, thirty, seaft, fstype
C*
	CHARACTER       advstr*(DCMXBF), naut*50
	LOGICAL		hvwnd
C------------------------------------------------------------------------
	iret = 0

        advstr = advise
        CALL ST_LSTR( advstr, ilen, iret ) 
	fstype = ' '
C
C*	Find the 64 KT wind.
C
        isix = INDEX( advstr(:ilen), '64 KT..' )
        IF ( isix .ne. 0 ) THEN
            inw = INDEX( advstr(isix:ilen), 'NW' )
            CALL HC_SPQD ( advstr(isix:isix+inw), naut, hvwnd, iret )
            sixty = naut
	    IF ( hvwnd ) fstype = 'HU'
          ELSE
	    hvwnd = .false.
        END IF
C
	IF ( .not. hvwnd ) THEN
C
C*	    Find the 100 KT or 64 KT wind for JTWC messages.
C
            isixty = INDEX( advstr(:ilen), 'RADIUS OF 100 KT WINDS' )
	    IF ( isixty .eq. 0 ) isixty = 
     +			INDEX( advstr(:ilen), 'RADIUS OF 064 KT WINDS' )
            irad = INDEX( advstr(:ilen), 'RADIUS OF 050' )
            IF (isixty .ne. 0 ) THEN
                isixty = isixty + 10
                CALL HC_SPQD ( advstr(isixty:irad), naut, hvwnd, 
     +                         iret)
                sixty = naut 
	        IF ( hvwnd .and. ( fstype .eq. ' ' ) ) fstype = 'HU'
              ELSE
	        hvwnd = .false.
            END IF
C
	    IF ( .not. hvwnd ) THEN
                sixty = 'MW -9999 -9999 -9999 -9999'
 	    END IF
	END IF
C
C*	Find the 50 KT wind.
C
        ifive = INDEX( advstr(:ilen), '. 50 KT..' )
        IF ( ifive .ne. 0 ) THEN
            inw = INDEX( advstr(ifive:ilen), 'NW' )
            CALL HC_SPQD ( advstr(ifive+2:ifive+inw),naut,hvwnd,iret )
            fifty = naut
	    IF ( hvwnd .and. ( fstype .eq. ' ' ) ) fstype = 'TS'
          ELSE
            ifive = INDEX( advstr(:ilen), 'RADIUS OF 050 KT WINDS' )
            irad = INDEX( advstr(:ilen), 'RADIUS OF 034' )
            IF (ifive .ne. 0 ) THEN
                ifive = ifive + 10
                CALL HC_SPQD ( advstr(ifive:irad), naut, hvwnd, 
     +                         iret)
                fifty = naut 
	        IF ( hvwnd .and. ( fstype .eq. ' ' ) ) fstype = 'TS'
              ELSE
	        hvwnd = .false.
            END IF
        END IF
	IF ( .not. hvwnd ) THEN
            fifty = '50 -9999 -9999 -9999 -9999'
	END IF
C
C*	Find the 34 KT wind.
C
        ithree = INDEX( advstr(:ilen), '34 KT..' )
        IF ( ithree .ne. 0 ) THEN
            inw = INDEX( advstr(ithree:ilen), 'NW' )
            CALL HC_SPQD ( advstr(ithree:ithree+inw), naut, hvwnd, iret)
            thirty = naut
	    IF ( hvwnd .and. ( fstype .eq. ' ' ) ) fstype = 'TS'
          ELSE 
            ithree = INDEX( advstr(:ilen), 'RADIUS OF 034 KT WINDS' )
            irad = INDEX( advstr(:ilen), '---' )
          
            IF (ithree .ne. 0 ) THEN
                inw = INDEX( advstr(ithree:), 'NORTHWEST QUADRANT')
                IF ( inw .ne. 0 ) THEN
                   inw = inw + 8
                 ELSE
                   inw = irad
                END IF
                ithree = ithree + 10 
                CALL HC_SPQD ( advstr(ithree:ithree+inw), naut, hvwnd, 
     +                         iret)
                thirty = naut
              ELSE
	        hvwnd = .false.
	        IF ( hvwnd .and. ( fstype .eq. ' ' ) ) fstype = 'TS'
            END IF
        END IF
	IF ( .not. hvwnd ) THEN
            thirty = '34 -9999 -9999 -9999 -9999'
	END IF
C
C*	Find the 12 foot seas.
C
        isea = INDEX( advstr(:ilen), '12 FT SEAS' )
        IF ( isea .gt. 0 ) THEN
            inw = INDEX( advstr(isea:ilen), 'NW' )
            CALL HC_SPQD ( advstr(isea:isea+inw), naut, hvwnd, iret )
            seaft = naut
          ELSE
            hvwnd = .false.
        END IF
	IF ( .not. hvwnd ) THEN
            seaft = '12 -9999 -9999 -9999 -9999'
	END IF
C
	IF ( fstype .eq. ' ' ) fstype = 'TD'
C*
	RETURN
	END
