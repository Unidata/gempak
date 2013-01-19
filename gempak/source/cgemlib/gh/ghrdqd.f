	SUBROUTINE GH_RDQD ( advise, sixty, fifty, thirty, seaft, iret )
C************************************************************************
C* GH_RDQD 								*
C*									*
C* This subroutine finds the wind speed quadrants and saves them to     *
C* individual strings.  Also the 12 foot sea information is saved.      *
C*                                                                      *
C* GH_RDQD ( ADVISE, SIXTY, FIFTY, THIRTY, SEAFT, IRET )		*
C*									*
C* Input parameters:							*
C*	ADVISE		CHAR*		Advisory wind string            *
C*									*
C* Output parameters:							*
C*	SIXTY		CHAR*		64 kt quadrant string		*
C*	FIFTY		CHAR*		50 kt quadrant string		*
C*	THIRTY		CHAR*		34 kt quadrant string		*
C*	SEAFT		CHAR*		12 ft seas quadrant string	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC          5/00						*
C* A. Hardy/NCEP	10/02	Fixed INDEX find for '50 KT'		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	advise, sixty, fifty, thirty, seaft
C*
	CHARACTER       advstr*300, naut*50
C------------------------------------------------------------------------
	iret = 0

        advstr = advise
        CALL ST_LSTR( advstr, ilen, iret) 
C
C*	Find the 64 KT wind.
C
        isix = INDEX( advstr(:ilen), '64 KT..')
        IF ( isix .ne. 0 ) THEN
            inw = INDEX( advstr(isix:ilen), 'NW')
            CALL GH_RDSP ( advstr(isix:isix+inw), naut, iret)
            sixty = naut
          ELSE
            sixty = '64 -9999 -9999 -9999 -9999'
        ENDIF
C
C*	Find the 50 KT wind.
C
        ifive = INDEX( advstr(:ilen), '. 50 KT..')
        IF ( ifive .ne. 0 ) THEN
            inw = INDEX( advstr(ifive:ilen), 'NW')
            CALL GH_RDSP ( advstr(ifive+2:ifive+inw), naut, iret)
            fifty = naut
          ELSE
            fifty = '50 -9999 -9999 -9999 -9999'
        ENDIF
C
C*	Find the 34 KT wind.
C
        ithree = INDEX( advstr(:ilen), '34 KT..')
        IF ( ithree .ne. 0 ) THEN
            inw = INDEX( advstr(ithree:ilen), 'NW')
            CALL GH_RDSP ( advstr(ithree:ithree+inw), naut, iret)
            thirty = naut
          ELSE
            thirty = '34 -9999 -9999 -9999 -9999'
        ENDIF
C
C*	Find the 12 foot seas.
C
        isea = INDEX( advstr(:ilen), '12 FT SEAS')
        IF ( isea .gt. 0 ) THEN
            inw = INDEX( advstr(isea:ilen), 'NW')
            CALL GH_RDSP ( advstr(isea:isea+inw), naut, iret)
            seaft = naut
          ELSE
            seaft = '12 -9999 -9999 -9999 -9999'
        ENDIF
C
	RETURN
	END
