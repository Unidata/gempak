	SUBROUTINE MA_CGSC ( string, iret )
C************************************************************************
C* MA_CGSC                                                              *
C*                                                                      *
C* This subroutine decodes the sky cover data in one Coast Guard report.*
C* The values are store in common /rintfv/.				*
C*                                                                      *
C* MA_CGSC  ( STRING, IRET )                                            *
C*                                                                      *
C* Input parameters:                                                    *
C*      STRING          CHAR*           String containing sky cover data*
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRCFRT)  REAL            Fractional cloud coverage       *
C*      RIVALS(IRCLAM)  REAL            Cloud amount                    *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP	 4/01   Original Author                         *
C* F. J. Yen/NCEP	 4/01	Rewrote and renamed from CG_GTCL.	*
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	string
C*
	CHARACTER*4	sccod (6)
	INTEGER		nch (6), scfrt (6)
C*
	DATA	sccod	/ 'MCY', 'CY', 'CLDY', 'PC', 'MC', 'C' /
	DATA	scfrt	/    6.,   8.,     8.,   4.,   1.,  0. /
	DATA	nch	/    3 ,   2 ,     4 ,   2 ,   2 ,  1  /
C------------------------------------------------------------------------
	iret = 0
	ipos = 0
	i = 1
	DO WHILE ( ipos .eq. 0 .and. i .le. 6 )
	    ipos = index (string, sccod (i) (1:nch(i)))
	    IF ( ipos .ne. 0 ) THEN
		rivals(ircfrt)    = scfrt (i)
		rivals(irclam(1)) = scfrt (i)
	    END IF
	    i = i + 1
	END DO
C*
	RETURN
	END
