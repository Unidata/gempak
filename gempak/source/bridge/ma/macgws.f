	SUBROUTINE MA_CGWS ( string, len, iret )
C************************************************************************
C* MA_CGWS                                                              *
C*                                                                      *
C* This subroutine decodes the wind speed data in one report. The	*
C* wind speed and source units are saved in common /rintfv/.		*
C*                                                                      *
C* MA_CGWS  ( STRING, LEN, IRET )                                       *
C*                                                                      *
C* Input parameters:                                                    *
C*      STRING          CHAR*           string containing sky cover data*
C*      LEN             INTEGER         length of string                *
C*                                                                      *
C* Output parameters:                                                   *
C*      RIVALS(IRSKNT)  REAL            wind speed (kts)                *
C*      RIVALS(IRSUWS)  REAL            source units for wind speed     *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = Normal return             *
C*                                       -1 = invalid wind speed        *
C**                                                                     *
C* Log:                                                                 *
C* C. Caruso Magee/NCEP  4/01   Original Author                         *
C* F. J. Yen/NCEP	 4/01	Cleaned up and renamed from CG_GTWS.	*
C************************************************************************
	INCLUDE		'macmn.cmn'
C*
	CHARACTER*(*)	string
C------------------------------------------------------------------------
	iret = 0
	CALL ST_INTG ( string(1:len), iwnspd, iret )
	rivals(irsknt) = FLOAT ( iwnspd )
	rivals(irsuws) = 4.
	IF ( rivals(irsknt) .lt. 0.0 .or.
     +		rivals(irsknt) .gt. 300. ) THEN
	    iret = -1
	END IF
C*
	RETURN
	END
