	SUBROUTINE WO_HDLN ( hstrng, wtype, stmid, iret )
C************************************************************************
C* WO_HDLN 								*
C*									*
C* This subroutine gets the watch type, tornado or thunderstorm.	*
C*                                                                      *
C* WO_HDLN ( HSTRNG, WTYPE, STMID, IRET )          			*
C*									*
C* Input parameters:	                                                *
C*  	HSTRNG          CHAR*           Headline string			*
C*									*
C* Output parameters:							*
C*  	WTYPE           CHAR*           Watch type			*
C*      STMID           CHAR*		Watch number			*
C*	IRET  	  	INTEGER	 	Return code			* 
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP 	10/02						*
C* A. Hardy/NCEP         4/03 	Created a check for finding watch number*
C* F. J. Yen/NCEP	10/06	Initialized stmid.			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	hstrng, wtype, stmid
C------------------------------------------------------------------------
	iret  = 0
	wtype = ' '
C
C*	Get watch type. 
C
	ix = INDEX ( hstrng, 'THUNDERSTORM' )
	IF ( ix .gt. 0 ) THEN 
            wtype = 'TSM'
          ELSE IF ( ix .le. 0 ) THEN
	    ix = INDEX ( hstrng, 'TORNADO' )
            IF ( ix .gt. 0 ) THEN
                wtype = 'TOR'
              ELSE IF ( ix .le. 0 ) THEN
                wtype = ' '
            END IF
        END IF
C
C* 	Get the watch number for thunderstorms or tornadoes.
C
	ix = INDEX ( hstrng, 'WS' )
        IF ( ix .eq. 0 ) THEN
	    ix = INDEX ( hstrng, 'WT' )
        END IF
C
	stmid = ' '
	IF ( ix .gt. 0 ) THEN
C
            CALL ST_ALNM ( hstrng (ix+3:ix+3), itype1, ier)
            CALL ST_ALNM ( hstrng (ix+4:ix+4), itype2, ier)
            CALL ST_ALNM ( hstrng (ix+5:ix+5), itype3, ier)
            CALL ST_ALNM ( hstrng (ix+6:ix+6), itype4, ier)
C
            IF ( itype1 .eq. 1 ) THEN
                IF ( itype2 .eq. 1 ) THEN
                    IF ( itype3 .eq. 1 ) THEN
                        IF ( itype4 .eq. 1 ) THEN
                            stmid = hstrng (ix+3:ix+6)
                          ELSE
                            stmid = '0' // hstrng (ix+3:ix+5)
                        END IF
                      ELSE
                        stmid = '00' // hstrng (ix+3:ix+4)
                    END IF
                  ELSE
                    stmid = '000' // hstrng (ix+3:ix+3)
                END IF 
            END IF
	END IF
C*
	RETURN
	END
