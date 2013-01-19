      SUBROUTINE DSROAM  ( irmflg, ipwdth, iphght, iret )
C************************************************************************
C* DSROAM								*
C* 									*
C* This subroutine sets up the roaming on the current device.  		*
C* 									*
C* DSROAM  ( IRMFLG, IPWDTH, IPHGHT, IRET )				*
C* 									*
C* Input parameters:                                                    *
C*      IRMFLG          INTEGER         Roam flag                       *
C*      IPWDTH          INTEGER         Pixmap(device) width            *
C*      IPHGHT          INTEGER         Pixmap(device) height           *
C*                                                                      *
C* Output parameters:							*
C* 	IRET		INTEGER 	Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	6/97						*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'DEVCHR.CMN'
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Call driver
C
	CALL HSROAM  ( irmflg, ipwdth, iphght, iret )
C*
	RETURN
	END
