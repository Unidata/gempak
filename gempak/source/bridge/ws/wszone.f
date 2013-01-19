	SUBROUTINE WS_ZONE ( zstrng, lenz, zones, nzone, purge, iret )
C************************************************************************
C* WS_ZONE 								*
C*									*
C* This subroutine decodes the zone information line.                	*
C*                                                                      *
C* WS_ZONE ( ZTRNG, LENZ, ZONES, NZONE, PURGE, IRET )                  	*
C*									*
C* Input parameters:	                                                *
C*  	ZSTRNG          CHAR*           Zone string			*
C*	LENZ  	  	INTEGER	  	Length of zone string		*
C*									*
C* Output parameters:							*
C*  	ZONES (NZONE)   CHAR*6   Zone names in storm area		*
C*	NZONE           INTEGER	 Number of zones in area		*
C*	PURGE           CHAR*    Purge time (default stop time, UTC)	*
C*	IRET  	  	INTEGER	 Return code			 	*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		08/02						*
C* M. Li/SAIC		08/02	WN_CNTY -> BR_CNTY			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	zstrng, zones(*), purge
C------------------------------------------------------------------------
	iret  = 0
C
C*	Get zones
C
	CALL BR_CNTY ( zstrng(:lenz-7), zones, nzone, iret ) 
C
C*	Get purge time
C
	purge = zstrng ( lenz-6:lenz-1 )
C*
	RETURN
	END
