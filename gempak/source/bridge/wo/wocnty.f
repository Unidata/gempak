	SUBROUTINE WO_CNTY ( cstrng, lenc, cnties, ncnty, purge, iret )
C************************************************************************
C* WO_CNTY 								*
C*									*
C* This subroutine decodes the county information line for the watch    *
C* outline updates.                					*
C*                                                                      *
C* WO_CNTY ( CSTRNG, LENC, CNTIES, NCNTY, PURGE, IRET )                	*
C*									*
C* Input parameters:	                                                *
C*  	CSTRNG          CHAR*           County string			*
C*	LENC  	  	INTEGER	  	Length of county string		*
C*									*
C* Output parameters:							*
C*  	CNTIES(NCNTY)   CHAR*6   	County names in watch area	*
C*	NCNTY           INTEGER	 	Number of counties in area	*
C*	PURGE           CHAR*   	Purge time (default stop time,  *
C*								UTC)	*
C*	IRET  	  	INTEGER	 Return code			 	*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	cstrng, cnties(*), purge
C------------------------------------------------------------------------
	iret  = 0
C
C*	Get the county information. 
C
	CALL BR_CNTY ( cstrng(:lenc-7), cnties, ncnty, iret ) 
C
C*	Get purge time.
C
	purge = cstrng ( lenc-6:lenc-1 )
C*
	RETURN
	END
