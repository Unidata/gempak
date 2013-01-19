	SUBROUTINE WC_CNTY ( cnties, lenc, county, ncoun, purge, iret )
C************************************************************************
C* WC_CNTY 								*
C*									*
C* This subroutine decodes the county information line.                	*
C*                                                                      *
C* WC_CNTY ( CNTIES, LENC, COUNTY, NCOUN, PURGE, IRET )                	*
C*									*
C* Input parameters:	                                                *
C*  	CNTIES          CHAR*           County string			*
C*	LENC  	  	INTEGER	  	Length of county string		*
C*									*
C* Output parameters:							*
C*  	COUNTY (NCOUN)  CHAR*6   	Array of County names 		*
C*	NCOUN           INTEGER	 	Number of counties in area	*
C*	PURGE           CHAR*    	Purge time (stop time, UTC)	*
C*	IRET  	  	INTEGER	 	Return code			*
C*					   -9 Error finding county line	*
C*									*
C**									*
C* Log:									*
C* A. Hardy/NCEP	10/02						*
C* A. Hardy/NCEP	 2/03		Changed log error level 2 -> 0	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
C*
	CHARACTER*(*)	cnties, county(*), purge
C------------------------------------------------------------------------
	iret  = 0
        purge = ' '
C
C*	Get the county information.
C
	CALL BR_CNTY ( cnties (:lenc-7), county, ncoun, ier ) 
        IF ( ier .ne. 0 ) THEN
            iret = -9
            CALL DC_WLOG ( 0, 'DCWCN', iret, ' ', ier )
            RETURN
        END IF
C
C*	Get the purge time.
C
	purge = cnties( lenc-6:lenc-1 )
C*
	RETURN
	END
