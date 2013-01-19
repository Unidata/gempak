	SUBROUTINE MT_VISB ( strvs1, strvs2, ivis, idecd, twofld, 
     +			     dirvis, iret)
C************************************************************************
C* MT_VISB                                                              *
C*                                                                      *
C* This subroutine will decode up to two visibility groups.  The	*
C* values are stored in common.						*
C* 								        *
C* MT_VISB ( STRVS1, STRVS2, IVIS, IDECD, TWOFLD, DIRVIS, IRET )	*
C*								        *
C* Input parameters: 						        *
C*      STRVS1		CHAR*		Visibility field       		*
C*      STRVS2		CHAR*		Fractional part of visibility	*
C*	IVIS		INTEGER		Relative number of vis. group   *
C*								        *
C* Output parameters:						        *
C*	RIVALS(IRVSBY)  REAL            Horizontal visibility (sm)      *
C*	RIVALS(IRVSBK)  REAL            Horizontal visibility (km)      *
C*	RIVALS(IRVSFL)  REAL            Visibility P or M indicator     *
C*	RIVALS(IRNVSB)  REAL            Number of visibility groups     *
C*	IDECD		INTEGER		Decode decision flag            *
C*					  2 = auto report with field	*
C*						decoded having slashes	*
C*					  1 = field decoded     	*
C*					  0 = field not decoded		*
C*	TWOFLD		LOGICAL		Two field flag			*
C*	DIRVIS		LOGICAL		Directional visibility flag	*
C*	IRET		INTEGER		Return code                     *
C*	      		    		  0 = normal return		*
C*					  9 = miscoded field		*
C*	                                 -1 = no field found		*
C**								        *
C* Log:									*
C* D. Kidwell/NCEP 	 4/95                                           *
C* D. Kidwell/NCEP    	 6/95   Added international group / meters      *
C* K. Tyle/GSC		 1/97	Set "M1/4SM" to 0; correctly decode 	*
C*				some miscoded fractional visibilities;	*
C*				change call to DC_WLOG; reorganize	*
C*				header and comments			*
C* K. Tyle/GSC		 2/97	Changed error processing		*
C* D. Kidwell/NCEP    	 5/97   Removed ERMISS reference to integer arg *
C* D. Kidwell/NCEP    	 6/97   ST_LSTR -> INDEX and ST_CRNM -> ST_INTG *
C* D. Kidwell/NCEP    	 6/97   Added check for string of length .ge. 40*
C* D. Kidwell/NCEP    	 4/98   New interface; reinstate "M1/4SM" as ok *
C* A. Hardy/GSC          7/98   Added ability to decode vis. in KM      *
C* D. Kidwell/NCEP    	 9/02   Moved most of code to BR_VISB           *
C* F. J. Yen/NCEP	 2/04	Updated call to BR_VISB due to CSC.	*
C************************************************************************
	INCLUDE 	'GEMPRM.PRM'
	INCLUDE 	'mtcmn.cmn'
C*
	CHARACTER*(*) 	strvs1, strvs2
	LOGICAL 	twofld, dirvis
C*
	INCLUDE         'ERMISS.FNC'
C------------------------------------------------------------------------
	iret   = 0
C
	autof = rivals ( irauto )
	CALL BR_VISB ( strvs1, strvs2, autof, visby, visbk, visfl,
     +		       idecd, twofld, dirvis, iret )
	IF ( .not. ERMISS ( visby ) )
     +		   rivals ( irvsby ( ivis ) ) = visby
	IF ( .not. ERMISS ( visbk ) ) 
     +		   rivals ( irvsbk ( ivis ) ) = visbk
	IF ( .not. ERMISS ( visfl ) ) 
     +		   rivals ( irvsfl ( ivis ) ) = visfl
	IF ( ( .not. ERMISS (visby) ) .or. ( .not. ERMISS (visbk) ) )
     +		   rivals ( irnvsb ) = ivis
C*
	RETURN
	END
