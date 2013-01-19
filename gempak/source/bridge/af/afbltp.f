	SUBROUTINE AF_BLTP  ( buhd, cborg, iret )
C************************************************************************
C* AF_BLTP								*
C*									*
C* This subroutine determines the type of a bulletin (i.e. "AMDAR",	*
C* "PIREP", "AIREP", or "RECCO") based upon BUHD and CBORG.  If the	*
C* type is unknown or cannot be determined, then it is set to "LEMON".	*
C* The output is stored in COMMON / BULTYP /.	  			*
C*									*
C* AF_BLTP  ( BUHD, CBORG, IRET )					*
C*									*
C* Input parameters:							*
C*	BUHD		CHAR*		Bulletin header 		*
C*	CBORG		CHAR*		Bulletin originator 		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*									*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		10/96	Set UBUS(2|3|91)KWBC to LEMON		*
C* J. Ator/NP12		11/96	Set URNT(12|14|40) to LEMON	 	*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NP12		10/97	Set UR..(10|11|13) to RECCO		*
C* A. Hardy/GSC         04/98   Added new AIREP WMO headings            *
C* A. Hardy/GSC         04/98   Moved XX1 else if statement             *
C* J. Ator/NCEP		07/00	Added UAHK31 and UACI31 to AIREP	*
C* J. Ator/NCEP		08/00	Cleaned up				*
C* J. Ator/NCEP		02/02	Set YRXX85 to AIREP, set UAAK04 to PIREP*
C* J. Ator/NCEP		10/04	Add XIXX84 to PIREP			*
C* J. Ator/NCEP		01/07	Add UAHW01 to PIREP, UAJP71 to LEMON	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	buhd, cborg
C-----------------------------------------------------------------------
	iret = 0
	bultyp = LEMON
C
	IF  ( buhd (1:2) .eq. 'UD' )  THEN
	    bultyp = AMDAR
	  ELSE IF  ( buhd (1:2) .eq. 'UB' )  THEN
	    IF  (  ( ( buhd (3:6) .eq. 'US2 ' ) .or.
     +		     ( buhd (3:6) .eq. 'US3 ' ) .or.
     +		     ( buhd (3:6) .eq. 'US91' ) )  .and.
     +		    ( cborg (1:4) .eq. 'KWBC' )  )  THEN
	      ELSE
		bultyp = PIREP
	    END IF
	  ELSE IF  ( buhd (1:2) .eq. 'UR' )  THEN
	    IF  ( ( buhd (5:6) .eq. '10' ) .or.
     +		  ( buhd (5:6) .eq. '11' ) .or.
     +		  ( buhd (5:6) .eq. '13' ) )  THEN
		bultyp = RECCO 
	    END IF
	  ELSE IF  ( buhd (1:6) .eq. 'XRXX84' )  THEN
	      bultyp = AIREP
	  ELSE IF  ( buhd (1:6) .eq. 'YRXX85' )  THEN
	      bultyp = AIREP
	  ELSE IF  ( buhd (1:6) .eq. 'XIXX84' )  THEN
	      bultyp = PIREP
	  ELSE IF  ( buhd (1:6) .eq. 'YIXX84' )  THEN
	      bultyp = PIREP
	  ELSE IF  ( buhd (1:2) .eq. 'UA' )  THEN
	    IF  ( ( buhd (3:6) .eq. 'AK04' ) .or.
     +		  ( buhd (3:6) .eq. 'HW1 ' ) .or.
     +		  ( buhd (3:6) .eq. 'HW01' ) .or.
     +		  ( buhd (3:6) .eq. 'CA1 ' ) .or.
     +		  ( buhd (3:6) .eq. 'CN01' ) .or.
     +		  ( buhd (3:6) .eq. 'CN10' ) .or.
     +		  ( buhd (3:6) .eq. 'PQ21' ) .or.
     +		  ( buhd (3:4) .eq. 'US'   ) .or.
     +		  ( ( buhd (3:4) .eq. 'XX' ) .and.
     +		    ( cborg (1:4) .eq. 'KAWN' ) )  )  THEN
		bultyp = PIREP
	      ELSE IF  ( buhd (3:6) .ne. 'JP71' )  THEN
		bultyp = AIREP
	    END IF
	END IF
C*
	RETURN
	END
