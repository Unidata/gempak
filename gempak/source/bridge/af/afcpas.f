	SUBROUTINE AF_CPAS  ( isnbd, ibdg, iret )
C************************************************************************
C* AF_CPAS                                                              *
C*                                                                      *
C* This subroutine decodes a location direction from a given compass    *
C* point from within the location data of a PIREP report.               *
C*                                                                      *
C* AF_CPAS ( ISNBD, IBDG, IRET )                                        *
C*                                                                      *
C* Input parameters:                                                    *
C*      ISNBD           INTEGER         Index of "like-type" group which*
C*                                      contains compass point          *
C*                                                                      *
C* Output parameters:                                                   *
C*      IBDG		INTEGER		Location direction in degrees   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -1 = error during decoding     *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* A. Hardy/GSC         04/98                                           *
C* D. Kidwell/NCEP      10/00	Cleaned up, added error return          *
C************************************************************************
 	INCLUDE         'GEMPRM.PRM'
	INCLUDE         'afcmn.cmn'
C-----------------------------------------------------------------------
	iret = 0
        ii   = isnbd
	ibdg = IMISSD
C
C*	Searching for northerly winds.
C
	IF ( fields ( ii ) (1:1) .eq. 'N' ) THEN 
	    ibdg = 0
C
C*	    Searching for NE or NW winds.
C
	    IF ( fields ( ii ) (2:2) .eq. 'E' ) THEN
	        ibdg = 45
	      ELSE IF ( fields ( ii ) (2:2) .eq. 'W' ) THEN
	        ibdg = 315
C
C*	      Searching for NNE or NNW winds.
C
	      ELSE IF ( fields ( ii ) (3:3) .eq. 'W' ) THEN
		ibdg = 337
              ELSE IF ( fields ( ii ) (3:3) .eq. 'E' ) THEN
		ibdg = 22
	    END IF
C
C*	  Searching for easterly winds.
C
	  ELSE IF ( fields ( ii ) (1:1) .eq. 'E' ) THEN
	    ibdg = 90
C
C*	    Searching for ENE or ESE winds.
C
	    IF ( fields ( ii ) (2:2) .eq. 'N' ) THEN
	        ibdg = 67
              ELSE IF ( fields ( ii ) (2:2) .eq. 'S' ) THEN
		ibdg = 112
	    END IF
C
C*	  Searching for southerly winds.
C
	  ELSE IF ( fields ( ii ) (1:1) .eq. 'S' ) THEN
            ibdg = 180
C
C*	    Searching for SE or SW winds.
C
	    IF ( fields ( ii ) (2:2) .eq. 'E' ) THEN
		ibdg = 135
	      ELSE IF ( fields ( ii ) (2:2) .eq. 'W' ) THEN
		ibdg = 225
C
C*	      Searching for SSE or SSW winds.
C
	      ELSE IF ( fields ( ii ) (3:3) .eq. 'E' ) THEN
		ibdg = 157
	      ELSE IF ( fields ( ii ) (3:3) .eq. 'W' ) THEN
		ibdg = 202
	    END IF
C
C* 	  Searching for westerly winds.
C
	  ELSE IF ( fields ( ii ) (1:1) .eq. 'W' )  THEN
	      ibdg = 270
C
C*	    Searching for WNW or WSW winds.
C
	    IF ( fields ( ii ) (2:2) .eq. 'S' ) THEN
		ibdg = 247
	      ELSE IF ( fields ( ii ) (2:2) .eq. 'N' ) THEN
		ibdg = 292
	    END IF
	END IF
C
	IF ( ibdg .eq. IMISSD ) iret = -1
C*
	RETURN
	END
