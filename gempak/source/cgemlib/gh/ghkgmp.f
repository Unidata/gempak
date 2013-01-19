	SUBROUTINE GH_KGMP ( rmnlat, rmnlon, rmxlat, rmxlon,
     +			     fmnlat, fmnlon, fmxlat, fmxlon, dellon,
     +			     iret ) 
C************************************************************************
C* GH_KGMP								*
C*									*
C* This subroutine sets up the map and gets the map bounds in           *
C* normalized coordinates.                                              *
C*									*
C* GH_KGMP ( RMNLAT, RMNLON, RMXLAT, RMXLON, FMNLAT, FMNLON, FMXLAT,    *
C*	     FMXLON, DELLON, IRET )                                     *
C*									*
C* Input parameters:							*
C*	RMNLAT		REAL		Bottom map latitude, degrees    *
C*	RMNLON		REAL		Left map longitude, degrees     *
C*	RMXLAT		REAL		Top map latitude, degrees       *
C*	RMXLON		REAL		Right map longitude, degrees    *
C*									*
C* Output parameters:							*
C*	FMNLAT		REAL		Bottom edge of map, norm. coord.*
C*	FMNLON		REAL		Left edge of map, norm. coord.  *
C*	FMXLAT		REAL		Top edge of map, norm. coord.   *
C*	FMXLON		REAL		Right edge of map, norm. coord  *
C*	DELLON		REAL		Longitudinal extent of map, deg.*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* D. Kidwell/NCEP       4/02           Extracted from GH_KGPH          *
C* X. Guo		03/10           Changed normalized coordinates  *
C*                                      to view coordinates             *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C-----------------------------------------------------------------------
	iret = 0
C
	dmnlat = rmnlat
	dmnlon = rmnlon
	dmxlat = rmxlat
	dmxlon = rmxlon
C
C*      Set up map. 
C
        CALL GSMMGN ( 0., 0., 0., 0., ier )
        CALL GSMMAP ('MER', dmnlat, dmnlon, dmxlat, dmxlon, ier )
C
C*      Fill out the margin areas with the rest of the map.
C
        CALL GQBND ( 'V', xmnlat, xmnlon, xmxlat, xmxlon, ier )
        CALL GTRANS ( 'V', 'M', 1, xmnlat, xmnlon, dmnlat,
     +                    dmnlon, ier )
        CALL GTRANS ( 'V', 'M', 1, xmxlat, xmxlon, dmxlat,
     +                    dmxlon, ier )
        CALL GSMMAP ('MER', dmnlat, dmnlon, dmxlat, dmxlon, ier )
C
C*	Get the longitudinal extent of the map, to be used by GH_KGFL.
C
	dellon = dmxlon - dmnlon
	IF ( dellon .lt. 0. ) dellon = dellon + 360.
C
        CALL GQBND ( 'V', fmnlat, fmnlon, fmxlat, fmxlon, ier )
C*
	RETURN
	END
