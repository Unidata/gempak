	SUBROUTINE GG_MAP2  ( mapfil, maptyp, mapwid, mapcol, iret )
C************************************************************************
C* GG_MAP2 : See GG_MAP ... added specification of mapfile
C      mapfil = char*(*) mapfile name
C      maptyp,mapwid,mapcol = maptyp,mapwid,mapcol

	CHARACTER*(*)	mapfil

	iret = -7
	CALL GSMFIL( mapfil, ier )
	if ( ier .ne. 0 ) return

        IF  ( mapcol .gt. 0 )  THEN
		CALL GQLINE  ( ilto, ilhwo, iwdo, iwdhwo, ier )
		CALL GSLINE  ( maptyp, 0, mapwid, 0, ier )
		CALL GSCOLR  ( mapcol, ier )
		CALL GDRMAP  ( ier )
		IF  ( ier .ne. 0 )  return
		CALL GSLINE  ( ilto, 0, iwdo, 0, ier )
        END IF
	iret = 0

	RETURN
	END
