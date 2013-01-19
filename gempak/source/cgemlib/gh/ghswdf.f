	SUBROUTINE GH_SWDF ( coltag, icol, inum, dlat, dlon, iret)
C************************************************************************
C* GH_SWDF								*
C*									*
C* This subroutine draws the lines and fills for the each polygon and   *
C* for the tangent lines to the polygon. 				*
C*									*
C* GH_SWDF ( COLTAG, ICOL, INUM, DLAT, DLON, IRET)			*
C*									*
C* Input parameters:							*
C*	COLTAG		CHAR*		Color tag of line and fill	*
C*	ICOL		INTEGER		Color of line and fill		*
C*	INUM		INTEGER		Total number points		*
C*      DLAT (INUM)	REAL		Latitudes for line		*
C*      DLON (INUM)	REAL		Longitudes for line		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 3/01   					*
C* A. Hardy/GSC		 6/01	Added color tag				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
        CHARACTER*(*)	coltag
        REAL		dlat(*), dlon(*)
C*
C-----------------------------------------------------------------------
	iret = 0
C
C*      Set the color for the polygon.
C
        CALL ST_LSTR ( coltag, lens, ier )
	CALL GH_COLR ( coltag(:lens), icol, ier)
C
C*      Draw the outline of the polygon. 
C
    	CALL GSLINE ( 1, 0, 4, 0, ier)
        CALL GLINE ( 'M', inum, dlat, dlon, ier )
C
C*      Fill the polygon. 
C
        CALL GSFILL( 1.5, 1, ier )
        CALL GFILL ( 'M', inum, dlat, dlon, ier )
C*
	RETURN
	END
