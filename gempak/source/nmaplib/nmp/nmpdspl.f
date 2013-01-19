	SUBROUTINE NMP_DSPL( panel, itype, file, attribs, iret )
C************************************************************************
C* NMP_DSPL                                                             *
C*                                                                      *
C* This subroutine draws a map, distance scale legend, lat/lon lines, 	*
C* and the stations.							*
C*									*
C* NMP_DSPL ( PANEL, ITYPE, FILE, ATTRIBS, IRET )			* 
C*                                                                      *
C* Input parameters:                                                    *
C*      PANEL           CHAR*           Panel location                  *
C*      ITYPE            INTEGER        Overlay type                    *
C*					0 = latlon			*
C*					1 = map				*
C*					2-4 = station			*
C*					  2 = marker only		*
C*					  3 = text only			*
C*					  4 = marker and text		*
C*					5 = Distance scale legend	*
C*      FILE            CHAR*           Overlay file                    *
C*      ATTRIBS         CHAR*           Attributes                  	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*					 0 = normal			*
C*					-2 = undrawn			*
C**                                                                     *
C* Log:                                                                 *
C* M. Li/GSC		09/00	initial coding				*
C* M. Li/GSC		03/01	cleanup					*
C* T. Piper/SAIC	08/04	Added GG_SCAL				*
C************************************************************************
        CHARACTER*(*)   file, panel, attribs
	CHARACTER	tstr*160
C------------------------------------------------------------------------
        iret = 0
	
	CALL GG_PANL ( panel, ier )

C
C*      Draw map, lat/lon lines, and station ID/marker.
C
	IF ( itype .eq. 0 ) THEN
C
C*	    Draw lat/lon lines. If the color is 0, lat/lon is not drawn.
C
	    CALL GG_LTLN ( attribs, ier )

	ELSE IF ( itype .eq. 1 ) THEN
C
C*	    Draw map. If the color is 0, the map is not drawn.
C
            CALL GG_MAP  ( attribs, ier )
	
	ELSE IF ( itype .ge. 2 .and. itype .le. 4) THEN
C
C*	    Draw station ID/marker
C*	       itype = 2 -- plot marker only, tcolor = 0
C*	       itype = 3 -- plot text only, mcolor = 0
C*	       itype = 4 -- plot marker and text, tcolor > 0, mcolor > 0
C
	    tstr = attribs // '|' // file
	    CALL GG_SPLT ( tstr, ier )

	ELSE IF ( itype .eq. 5 ) THEN
C
C*	    Draw distance scale legend
C
	    CALL GG_SCAL ( attribs, ier )
	ELSE
	    iret = -2
	END IF

	IF ( ier .ne. 0 ) iret = -2

	RETURN
	END

