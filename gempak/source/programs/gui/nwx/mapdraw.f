	SUBROUTINE MAP_DRAW ( map, garea, proj, latlon, panel, text,
     +			      title, ititl, linttl, shrttl,
     +			      clear, iret )
C************************************************************************
C* MAP_DRAW								*
C*									*
C* This subroutine draws a map, lat/lon lines, and a title.		*
C*									*
C* MAP_DRAW ( MAP, GAREA, PROJ, LATLON, PANEL, TEXT, TITLE, 		*
C* 	      ITITL, LINTTL, SHRTTL, CLEAR, IRET )			*
C*									*
C* Input parameters:							*
C*	MAP		CHAR*	 	Map Color			*
C*	GAREA   	CHAR*	 	Graphics area			*
C*	PROJ		CHAR*	 	Map projection	name		*
C*	LATLON		CHAR*	 	Line color			*
C*	PANEL		CHAR*	 	Panel location			*
C*	TEXT		CHAR*	 	Text input			*
C*	TITLE		CHAR*		Title string			*
C*	ITITL		INTEGER		Title color			*
C*	LINTTL		INTEGER		Title line			*
C*	SHRTTL		CHAR*		Short title string		*
C*	CLEAR		LOGICAL	 	Clear screen flag		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER	 	Return code			*
C**									*
C* Log:									*
C* L. Williams/EAI	 4/94	Converted to a subroutine from GPMAP	*
C* S. Jacobs/NMC	 7/94	Copied for use with the NWX program	*
C* E. Safford/SAIC	12/07	wrap the gemplot calls			*
C* S. Guan/NCEP         07/18   Change hipowo.cia to hipowo.nws         *
C************************************************************************
	INCLUDE 'GEMPRM.PRM'
	CHARACTER*(*)	map, garea, proj, latlon, panel, text,
     +			title, shrttl
	LOGICAL		clear
C*
	CHARACTER	satfil*132/CHNULL/
	LOGICAL		drpflg
C------------------------------------------------------------------------
	iret = 0
C
C*	Set the map file.
C
C--	CALL WGEM_GSMFIL ( 'mepowo.gsf', ier )
	CALL WGEM_GSMFIL ( 'hipowo.nws', ier )
	IF  ( ier .ne. 0 )  CALL WGEM_ER_WMSG ( 'GEMPLT', ier, ' ', ierr )
C
C*      Set the projection.
C
	CALL WGEM_GG_MAPS ( proj, garea, satfil, drpflg, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Set the text attributes.
C
	CALL WGEM_IN_TEXT ( text, ier )
C
C*	Clear the screen and write the short title.
C
	IF  ( clear )  CALL WGEM_GCLEAR ( ier )
	IF  ( clear )  CALL WGEM_GMESG ( shrttl, ier )
	CALL WGEM_GG_PANL ( panel, ier )
C
C*	Display satellite image, if desired.
C
	IF  ( drpflg )  CALL WGEM_GSATIM ( satfil, ier )
C
C*	Draw map and lat/lon lines.
C
	CALL WGEM_GG_MAP  ( map, ier )
	CALL WGEM_GG_LTLN ( latlon, ier )
C
C*	Draw the title.
C
	CALL WGEM_GSCOLR  ( ititl, ier )
	CALL WGEM_GG_WSTR ( title, linttl, ier )
C
C*      Flush the buffers.
C
	CALL WGEM_GEPLOT ( ier )
C*
	RETURN
	END
