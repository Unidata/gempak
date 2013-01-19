      SUBROUTINE GDPPRT ( iframe, ibang, iret ) 
C************************************************************************
C* GDPPRT								*
C*									*
C* This subroutine prints the current parameter values according to	*
C* the given frame number (iframe) and overlay number (ibang).		*
C* program.								*
C*									*
C* GDPPRT ( IFRAME, IBANG, IRET )					*
C*									*
C* Input parameters:							*
C*									*
C* IFRAME	INTEGER		Frame number				*
C* IBANG	INTEGER		Layer					*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* D.W.Plummer		11/96						*
C* D.W.Plummer		 2/97	Changed name from GDPDSP to GDPPRT	*
C* T. Lee/GSC		 7/99	Uppercased for text output		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'gdplot.cmn'
C
 	CHARACTER*256	gdfile
 	CHARACTER*128	gdatim, garea, proj, map, latlon,
     +			type, gdpfun, glevel, gvcord, title, panel
C-----------------------------------------------------------------------
	iret = 0
	ier  = 1
	gdfile = gdf(ibang)
	gdatim = gda(ibang)
	garea  = gar(ibang)
	proj   = pro(ibang)
	map    = mpp(ibang)
	latlon = lat(ibang)
	type   = typ(ibang)
	gdpfun = gdp(ibang)
	glevel = gle(ibang)
	gvcord = gvc(ibang)
	title  = tit(ibang)
	panel  = pan(ibang)
C
	IF ( ibang .eq. 1 )  THEN
C
	    WRITE ( 6, 1001 )  iframe
1001	FORMAT ( / ' PARAMETERS FOR GDPLOT : FRAME NUMBER ', I2 , 
     +             ' INITIAL PLOT SETTINGS -' )
	    CALL ST_LSTR ( gdfile, lgdf, iret )
	    CALL ST_LSTR ( gdatim, lgda, iret )
	    WRITE ( 6, 1010 )  gdfile(1:lgdf), gdatim(1:lgda)
1010	FORMAT ( '    GDFILE=', A , ', GDATTIM=', A )
C
	    CALL ST_LSTR (  garea, lgar, iret )
	    CALL ST_LSTR (   proj, lpro, iret )
	    CALL ST_LSTR (    map, lmap, iret )
	    CALL ST_LSTR ( latlon, llat, iret )
	    WRITE ( 6, 1011 )  garea(1:lgar), proj(1:lpro), map(1:lmap),
     +			       latlon(1:llat)
1011	FORMAT ( '    GAREA=', A , ', PROJ=', A , ', MAP=', A ,
     +           ', LATLON=', A )
C
	    CALL ST_LSTR ( title, ltit, iret )
	    CALL ST_LSTR ( panel, lpan, iret )
	    IF ( ltit .ne. 0 .and. title(1:1) .ne. '0' )  
     +	        WRITE ( 6, 1012 )  panel(1:lpan), title(1:ltit)
1012	FORMAT ( '    PANEL=', A, ', TITLE=', A )
C
	    CALL ST_LSTR ( sat(ibang), lsat, iret )
	    IF ( lsat .ne. 0 )
     +		WRITE ( 6, 1013 )  sat(ibang)(:lsat)
1013	FORMAT ( '    SATFIL=', A )
C
	    WRITE ( 6, 2010 ) 'OVERLAY', 'TYPE',' LEVEL',
     +			      'VCORD','GRID FUNCTION'
2010	FORMAT ( 4X, A, 2X, A, 6X, A, 11X, A, 7X, A )
C
	END IF
C
C*	Translate vertical coordinate.
C
C	CALL LV_CCRD ( ivcord, vcord, ier )
	CALL ST_LCUC (   type, type, iret )
	CALL ST_LCUC ( gdpfun, gdpfun, iret )
	CALL ST_LSTR ( gdpfun, lgdp, iret )
	CALL ST_LCUC ( gvcord, gvcord, iret )
	WRITE ( 6, 2020 )  ibang, type(1:7), glevel(1:12), 
     7			   gvcord(1:8), gdpfun(1:lgdp)
2020	FORMAT ( 4X, I5, 4X, A, 4X, A, 4X, A, 4X, A )
C
C*	Write out any changes to parameters
C
	IF ( ibang .gt. 1 )  THEN
C
	    CALL ST_LSTR ( gdf(ibang), lgdf, iret )
	    IF ( gdf(ibang) .ne. gdf(ibang-1) )  
     +		WRITE ( 6, 2050 )  ibang, gdf(ibang)(:lgdf)
2050	FORMAT ( 10X, 'CHANGE for OVERLAY ',I2,
     7           ' : GDFILE = ', A )
C
	    CALL ST_LSTR ( pan(ibang), lpan, iret )
	    IF ( pan(ibang) .ne. pan(ibang-1) )  
     +		WRITE ( 6, 2060 )  ibang, pan(ibang)(:lpan)
2060	FORMAT ( 10X, 'CHANGE for OVERLAY ',I2,
     7           ' : PANEL  = ', A )
C
	    CALL ST_LSTR ( gda(ibang), lgda, iret )
	    IF ( gda(ibang) .ne. gda(ibang-1) )  
     +		WRITE ( 6, 2040 )  ibang, gda(ibang)(:lgda)
2040	FORMAT ( 10X, 'CHANGE for OVERLAY ',I2,
     +           ' : GDATIM = ', A )
C
	    CALL ST_LSTR ( sat(ibang), lsat, iret )
	    IF ( sat(ibang) .ne. sat(ibang-1) )  
     +		WRITE ( 6, 2030 )  ibang, sat(ibang)(:lsat)
2030	FORMAT ( 10X, 'CHANGE for OVERLAY ',I2,
     +           ' : SATFIL = ', A )
C
	END IF
C*
	RETURN
	END
