	SUBROUTINE GDPSTG ( pname, index, string, len, iret )
C************************************************************************
C* GDPSTG								*
C*									*
C* This subroutine takes a GDPLOT parameter (pname) and splits and	*
C* stores the bang values into gdplot.cmn for later reference.		*
C*									*
C* GDPSTG  ( PNAME, INDEX, STRING, LEN, IRET )				*
C*									*
C* Input parameters:							*
C*	PNAME		CHAR*		GDPLOT parameter name		*
C*	INDEX		INTEGER		Index of string to get		*
C*									*
C* Output parameters:							*
C*	STRING		CHAR*		GDPLOT parameter string		*
C*	LEN		INTEGER		GDPLOT parameter string length	*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = pname not found		*
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	10/96	New for gdplot.				*
C* D.W.Plummer/NCEP	 3/97	Added LUTFIL				*
C* D.W.Plummer/NCEP	 4/97	Added FILTER				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'gdplot.cmn'
C*
	CHARACTER	pname*(*), string*(*)
C
	iret = 0
C
	IF ( pname .eq. 'PROJ' )  THEN
	    string = pro ( index )
C
	ELSE IF ( pname .eq. 'GAREA' )  THEN
	    string = gar ( index )
C
	ELSE IF ( pname .eq. 'MAP' )  THEN
	    string = mpp ( index )
C
	ELSE IF ( pname .eq. 'LATLON' )  THEN
	    string = lat ( index )
C
	ELSE IF ( pname .eq. 'GDFILE' )  THEN
	    string = gdf ( index )
C
	ELSE IF ( pname .eq. 'GLEVEL' )  THEN
	    string = gle ( index )
C
	ELSE IF ( pname .eq. 'GVCORD' )  THEN
	    string = gvc ( index )
C
	ELSE IF ( pname .eq. 'GDPFUN' )  THEN
	    string = gdp ( index )
C
	ELSE IF ( pname .eq. 'CINT' )  THEN
	    string = cin ( index )
C
	ELSE IF ( pname .eq. 'LINE' )  THEN
	    string = lin ( index )
C
	ELSE IF ( pname .eq. 'TITLE' )  THEN
	    string = tit ( index )
C
	ELSE IF ( pname .eq. 'PANEL' )  THEN
	    string = pan ( index )
C
	ELSE IF ( pname .eq. 'SCALE' )  THEN
	    string = sca ( index )
C
	ELSE IF ( pname .eq. 'FINT' )  THEN
	    string = fin ( index )
C
	ELSE IF ( pname .eq. 'FLINE' )  THEN
	    string = fli ( index )
C
	ELSE IF ( pname .eq. 'TYPE' )  THEN
	    string = typ ( index )
C
	ELSE IF ( pname .eq. 'GDATTIM' )  THEN
	    string = gda ( index )
C
	ELSE IF ( pname .eq. 'SATFIL' )  THEN
	    string = sat ( index )
C
	ELSE IF ( pname .eq. 'RADFIL' )  THEN
	    string = rad ( index )
C
	ELSE IF ( pname .eq. 'TEXT' )  THEN
	    string = tex ( index )
C
	ELSE IF ( pname .eq. 'CONTUR' )  THEN
	    string = con ( index )
C
	ELSE IF ( pname .eq. 'WIND' )  THEN
	    string = win ( index )
C
	ELSE IF ( pname .eq. 'REFVEC' )  THEN
	    string = ref ( index )
C
	ELSE IF ( pname .eq. 'SKIP' )  THEN
	    string = ski ( index )
C
	ELSE IF ( pname .eq. 'HILO' )  THEN
	    string = hil ( index )
C
	ELSE IF ( pname .eq. 'HLSYM' )  THEN
	    string = hls ( index )
C
	ELSE IF ( pname .eq. 'CLRBAR' )  THEN
	    string = clr ( index )
C
	ELSE IF ( pname .eq. 'STNPLT' )  THEN
	    string = stn ( index )
C
	ELSE IF ( pname .eq. 'STREAM' )  THEN
	    string = str ( index )
C
	ELSE IF ( pname .eq. 'POSN' )  THEN
	    string = pos ( index )
C
	ELSE IF ( pname .eq. 'COLORS' )  THEN
	    string = col ( index )
C
	ELSE IF ( pname .eq. 'MARKER' )  THEN
	    string = mar ( index )
C
	ELSE IF ( pname .eq. 'GRDLBL' )  THEN
	    string = grd ( index )
C
	ELSE IF ( pname .eq. 'LUTFIL' )  THEN
	    string = lut ( index )
C
	ELSE IF ( pname .eq. 'FILTER' )  THEN
	    string = fil ( index )
C
	ELSE 
C
	    iret = -1
C
	END IF
C
	CALL ST_LSTR ( string, len, iret )
C
	RETURN
C
	END
