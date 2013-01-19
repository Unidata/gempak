	SUBROUTINE GDPSTP ( pname, s, iret )
C************************************************************************
C* GDPSTP								*
C*									*
C* This subroutine takes a GDPLOT parameter (pname) and splits and	*
C* stores the bang values into gdplot.cmn for later reference.		*
C*									*
C* GDPSTP  ( PNAME, S, IRET )						*
C*									*
C* Input parameters:							*
C*	PNAME		CHAR*		GDPLOT parameter name		*
C*	S		CHAR*		GDPLOT parameter string		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -1 = pname not found/processed *
C**									*
C* Log:									*
C* D.W.Plummer/NCEP	10/96	New for gdplot.				*
C* D.W.Plummer/NCEP	 4/97	Added FILTER parameter.			*
C* K. Brill/HPC		12/02	Added IJSKIP parameter.			*
C* M. Li/SAIC		11/03	Added IMCBAR				*
C* T. Piper/SAIC	08/04	Added MSCALE				*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'gdplot.cmn'
C*
	CHARACTER	pname*(*), s*(*)
C
	iret = 0
C
	IF ( pname .eq. 'PROJ' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., pro, npro, iret )
C
	ELSE IF ( pname .eq. 'GAREA' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., gar, ngar, iret )
C
	ELSE IF ( pname .eq. 'IJSKIP' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., skp, nskp, iret )
C
	ELSE IF ( pname .eq. 'MAP' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., mpp, nmpp, iret )
C
	ELSE IF ( pname .eq. 'MSCALE' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., mscl, nmscl, iret )
C
	ELSE IF ( pname .eq. 'LATLON' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., lat, nlat, iret )
C
	ELSE IF ( pname .eq. 'GDFILE' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., gdf, ngdf, iret )
C
	ELSE IF ( pname .eq. 'GLEVEL' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., gle, ngle, iret )
C
	ELSE IF ( pname .eq. 'GVCORD' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., gvc, ngvc, iret )
C
	ELSE IF ( pname .eq. 'GDPFUN' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., gdp, ngdp, iret )
C
	ELSE IF ( pname .eq. 'CINT' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., cin, ncin, iret )
C
	ELSE IF ( pname .eq. 'LINE' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., lin, nlin, iret )
C
	ELSE IF ( pname .eq. 'TITLE' )  THEN
	    CALL GDINST ( s, '<', MAXB, .false., tit, ntit, iret )
C
	ELSE IF ( pname .eq. 'PANEL' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., pan, npan, iret )
C
	ELSE IF ( pname .eq. 'SCALE' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., sca, nsca, iret )
C
	ELSE IF ( pname .eq. 'FINT' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., fin, nfin, iret )
C
	ELSE IF ( pname .eq. 'FLINE' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., fli, nfli, iret )
C
	ELSE IF ( pname .eq. 'TYPE' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., typ, ntyp, iret )
C
	ELSE IF ( pname .eq. 'GDATTIM' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., gda, ngda, iret )
C
	ELSE IF ( pname .eq. 'SATFIL' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., sat, nsat, iret )
C
	ELSE IF ( pname .eq. 'RADFIL' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., rad, nrad, iret )
C
	ELSE IF ( pname .eq. 'TEXT' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., tex, ntex, iret )
C
	ELSE IF ( pname .eq. 'CONTUR' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., con, ncon, iret )
C
	ELSE IF ( pname .eq. 'WIND' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., win, nwin, iret )
C
	ELSE IF ( pname .eq. 'REFVEC' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., ref, nref, iret )
C
	ELSE IF ( pname .eq. 'SKIP' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., ski, nski, iret )
C
	ELSE IF ( pname .eq. 'HILO' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., hil, nhil, iret )
C
	ELSE IF ( pname .eq. 'HLSYM' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., hls, nhls, iret )
C
	ELSE IF ( pname .eq. 'CLRBAR' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., clr, nclr, iret )
C
	ELSE IF ( pname .eq. 'STNPLT' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., stn, nstn, iret )
C
	ELSE IF ( pname .eq. 'STREAM' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., str, nstr, iret )
C
	ELSE IF ( pname .eq. 'POSN' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., pos, npos, iret )
C
	ELSE IF ( pname .eq. 'COLORS' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., col, ncol, iret )
C
	ELSE IF ( pname .eq. 'MARKER' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., mar, nmar, iret )
C
	ELSE IF ( pname .eq. 'GRDLBL' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., grd, ngrd, iret )
C
	ELSE IF ( pname .eq. 'LUTFIL' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., lut, nlut, iret )
C
	ELSE IF ( pname .eq. 'FILTER' )  THEN
	    CALL GDINST ( s, '<', MAXB,  .true., fil, nfil, iret )
C
        ELSE IF ( pname .eq. 'IMCBAR' )  THEN
            CALL GDINST ( s, '<', MAXB,  .true., imc, nimc, iret )
C
	ELSE 
C
	    iret = -1
C
	END IF
C
	RETURN
C
	END
