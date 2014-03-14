	SUBROUTINE IM_NEX2  ( imgfil, iret )
C************************************************************************
C* IM_NEX2								*
C*									*
C*									*
C* IM_NEX2  ( IMGFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C**									*
C* Log:									*
C* S. Chiswell	Unidata	 4/04	Modified to use GSATMG			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
	INCLUDE		'AREAFL.CMN'
	INCLUDE		'imcmn.cmn'
C*
	CHARACTER*(*)	imgfil
C*
	CHARACTER	filnam*160, proj*4
	CHARACTER	radar_field*8, sweep_num*8
	CHARACTER	lev2tmp*20, tmpl*80, radid*4
C------------------------------------------------------------------------
	iret = 0
C
C*	Add a NULL to the end of the file name.
C
	CALL ST_NULL ( imgfil, filnam, lenf, ier )
C
	IF ( ICHAR(im_sweep_num(1:1)) .eq. 0 ) THEN 
		sweep_num = '0'
	ELSE
		sweep_num = im_sweep_num
	END IF
C
	IF ( ICHAR(im_radar_field(1:1)) .eq. 0 ) THEN
                radar_field = 'dz'
        ELSE
                radar_field = im_radar_field
        END IF
C
	CALL ST_NULL ( sweep_num, sweep_num, lenf, ief )
	CALL ST_NULL ( radar_field, radar_field, lenf, ief )
C
C*	see if Radar ID can be extracted from file name (as a failsafe)
C
	radid = '    '
	lev2tmp = 'NEXRII'
	CALL ST_NULL (lev2tmp, lev2tmp, lenf, ief)
	CALL ctb_dttmpl ( lev2tmp, tmpl, ief )
	IF ( ief .eq. 0 ) THEN
	    CALL ST_RNUL ( tmpl, tmpl, ilen1, ief)
	    CALL ST_RPST ( tmpl, '%SITE%', '****', ipos, tmpl, ief)
	    IF ( ( ief .eq. 0 ) .and. ( ipos .gt. 0 ) ) THEN
		CALL ST_LSTR ( tmpl, ilen1, ief )
		CALL ST_LSTR ( imgfil, ilen2, ief )
		i = ilen2
		DO WHILE ( ( i .gt. 0 ) .and. (imgfil (i:i) .ne. '/' ) )
		   i = i - 1
		END DO
		IF ( ilen1 .eq. (ilen2 - i) ) THEN
		    ipos = ipos + i
		    radid = imgfil(ipos:ipos+3)
		END IF
	    END IF
	END IF
C
C*	Read the Level II Archive file header and get necessary information.
C
	CALL IM_RNEX2 ( filnam, radid, sweep_num, radar_field, imnpix, 
     +			imnlin, imsorc, imtype, imdate, imtime, range, 
     +			belv, iswpnum, clat, clon, 
     +			xll, xur, yll, yur, vel_res, ier )
	IF  ( ier .ne. 0 )  THEN
	    iret = -2
	    RETURN
	END IF
C
C*	Set the remaining image definition common variables.
C
	imldat = imnpix * imnlin
C
C*	kilometers per pixel of image
C
	rmxres = ( 2. * range ) / float(imnpix - 1)
	rmyres = (2. * range) / float(imnlin - 1)
	rmxysc = rmxres / rmyres
	imdpth = 1
	imradf = 1
	imleft = 1 
	imtop  = 1 
	imrght = imnpix
	imbot  = imnlin
	rmbelv = belv

	cmstyp = radar_field
	imnchl = iswpnum
C	CALL ST_NUMB ( sweep_num, imnchl, ier )
C
C*	Set the map projection.
C
	iadir (6)  = 1
	iadir (7)  = 1
	iadir (12) = 1
	iadir (13) = 1
	CALL ST_CTOI ( 'RADR', 1, ianav (1), ier )
        ianav (2)  = imnlin / 2
        ianav (3)  = imnpix / 2
C
C*	Set the station lat/lon in navigation block
C
c	ianav (4) = clat_dms
C
C*      Set west lon (neg) to match MCIDAS west lon (pos)
C
c	ianav (5) = -1. * clon_dms
C
	deg = ABS ( clat )
        ideg = INT ( deg )
	fms = deg - float ( ideg )
	imin = INT ( fms * 60 )
	fsec = fms * 60. - float ( imin )
	isec = INT ( fsec * 60. + . 5 )
	IF ( isec .gt. 59 ) THEN
	   isec = 0
	   imin = imin + 1
	END IF
	IF ( imin .gt. 59 ) THEN
	   imin = 0
	   ideg = ideg + 1
	END IF
	ianav (4) = ( ( ideg * 100 ) + imin ) * 100 + isec
	IF ( clat .lt. 0 ) ianav (4) = -1 * ianav (4)

	deg = ABS ( clon )
        ideg = INT ( deg )
	fms = deg - float ( ideg )
	imin = INT ( fms * 60. )
	fsec = fms * 60 - float ( imin )
	isec = INT ( fsec * 60. + . 5 )
	IF ( isec .gt. 59 ) THEN
	   isec = 0
	   imin = imin + 1
	END IF
	IF ( imin .gt. 59 ) THEN
	   imin = 0
	   ideg = ideg + 1
	END IF
	ianav (5) = ( ( ideg * 100 ) + imin ) * 100 + isec
C
C*          Set west lon (neg) to match MCIDAS west lon (pos)
C
	IF ( clon .gt. 0 ) ianav (5) = -1 * ianav (5)
C
C	resolution
	ianav (6) = 1000 * rmxres
	ianav (7) = 0
	ianav (8) = 0
	
        CALL GSATMG ( imgfil, iadir, ianav, imleft, imtop, imrght,
     +                    imbot, ier )

	CALL IM_CALBAR_INIT ( ier )
C
        cmcalb = 'PROD'
	imcalbar = 1
C
C*	Read in min and max pixel and corresponding data
C*	values. Scale value is for data points.
C
	if ( imtype .eq. 225 ) then
           cmbunt = 'dBZ'
	   CALL IM_CALBAR ( 0., 107., -32., 75., ier )
	   cmblev (1) = 'ND'
	   cmblev (108) = '>=75'
	   imndlv = 108
	else if ( imtype .eq. 226 ) then
           cmbunt = 'm s**-1'
	   if (vel_res .eq. .5 ) then
	      CALL IM_CALBAR ( 0., 255., -63.5, 63.5, ier )
	   else
	      iminval = -127.
	      imaxval = 127
	      CALL IM_CALBAR ( 0., 255., -127., 127., ier )
	   end if
	   cmblev (1) = 'ND'
	   cmblev (256) = 'RF'
	   imndlv = 256
	else
           cmbunt = 'm s**-1'
	   CALL IM_CALBAR ( 0., 255., -64., 64., ier )
	   cmblev (1) = 'ND'
	   cmblev (256) = 'RF'
	   imndlv = 256
	endif

	RETURN
	END
