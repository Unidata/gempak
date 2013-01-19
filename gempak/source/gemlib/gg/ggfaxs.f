	SUBROUTINE GG_FAXS  ( imgfil, is, ix, iy, idrpfl, iret )
C************************************************************************
C* GG_FAXS								*
C*									*
C* This subroutine defines the margine and set common blocks		*
C*									*
C* GG_FAXS  ( IMGFIL, IS, IX, IY, IDRPFL, IRET )			*
C*									*
C* Input parameters:							*
C*      IMGFIL          CHAR*           6-bit FAX product file name     *
C*	IS		INTEGER		File size			*
C*	IX		INTEGER		Pixels per line			*
C*	IY		INTEGER		Image lines			*
C* Output parameters:							*
C*	IDRPFL		INTEGER		Flag to drop the image		*
C*					  0 = No input from user	*
C*					  1 = Drop image		*
C*					  2 = Do not drop image		*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return		*
C**									*
C* Log:									*
C* R.Tian/SAIC		04/02	Modified from GG_MAPS			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
C
	REAL		xn, yn
C------------------------------------------------------------------------
	iret = 0
C
C*	Set graph mode.
C
	CALL GSMODE  ( 2, ier )
C
C*	Set no margins.
C
	CALL GSMMGN  ( 0.0, 0.0, 0.0, 0.0, ier )
C
C*	Set drop image.
C
	idrpfl = 1
C
C*      Set the IMGDEF common block.
C
        imftyp = IFNFAX
        imbank = 3
        imdoff = 0
	imldat = is
	imnpix = ix
	imnlin = iy
        imdpth = 1
        rmxres = 0
        rmyres = 0
        imleft = 1
        imtop  = 1
        imrght = imnpix 
        imbot  = imnlin 
        rmxysc = 1.0
        imbswp = 0
        imnchl = IMISSD
        imprsz = IMISSD
        imdcsz = IMISSD
        imclsz = IMISSD
        imlvsz = IMISSD
        imvald = IMISSD
        imrdfl = 0
        immnpx = 0
        immxpx = 255
        imsorc = IMISSD
        imtype = IMISSD
C
        imradf = 0
        imdate = IMISSD
        imtime = IMISSD
        imndlv = IMISSD
        cmnlev = RMISSD
        cmbunt = ' '
        cmsorc = ' '
        cmtype = ' '
        cmlutf = 'GRAY'
        cmfile = imgfil
        cmcalb = ' '
C
C*	Set plotting area to be available space.
C
	xn = FLOAT ( imnpix )
	yn = FLOAT ( imnlin )
	CALL GSGRAF (1, 1, -1.0, 0.0, 0.0, xn, yn, ier)
C*
	RETURN
	END
