	SUBROUTINE HSATPX ( dx, dy, offx, offy, imgfil, ixout0, iyout0, 
     &		ixout1, iyout1, iarea, mode, ipix, dxo, dyo, iret )
C************************************************************************
C* HSATPX - GN								*
C*									*
C* This subroutine gets a pixel value from an image file.		*
C*									*
C* HSATPX ( DX, DY, OFFX, OFFY, IMGFIL, IXOUT0, IYOUT0, IXOUT1, IYOUT1, *
C*          IAREA, MODE, IPIX, DXO, DYO, IRET )				*
C*									*
C* Input parameters:							*
C*	DX		REAL		X coordinates			*
C*	DY		REAL		Y coordinates			*
C*	OFFX		INTEGER		Offset X			*
C*	OFFY		INTEGER		Offset Y			*
C*	IMGFIL		CHAR*		Image file name			*
C*	IXOUT0		INTEGER		Left of the image		*
C*	IYOUT0		INTEGER		Bottom of the image		*
C*	IXOUT1		INTEGER		Right of the image		*
C*	IYOUT1		INTEGER		Top of the image		*
C*	IAREA		INTEGER		Pixel area indicator		*
C*	MODE		INTEGER		Pixel mode indicator		*
C*									*
C* Output parameters:							*
C*	IPIX		INTEGER		Pixel value			*
C*	DXO		REAL		Device Coordinate X		*
C*	DYO		REAL		Device Coordinate Y		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* T. Lee/GSC		 9/99	Created					*
C* T. Lee/GSC		12/99	Added pixel area and mode retrieval	*
C* T. Piper/SAIC	1/02	set ipix value				*
C* D.W.Plummer/NCEP	 6/04	Calling seq chg for offset and dxo,dyo	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*
	CHARACTER*(*) 	imgfil
C------------------------------------------------------------------------
	ipix = 0
	iret = NORMAL
C*
	RETURN
	END
