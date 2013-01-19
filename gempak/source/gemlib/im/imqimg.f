	SUBROUTINE IM_QIMG  ( imgrad, imgsrc, imgtyp, imgdep, iret )
C************************************************************************
C* IM_QIMG								*
C*									*
C* This subroutine returns image information from the image common	*
C* block.								*
C*									*
C* IM_QIMG  ( IMGRAD, IMGSRC, IMGTYP, IMGDEP, IRET )			*
C*									*
C* Output parameters:							*
C*	IMGRAD		INTEGER		Radar image flag		*
C*	IMGSRC		INTEGER		Image source			*
C*	IMGTYP		INTEGER		Image type ID			*
C*	IMGDEP		INTEGER		Pixel depth (bytes)		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = Invalid image product	*
C**									*
C* Log:									*
C* T. Piper/SAIC	 09/06	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
C------------------------------------------------------------------------
C
C*	Check if the image file is valid.
C
	IF ( imftyp .eq. IFINVD ) THEN
	    imgrad = IMISSD
	    imgsrc = IMISSD
	    imgtyp = IMISSD
	    imgdep = IMISSD
	    iret = -4
	ELSE
	    imgrad = imradf
	    imgsrc = imsorc
	    imgtyp = imtype
	    imgdep = imdpth
	    iret = 0
	END IF
C	
	RETURN
	END
