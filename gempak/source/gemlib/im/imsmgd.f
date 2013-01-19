	SUBROUTINE IM_SMGD ( sat_id, imgtyp, minpix, maxpix,
     +						lutfil, iret )
C************************************************************************
C* IM_SMGD								*
C*									*
C* This subroutine sets image information in the image common block.	*
C*									*
C* IM_SMGD  ( SAT_ID, IMGTYP, MINPIX, MAXPIX, LUTFIL, IRET )		*
C*									*
C* Input parameters:							*
C*	SAT_ID		CHAR*		Image source (e.g., GOES11)	*
C*	IMGTYP		CHAR*		Image type (e.g., IR)		*
C*	MINPIX		INTEGER		Min pixel value			*
C*	MAXPIX		INTEGER		Max pixel value			*
C*	LUTFIL		CHAR*		LUT file name			*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = Invalid image product	*
C**									*
C* Log:									*
C* T. Piper/SAIC	 09/06	Created					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'

	CHARACTER*(*)	sat_id, imgtyp, lutfil
C*
C------------------------------------------------------------------------
C
C*	Check if the image file is valid.
C
	IF ( imftyp .eq. IFINVD ) THEN
	    iret = -4
	ELSE
	    cmsorc = sat_id
	    cmtype = imgtyp
	    immnpx = minpix
	    immxpx = maxpix
	    cmlutf = lutfil
	    iret = 0 
	END IF
C	
	RETURN
	END
