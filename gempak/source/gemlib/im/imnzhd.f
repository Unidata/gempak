	SUBROUTINE IM_NZHD  ( imgfil, barr, ipdoff, iret )
C************************************************************************
C* IM_NZHD								*
C*									*
C* This subroutine reads the header information from a raw NIDS array	*
C* and sets the navigation.						*
C*									*
C* IM_NZHD  ( IMGFIL, BARR, IPDOFF, IRET )				*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*	BARR(*)		BYTE		Input NIDS product		*
C*	IPDOFF		INTEGER		Start offset of product in file	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* S. Chiswell/Unidata	11/00	Copied from IM_NIHD to file I/O		*
C*				note: imprsz is used for prefix offset 	*
C*				device driver (typically used for AREA)	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
	BYTE		barr (*)
C*
	BYTE		bhdr (156)
	INTEGER*2	iarr2 (78)
	INTEGER*4	inhead (39)
C*
	EQUIVALENCE	(bhdr, inhead)
	EQUIVALENCE	(bhdr, iarr2)
C------------------------------------------------------------------------
	iret = 0
C
C*	Use only the first 156 bytes from the input array.
C
	DO  i = 1, 156
	    bhdr (i) = barr (i)
	END DO
C
C*	Use prefix line size common block variable to hold offset size
C*	to beginning of zlib portion of product
C
	imprsz = ipdoff
C
C*	Parse the header information into the individual values.
C
	CALL IM_NIDH ( imgfil, inhead, iret )
C
	RETURN
	END
