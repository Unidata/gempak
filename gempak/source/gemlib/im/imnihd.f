	SUBROUTINE IM_NIHD  ( imgfil, iret )
C************************************************************************
C* IM_NIHD								*
C*									*
C* This subroutine reads the header information from a raw NIDS file	*
C* and sets the navigation.						*
C*									*
C* IM_NIHD  ( IMGFIL, IRET )						*
C*									*
C* Input parameters:							*
C*	IMGFIL		CHAR*		Image file name			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = Error opening/reading file*
C*					 -3 = Invalid image file format *
C*					 -4 = Invalid image product	*
C*					 -5 = Invalid image navigation	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 2/95						*
C* C. Lin/EAI	 	 6/95  	change input filename -> lunmf		*
C* J. Cowie/COMET	10/95	Add image size maximum check, set xy	*
C*				image scaling to 1.0			*
C* J. Cowie/COMET	 4/96	Added calib setting			*
C* J. Cowie/COMET	 1/97	Changed IMGDEF common variable names	*
C* S. Chiswell/UNIDATA	12/97	Handle byte flipping			*
C* J. Cowie/COMET	12/97	Added imradf, removed unused variables,	*
C*				add cross section products		*
C* S. Jacobs/NCEP	12/97	Added TB_NIDS; Removed NIPRM.PRM	*
C* T. Piper/GSC		11/98	Updated prolog				*
C* S. Jacobs/NCEP	 5/99	Added setting of imbswp for byte-swap	*
C* S. Jacobs/NCEP	 2/00	Use the 1st time in the file, not 2nd	*
C* S. Jacobs/NCEP	11/00	Changed to call IM_NIDH			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'IMGDEF.CMN'
C*
	CHARACTER*(*)	imgfil
C*
	INTEGER*4	inhead (39)
C------------------------------------------------------------------------
	iret = 0
C
C*	Open the file and read the Message Header and Product
C*	Description blocks
C
	CALL FL_DOPN  ( imgfil, 39, .false., lunmf, iret )
        READ (UNIT=lunmf, REC=1, IOSTAT=io) inhead
C
        IF  ( io .ne. 0 )  THEN
            iret = -2
            RETURN
        END IF
	CALL FL_CLOS  ( lunmf, ier )
C
C*	Parse the header information into the individual values.
C
	CALL IM_NIDH ( imgfil, inhead, iret )
C
	RETURN
	END
