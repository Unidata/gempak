	SUBROUTINE DSCRGB  ( icolr, ired, igreen, iblue, iret )
C************************************************************************
C* DSCRGB								*
C*									*
C* This subroutine defines the color components of a color by		*
C* specifying the values of red, green, and blue.  The color components	*
C* must be in the range 0 - 255.					*
C*									*
C* DSCRGB  ( ICOLR, IRED, IGREEN, IBLUE, IRET )				*
C*									*
C* Input parameters:							*
C* 	ICOLR		INTEGER		Color number			*
C*	IRED		INTEGER		Red color component		*
C*	IGREEN		INTEGER		Green color component		*
C*	IBLUE		INTEGER		Blue color component		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 8/85						*
C* M. desJardins/GSFC	 5/88	Documentation				*
C* M. desJardins/GSFC	 5/89	Check NCOL saved in common		*
C* M. desJardins/NMC	01/92	Check for change of background color	*
C* M. desJardins/NMC	02/92	Check for numbers in range 0 - 1	*
C* G. Krueger/EAI	11/95	Removed HLS;Added XNAME; Mod. RGB range	*
C* L. Williams/EAI	 3/96   Removed cname, icred, icgrn, icblue and *
C*				cxname					*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE 	'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C
C*	Exit, if this is not a color device.
C
	IF  ( .not. colcmp )  RETURN
C
C*	Check that this is valid color number on this device.
C
	IF  ( ( icolr .gt. nncolr ) .and. ( icolr .ne. 101 ) )  THEN
	    iret = NICNUM
	    RETURN
	END IF
C
C*	Set color components on the device.
C
	CALL HSCRGB  ( icolr, ired, igreen, iblue, iret )
C*
	RETURN
	END
