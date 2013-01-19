	SUBROUTINE HDOTS  ( ix, iy, ilwid, iret )
C************************************************************************
C* HDOTS - PS								*
C*									*
C* This subroutine draws a dot on a graphics device.			*
C*									*
C* HDOTS  ( IX, IY, ILWID, IRET )					*
C*									*
C* Input parameters:							*
C*	IX 		INTEGER		X coordinates			*
C*	IY 		INTEGER		Y coordinates			*
C*	ILWID		INTEGER		Line width			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/91						*
C* M. desJardins/NMC	 4/91	Added psplot				*
C* J. Whistler/SSAI	 6/91	Fixed bad placement of psplot		*
C* A. Chang/EAI	 	 2/94	Modified to call C routine		*
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
	CALL PDOTS ( ix, iy, ilwid, iret )
C*
	RETURN
	END
