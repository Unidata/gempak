	SUBROUTINE HDOTS  ( ix, iy, ilwid, iret )
C************************************************************************
C* HDOTS - TIFF								*
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
C* S. Jacobs/NCEP	12/98						*
C************************************************************************
C------------------------------------------------------------------------
	CALL TDOTS ( ix, iy, ilwid, iret )
C*
	RETURN
	END
