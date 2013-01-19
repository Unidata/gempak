	SUBROUTINE HDOTS  ( ix, iy, ilwid, iret )
C************************************************************************
C* HDOTS - FAX								*
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
C* E. Wehner/EAi	7/96	Adopted to call raster C routine	*
C* S. Maxwell/GSC       6/97    Documentation changes                   *
C************************************************************************
C------------------------------------------------------------------------
	CALL RDOTS ( ix, iy, ilwid, iret )
C*
	RETURN
	END
