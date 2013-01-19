	SUBROUTINE HSCLR2  ( icolr, iclr2,  iret )
C************************************************************************
C* HSCLR2 - VG								*
C* 									*
C* This subroutine sets the color on a graphics device.			*
C* 									*
C* HSCLR2  ( ICOLR, ICLR2, IRET )					*
C* 									*
C* Input parameters:							*
C*	ICOLR		INTEGER		Color number			*
C*      ICLR2           INTEGER         Color number                    *
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Li/GSC		 7/00		Created	  			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
C------------------------------------------------------------------------
	iret = NORMAL
C*
	CALL VSCLR2 ( icolr, iclr2, iret )
C*
	RETURN
	END
