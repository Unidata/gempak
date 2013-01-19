	SUBROUTINE GSMMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GSMMGN								*
C* 									*
C* This subroutine sets the margin sizes to be used in map mode of the	*
C* map/graph coordinate system.  The margin sizes may be specified as	*
C* either a fraction of the view region or as a multiple of the current	*
C* character size.  If the values entered are greater than 0.0 and less	*
C* than 1.0, they are considered to be a fraction of the view region.	*
C* If the values are 1.0 or greater, they are taken to be multiples of 	*
C* the character size at the time of the call to GSMMGN.  The default 	*
C* margin size is zero.							*
C* 									*
C* GSMMGN should be called before any plotting is done; margin size	*
C* should not be changed after plotting has begun.  If size is 		*
C* specified in terms of character size, the size at the time of the	*
C* call is used.  Later changes to the text size do not affect the	*
C* margins.								*
C* 									*
C* GSMMGN  ( XL, YB, XR, YT, IRET )					*
C*									*
C* Input parameters:							*
C*	XL		REAL		Left margin size		*
C*	YB		REAL		Bottom margin size		*
C*	XR		REAL		Right margin size		*
C*	YT		REAL		Top margin size			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 4/84	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* K. Brill/NMC		10/91	Documentation error fixed 		*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE		'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (2)
	REAL 		rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C*
	isend (1) = 6
	isend (2) = FSMMGN
	rsend (1) = xl
	rsend (2) = yb
	rsend (3) = xr
	rsend (4) = yt
C*
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL ) RETURN
C*
	CALL GPUTR  ( rsend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C*
C*	If write is successful, get output parameters.
C*
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
