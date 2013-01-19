	SUBROUTINE GSGMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GSGMGN								*
C* 									*
C* This subroutine sets the margin sizes to be used in graph mode of 	*
C* the map/graph coordinate system.  The margin sizes may be specified	*
C* as either a fraction of the view region or as a multiple of the	*
C* current character size.  If the values entered are greater than 0    *
C* and less than 1, they are considered to be a fraction of the view    *
C* region.  If the values are 1 or greater, they are taken to be        *
C* multiples of the character size at the time of the call to GSGMGN.   *
C* The default margin size is zero.					*
C* 									*
C* GSGMGN should be called before any plotting is done; margin size	*
C* should not be changed after plotting has begun.  If size is 		*
C* specified in terms of character size, the text size at the time 	*
C* of the call is used.  Later changes to the size do not affect 	*
C* the margins.								*
C* 									*
C* GSGMGN  ( XL, YB, XR, YT, IRET )					*
C*									*
C* Input parameters:							*
C* 	XL		REAL		Left margin size		*
C* 	YB		REAL		Bottom margin size		*
C* 	XR		REAL		Right margin size		*
C* 	YT		REAL		Top margin size 		*
C* 									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* L. Williams/EAi       3/94   Removed blank comments from header      *
C* A. Hardy/GSC          6/98   Cleaned up prolog                       *
C************************************************************************
	INCLUDE 	'FUNCCODE.PRM'
	INCLUDE 	'ERROR.PRM'
C*
	INTEGER 	isend (2)
	REAL 		rsend (4)
C------------------------------------------------------------------------
C*	Load input parameters into buffer and write them to the mailbox.
C
	isend (1) = 6
	isend (2) = FSGMGN
	rsend (1) = xl
	rsend (2) = yb
	rsend (3) = xr
	rsend (4) = yt
	CALL GPUT  ( isend, 2, iret )
	IF  ( iret .ne. NORMAL )  RETURN
	CALL GPUTR  ( rsend, 4, iret )
	IF  ( iret .ne. NORMAL )  RETURN
C
C*	Get output parameters.
C
	CALL GGET  ( iret, 1, ier )
	IF  ( ier .ne. NORMAL )  iret = ier
C*
	RETURN
	END
