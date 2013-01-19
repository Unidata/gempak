	SUBROUTINE GSMMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GSMMGN								*
C* 									*
C* This subroutine sets the margin size to be used in map mode.  The 	*
C* margin sizes may be specified as either a fraction of the view	*
C* region or as a multiple of the current character size.  If the 	*
C* values entered are greater than 0.0 and less than 1.0, they are	*
C* considered to be a fraction of the view region.  If the values are	*
C* 1.0 or greater, they are taken to be multiples of the character	*
C* size at the time of the call to GSMMGN.  The default margin size	*
C* is zero.								*
C*									*
C* GSMMGN should be called before any plotting is done; margin 		*
C* size should not be changed after plotting has begun.  If size 	*
C* is specified in terms of character size, the size at the time 	*
C* of the call is used.  Later changes to the text size will not 	*
C* affect the margins.							*
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
C* J.M. Vilardo/RDS	 6/84	GEMPLT Version 3.0			*
C* M. desJardins/GSFC	 6/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 6/88	Cleaned up				*
C* K. Brill/NMC		10/91	Documentation error fixed.		*
C* S. Jacobs/NMC	 1/95	Added multi-window common block		*
C* S. Jacobs/NMC	 2/95	Added USZM to save the margin text size	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVWIN.CMN'
C------------------------------------------------------------------------
C*	Check for map mode.
C
	IF ( igmode .ne. 1 ) THEN
	    iret = NIMODE
	  ELSE
	    iret = NORMAL
C
C*	    Check if margins need to be set
C
	    IF (( ( 0. .le. xl ) .and. ( xl .lt. 1.0 ) .and.
     +		  ( 0. .le. yb ) .and. ( yb .lt. 1.0 ) .and.
     +		  ( 0. .le. xr ) .and. ( xr .lt. 1.0 ) .and.
     +		  ( 0. .le. yt ) .and. ( yt .lt. 1.0 ) )
     +				 .or.
     +		( ( xl .eq. 0. ) .or.  ( 1.0 .le. xl ) .and.
     +		  ( yb .eq. 0. ) .or.  ( 1.0 .le. yb ) .and.
     +		  ( xr .eq. 0. ) .or.  ( 1.0 .le. xr ) .and.
     +		  ( yt .eq. 0. ) .or.  ( 1.0 .le. yt ) )) THEN
C
		xlmmgn = xl
		ybmmgn = yb
		xrmmgn = xr
		ytmmgn = yt
		cszm   = rtxsz
		CALL UPDPXY
		umarg (ncurwn, 1) = xl
		umarg (ncurwn, 2) = yb
		umarg (ncurwn, 3) = xr
		umarg (ncurwn, 4) = yt
		uszm  (ncurwn)    = rtxsz
	      ELSE
		iret = NINVAL
	    END IF
	END IF
C*
	RETURN
	END
