	SUBROUTINE GSGMGN  ( xl, yb, xr, yt, iret )
C************************************************************************
C* GSGMGN								*
C* 									*
C* This subroutine sets the margin size to be used in graph mode of 	*
C* the map/graph coordinate system.  The margin sizes may be 		*
C* specified as either a fraction of the view region or as a 		*
C* multiple of the current character size.  If the values entered 	*
C* are greater than 0 and less than 1, they are considered to		*
C* be a fraction of the view region.  If the values are 1 or 		*
C* greater, they are taken to be multiples of the character size 	*
C* at the time of the call to GSGMGN.  The default margin size is 	*
C* zero.								*
C* 									*
C* GSGMGN should be called before any plotting is done; margin 		*
C* size should not be changed after plotting has begun.  If size is	*
C* specified in terms of character size, the text size at the		*
C* time of the call is used.  Later changes to the size will not	*
C* affect the margins.							*
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
C* M. Vilardo/RDS	 9/84	GEMPLT Version 3.0			*
C* M. Goodman/RDS	 5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	 9/88	GEMPAK4					*
C* S. Jacobs/NMC	 1/95	Added multi-window common block		*
C* S. Jacobs/NMC	 2/95	Added USZG to save the margin text size	*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'XYDEF.CMN'
	INCLUDE 	'DEVSET.CMN'
	INCLUDE		'DEVREQ.CMN'
	INCLUDE		'DEVWIN.CMN'
C------------------------------------------------------------------------
C*	Check for graph mode
C
	IF  ( igmode .ne. 2 )  THEN
	    iret = NIMODE
	  ELSE
	    iret = NORMAL
C
C*	    Check if margins need to be set.
C
	    IF  ( ( ( 0. .le. xl ) .and. ( xl .lt. 1.0 ) .and.
     +		    ( 0. .le. yb ) .and. ( yb .lt. 1.0 ) .and.
     +		    ( 0. .le. xr ) .and. ( xr .lt. 1.0 ) .and.
     +		    ( 0. .le. yt ) .and. ( yt .lt. 1.0 ) )
     +				 .or.
     +		  ( ( xl .eq. 0. ) .or.  ( 1.0 .le. xl ) .and.
     +		    ( yb .eq. 0. ) .or.  ( 1.0 .le. yb ) .and.
     +		    ( xr .eq. 0. ) .or.  ( 1.0 .le. xr ) .and.
     +		    ( yt .eq. 0. ) .or.  ( 1.0 .le. yt ) ) )  THEN
		xlgmgn = xl
		ybgmgn = yb
		xrgmgn = xr
		ytgmgn = yt
		cszg   = rtxsz
		CALL UPDPXY
		umarg (ncurwn, 1) = xl
		umarg (ncurwn, 2) = yb
		umarg (ncurwn, 3) = xr
		umarg (ncurwn, 4) = yt
		uszg  (ncurwn)    = rtxsz
	      ELSE
		iret = NINVAL
	    END IF
	END IF
C*
	RETURN
	END
