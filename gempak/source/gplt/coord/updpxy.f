	SUBROUTINE UPDPXY
C************************************************************************
C* UPDPXY								*
C* 									*
C* This subroutine updates the coordinate systems for the P level.	*
C* 									*
C* UPDPXY								*
C**									*
C* Log:									*
C* G. Chatters/RDS	7/84						*
C* M. desJardins/GSFC	5/85	GEMPLT Version 3.1			*
C* M. desJardins/GSFC	3/86	Corrected margins for satellite proj	*
C* S. Jacobs/NMC	9/94	Removed check for MCGOES to set margins *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
C-------------------------------------------------------------------------
C*	Check to see if a device has been set before doing computations.
C
	IF ( ddev .NE. ' ' ) THEN
C
C*	        Compute positions of margin boundaries.  Computation depends
C*	        on whether margin width is specified in fraction or by 
C*	        character width.  First pick up margin size and display 
C*	        boundaries cooresponding to current mode.
C
	        IF ( igmode .eq. 2 ) THEN
		    xl = xlgmgn
	            yb = ybgmgn
		    xr = xrgmgn
		    yt = ytgmgn
		    sz = cszg
		  ELSE IF ( igmode .eq. 1 ) THEN
		    xl = xlmmgn
	            yb = ybmmgn
		    xr = xrmmgn
		    yt = ytmmgn
		    sz = cszm
		  ELSE
		    xl = 0.
		    yb = 0.
		    xr = 1.
		    yt = 1.
		    sz = 0.
	   	END IF
C
C*	   	Do margin position computation according to whether margin is
C*	   	specified as fractional width or multiple of character size.
C
	   	IF ( ( 0.0 .le. xl ) .and. ( xl .lt. 1.0 ) .and.
     *	             ( 0.0 .le. yb ) .and. ( yb .lt. 1.0 ) .and.
     *	             ( 0.0 .le. xr ) .and. ( xr .lt. 1.0 ) .and.
     *	             ( 0.0 .le. yt ) .and. ( yt .lt. 1.0 ) ) THEN
C*
	            xbndlw = xbndlv + xl * ( xbndrv - xbndlv )
	            ybndbw = ybndbv + yb * ( ybndtv - ybndbv )
	      	    xbndrw = xbndrv - xr * ( xbndrv - xbndlv )
	            ybndtw = ybndtv - yt * ( ybndtv - ybndbv )
C*
	          ELSE
	            xcsiz  =  7.0 * bscalc * sz / ABS ( andx1 )
	      	    ycsiz  =  9.0 * bscalc * sz / ABS ( andy1 )
	            xbndlw = xbndlv + xl * xcsiz
	            ybndbw = ybndbv + yb * ycsiz
	            xbndrw = xbndrv - xr * xcsiz
	            ybndtw = ybndtv - yt * ycsiz
	   	END IF
C
C*	        Get linear coordinate bounds.
C
		IF  ( ( igmode .eq. 2 ) .and. ( gset ) ) THEN
	      	    xbndll = xgbndl
	      	    ybndbl = ygbndb
	            xbndrl = xgbndr
	            ybndtl = ygbndt
		    width  = 1.
		    hgt    = yxgraf
		  ELSE IF  ( ( igmode .eq. 1 ) .and. ( mset ) ) THEN
		    xbndll = xmbndl
		    ybndbl = ymbndb
		    xbndrl = xmbndr
		    ybndtl = ymbndt
		    width  = ( xbndrl - xbndll )
		    hgt    = ( ybndtl - ybndbl )
		  ELSE
		    xbndll = 0.
		    ybndbl = 0.
		    xbndrl = 1.
		    ybndtl = 1.
		    width  = 1.
		    hgt    = 1.
	   	END IF
C
C*	   	Compute L to P scale and offset values.
C*	   	First compute X and Y direction scale factors separately,
C*	   	then select the one which will work for both axes.
C
		IF  ( ( igmode .eq. 2 ) .and. ( yxgraf .eq. 0. ) ) THEN
		  alpx0 = xbndlw
		  alpx1 = xbndrw - alpx0
		  alpy0 = ybndbw
		  alpy1 = ybndtw - alpy0
		 ELSE IF  ( igmode .eq. 2 ) THEN
		  scalx = ABS ( xbndrw - xbndlw )
		  scaly = ABS ( ( ybndtw - ybndbw ) / hgt )
		  IF  ( scalx .le. scaly ) THEN
		    alpx0 = xbndlw
		    alpx1 = xbndrw - alpx0
		    ymidp = ( ybndtw + ybndbw ) / 2.
		    dy    = ybndtw - ybndbw
		    dx    = ABS ( xbndrw - xbndlw )
		    sn    = SIGN ( 1., dy )
		    yb    = ymidp - .5 * hgt * dx * sn
		    yt    = ymidp + .5 * hgt * dx * sn
		    alpy0 = yb
		    alpy1 = yt - alpy0
		   ELSE
		    alpy0 = ybndbw
		    alpy1 = ybndtw - alpy0
		    xmidp = ( xbndlw + xbndrw ) / 2.
		    dy    = ABS ( ybndtw - ybndbw )
		    dx    = xbndrw - xbndlw
		    sn    = SIGN ( 1., dx )
		    xl    = xmidp - .5 * dy * sn / hgt
		    xr    = xmidp + .5 * dy * sn / hgt
		    alpx0 = xl
		    alpx1 = xr - alpx0
		  END IF
		 ELSE
	   	  xs = ( xbndrw - xbndlw ) / width
	   	  ys = ( ybndtw - ybndbw ) / hgt
	   	  scal1 = xs 
	   	  scal2 = ys
	   	  scal  = AMIN1 ( ABS ( scal1 ), ABS ( scal2 ) )
	   	  alpx1 = SIGN ( scal, xs )
	   	  alpy1 = SIGN ( scal, ys )
C*
	   	  xmidp = ( xbndrw + xbndlw ) / 2.0
	   	  ymidp = ( ybndtw + ybndbw ) / 2.0
	    	  xmidl = ( xbndrl + xbndll ) / 2.0
	   	  ymidl = ( ybndtl + ybndbl ) / 2.0
C*
	   	  alpx0 = xmidp - xmidl * alpx1
	   	  alpy0 = ymidp - ymidl * alpy1
		END IF
C
C*	   	Compute the bounds of the P area.
C
	   	xbndlp = alpx1 * xbndll + alpx0
	   	ybndbp = alpy1 * ybndbl + alpy0
	   	xbndrp = alpx1 * xbndrl + alpx0
	   	ybndtp = alpy1 * ybndtl + alpy0
C
C*	   	Note: INTDLM ( x, ix1, ix2 ) rounds x towards ix1.
C
	   	ixwlp = INTDLM (andx1 * xbndlp + andx0, ixbndl, ixbndr )
	   	iywbp = INTDLM (andy1 * ybndbp + andy0, iybndb, iybndt )
	   	ixwrp = INTDLM (andx1 * xbndrp + andx0, ixbndr, ixbndl )
	   	iywtp = INTDLM (andy1 * ybndtp + andy0, iybndt, iybndb )
C*
	END IF
C
C*	Update clipping windows.
C
	CALL UPWNDW
C
	RETURN
	END
