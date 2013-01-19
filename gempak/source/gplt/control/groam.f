      SUBROUTINE GROAM  ( ityp, sys, x, y, iret )
C************************************************************************
C* GROAM								*
C* 									*
C* This subroutine roams the current window to the specified position   *
C* in any coordinate system except 'S'. The base point of the roam can  *
C* be upper left of the screen or the center of the screen.             *
C* 									*
C* GROAM  ( ITYP, SYS, X, Y, IRET )					*
C* 									*
C* Input parameters:                                                    *
C*      ITYP            INTEGER         The base point of roam          *
C*                                        0 = upper left screen corner  *
C*                                        1 = center of the screen      *
C*                                        2 = delta_x, delta_y      	*
C*      SYS       CHAR*         Coordinate system               	*
C*                                        'S' = screen coordinates      *
C*                                        'D' = device coordinates      *
C*                                        'N' = normalized coordinates  *
C*                                        'V' = view coordinates        *
C*                                        'P' = plot coordinates        *
C*                                        'M' = map coordinates         *
C*                                        'G' = grid coordinates        *
C*      X          REAL         Upper left x coordinate            	*
C*      Y          REAL         Upper left y coordinate           	*
C*                                                                      *
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI		 6/97						*
C* D.W.Plummer/NCEP	 6/97	Change delta roam to center of screen	*
C* D.W.Plummer/NCEP	 7/97	Add assignment of ityp to itypx		*
C************************************************************************
	INCLUDE 	'ERROR.PRM'
	INCLUDE 	'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
        INCLUDE         'GEMPRM.PRM'
C*
        CHARACTER*(*)   sys
C
        CHARACTER       sysuc*1
C------------------------------------------------------------------------
	iret = NORMAL
C
	itypx = ityp
C
C*      Check that device has been set. If the current device is not
C*	'XW' or 'XWP', do nothing
C
        IF  ( ddev .eq. ' ' ) THEN
            iret = NDVICE
            RETURN
	  ELSE IF ( (ddev .ne. 'XW') .and. (ddev .ne. 'XWP') ) THEN
	    RETURN
        END IF
C
C*      Find coordinate system 
C
        CALL ST_LCUC ( sys, sysuc, ier )
        isys = INDEX ( sysup, sysuc )
        IF  ( isys .le. 1 ) THEN
            iret = NOCORD
            RETURN
        END IF
C
C*      Process the delta_x, delta_y for type 2 
C
	IF ( itypx .eq. 2 ) THEN
C
C*          Call GTRANS to transform the current offset in input 
C*	    coordinates 
C
	    xp = ( xbndls + xbndrs ) / 2
	    yp = ( ybndts + ybndbs ) / 2
      	    CALL GTRANS('D', sysuc, 1, xp, yp, gx, gy, ier )
	    x = gx + x
	    y = gy + y
	    itypx = 1
	END IF
C
C*      Call GTRANS to transform the input to 'D' coordinates 
C
      	CALL GTRANS(sysuc, 'D', 1, x, y, gx, gy, iret )
C
	CALL DROAM  ( itypx, gx, gy, iret )
C
C*      If the return code from DROAM to be normal, then 
C*      set the new offsets.
C
        IF  ( iret .eq. NORMAL )  THEN
            CALL DQDCHR  ( nncolr, ier )
C
C*          Move the offsets into the XYDEF common area.
C
	    ixos = isxoff
	    iyos = isyoff
C
            IF ( ileft .le. iright ) THEN
                ibndls = ileft + isxoff
                ibndrs = ileft + isxoff + iswdth
              ELSE
                ibndls = iright + isxoff + iswdth
                ibndrs = iright + isxoff
            END IF
C
            IF ( itop .le. ibot ) THEN
                ibndts = itop + isyoff
                ibndbs = itop + isyoff + ishght
              ELSE
                ibndts = ibot + isyoff + ishght
                ibndbs = ibot + isyoff
            END IF
C
C*          Update the bounds
C
            xbndls = FLOAT ( ibndls )
            ybndbs = FLOAT ( ibndbs )
            xbndrs = FLOAT ( ibndrs )
            ybndts = FLOAT ( ibndts )

        END IF
C*
	RETURN
	END
