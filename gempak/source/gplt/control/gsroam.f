      SUBROUTINE GSROAM  ( irmflg, ipwdth, iphght, iret )
C************************************************************************
C* GSROAM								*
C* 									*
C* This subroutine sets up the roam.					*
C* 									*
C* GSROAM  ( IRMFLG, IPWDTH, IPHGHT, IRET )				*
C* 									*
C* Input parameters:							*
C*      IRMFLG          INTEGER         Roam flag                       *
C*      IPWDTH          INTEGER         Pixmap(device) width            *
C*      IPHGHT          INTEGER         Pixmap(device) height           *
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI		 6/97						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'XYDEF.CMN'
C*
C------------------------------------------------------------------------
	iret = NORMAL
C
	IF  ( ddev .ne. ' ' )  THEN
C
	    CALL DSROAM  ( irmflg, ipwdth, iphght, iret )
C
C*	Set the new size information.
C
	    CALL DQDCHR  ( nncolr, ier )
C
C*	Move the screen bounds into the XYDEF common area.
C
	    IF ( ileft .le. iright) THEN
		ibndls = ileft + isxoff
		ibndrs = ileft + iswdth + isxoff
	    ELSE
		ibndls = iright + iswdth + isxoff
		ibndrs = iright + isxoff
	    END IF 
C
	    IF ( itop .le. ibot ) THEN
		ibndbs = itop + ishght + isyoff 
		ibndts = itop + isyoff 
	    ELSE
		ibndbs = ibot + isyoff
		ibndts = ibot + ishght + isyoff 
	    END IF
C
C*	Move the device bounds into the XYDEF common area.
C
	    ixbndl = ileft
	    iybndb = ibot
	    ixbndr = iright
	    iybndt = itop
C
	    ixos   = isxoff
	    iyos   = isyoff
C
C*	Update transformation information.
C
	    CALL UPDSXY
	END IF
C*
	RETURN
	END
