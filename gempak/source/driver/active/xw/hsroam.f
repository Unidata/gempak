	SUBROUTINE HSROAM  ( irmflg, ipwdth, iphght, iret )
C************************************************************************
C* HSROAM - XW								*
C* 									*
C* This subroutine sets the roam on current driver.  			*
C* 									*
C* HSROAM  ( IRMFLG, IPWDTH, IPHGHT, IRET )				*
C*									*
C* Input parameters:                                                    *
C*      IRMFLG          INTEGER         Roam flag                       *
C*      IPWDTH          INTEGER         Pixmap(device) width            *
C*      IPHGHT          INTEGER         Pixmap(device) height           *
C*                                                                      *
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* C. Lin/EAI	 	 6/97						*
C* S. Maxwell/GSC	 6/97		Documentation changes		*
C************************************************************************
	INCLUDE		'DEVCHR.CMN'
C*
C------------------------------------------------------------------------
C
C*	Clear the pixmap.
C
	CALL XSROAM ( irmflg, ipwdth, iphght, idwdth, idhght, iret )
C
C*	Set the device size.
C
	ibot   = idhght - 1
	iright = idwdth - 1
C
	isxoff = 0
	isyoff = 0
C*
	RETURN
	END
