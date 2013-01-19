	SUBROUTINE HSROAM  ( irmflg, ipwdth, iphght, iret )
C************************************************************************
C* HSROAM - GN								*
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
C* S. Maxwell/GSC        6/97   Documentation changes                   *
C************************************************************************
	INCLUDE		'ERROR.PRM'
C*	
C------------------------------------------------------------------------
        iret = NORMAL
C*
	RETURN
	END
