	SUBROUTINE HSICMN ( iret )
C************************************************************************
C* HSICMN - GIF 							*
C*                                                                      *
C* This subroutine sets image common information                        *
C*                                                                      *
C* HSICMN (IRET )                                                       *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* J. Cowie/COMET        3/95                                           *
C* T. Lee/GSC		 7/00	Renamed xsicmn to wsicmn		*
C************************************************************************
        INCLUDE         'IMGDEF.CMN'
	INCLUDE		'driver.cmn'
C------------------------------------------------------------------------
C
C*      Pass the first variable of the common block
C
        CALL WSICMN ( imftyp, iret )
C
C*	Save color bank ID in common.
C
	imgtyp = imbank
C*
        RETURN
        END
