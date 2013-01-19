	SUBROUTINE VG_STRT  ( iret )
C************************************************************************
C* VG_STRT								*
C*									*
C* This subroutine starts the vertical interpolation code for GEMPAK	*
C* user interface.							*
C*									*
C* VG_STRT ( IRET )							*
C*									*
C* Output parameters:							*
C*	IRET		  INTEGER	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* K. Brill/NMC      06/92						*
C* Miller/COMET	      6/94	Remove CALL IN_BDTA			*
C* T. Piper/SAIC        01/08   Added GD_INIT; removed from IN_BDTA     *
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		respnd
C-----------------------------------------------------------------------
	iret = 0
C
C*  Initialize TAE.
C
	CALL IP_INIT  ( respnd, iret )
	CALL IP_IDNT  ( 'GDVINT', ier )
	IF  ( ( iret + ier ) .eq. 0 )  THEN
C
C*  Initialize GEMPLT.
C
	    mode = 1
	    CALL GG_INIT  ( mode, iret )
C
C*  Initialize grid library common area grdcmn.cmn
C 
	    CALL GD_INIT  ( ier )
	END IF
C*
	RETURN
	END
