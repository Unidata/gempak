	SUBROUTINE DSHASH ( szhsh, ihwid, ilwid, size, 
     +                      jhwid, jlwid, iret )
C************************************************************************
C* DSHASH								*
C* 									*
C* This subroutine sets the hash mark size, line width, and line 	*
C* spacing.  If these parameters are not positive, they are not changed.*
C* 									*
C* DSHASH  ( SZHSH, IHWID, ILWID, SIZE, JHWID, JLWID, IRET )		*
C*                                                                    	*
C* Input parameters:							*
C* 	SZHSH		REAL		Hash mark size multiplier	*
C* 				   	  <=0 = no change		*
C*	IHWID		INTEGER		Hash mark line width multiplier	*
C*					  <=0 = no change		*
C*	ILWID		INTEGER		Hash mark line spacing		*
C*					  <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL		Actual size			*
C* 	JHWID		INTEGER		Hash mark line width		*
C*	JLWID		INTEGER		Hash mark line spacing		*
C* 	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* I. Durham/GSC	03/98						*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Set arrow size, head size, width, and type in active common 
C*      area.
C
	IF ( szhsh .gt. 0. ) thshsz = szhsh
	IF ( ihwid .gt. 0  ) mhwid = ihwid
	IF ( ilwid .gt. 0  ) mlwidh = ilwid
	size = thshsz
	jhwid = mhwid
	jlwid = mlwidh
C
C*      If the driver is VG, send the attributes to the device
C*      for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSHASH ( thshsz, mhwid, mlwidh, iret )
	END IF
C*
	RETURN
	END
