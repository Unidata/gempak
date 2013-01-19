	SUBROUTINE DSCMBO ( szcmwx, icmbwd, size, jcmbwd, iret )
C************************************************************************
C* DSCMBO								*
C* 									*
C* This subroutine sets the combination weather symbol parameters.  If  *
C* these parameters are not positive, no changes are made.   		*
C* 									*
C* DSCMBO  ( SZCMWX, ICMBWD, SIZE, JCMBWD, IRET )			*
C*                                                                    	*
C* Input parameters:							*
C* 	SZCMWX		REAL	     Combination symbol size multiplier	*
C* 					  <=0 = no change		*
C*      ICMBWD		INTEGER	     Combination symbol width multiplier*
C*                                        <=0 = no change		*
C* Output parameters:							*
C*	SIZE		REAL	     Combination weather symbol size	*
C*	JCMBWD		INTEGER      Combination weather symbol width	*
C* 	IRET		INTEGER	     Return code			*
C**									*
C* Log:									*
C* A. Hardy/GSC		10/98	Copied from DSWTHR                      *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVACT.CMN'
	INCLUDE		'DEVCHR.CMN'
C-----------------------------------------------------------------------
	iret = NORMAL
C*
	IF  ( szcmwx .gt. 0. )   tcsysz = szcmwx
	IF  ( icmbwd .gt. 0  )   mcsywd = icmbwd
	size = tcsysz
        jcmbwd = mcsywd
C
C*	If the driver is VG, send the attributes to the device
C*	for output.
C
	IF  ( ddev .eq. 'VG' )  THEN
	    CALL HSSYMB ( 9, 0, size, jcmbwd, iret )
	END IF
C*
	RETURN
	END
