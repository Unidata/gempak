	SUBROUTINE GQSYSZ  ( szmkx, szmky, sztxx, sztxy, szwbx, 
     +			     szwby, iret )
C************************************************************************
C* GQSYSZ								*
C*									*
C* This subroutine returns the current size of text, markers 		*
C* and wind barbs in terms of normalized device coordinates.		*
C*									*
C* GQSYSZ  ( SZMKX, SZMKY, SZTXX, SZTXY, SZWBX, SZWBY, IRET )		*
C*									*
C* Output parameters:							*
C*	SZMKX		REAL		Width of markers		*
C*	SZMKY		REAL		Height of markers		*
C*	SZTXX		REAL		Width of text characters	*
C*	SZTXY		REAL		Height of text characters	*
C*	SZWBX		REAL		Length of wind barbs if		*
C*					oriented along x axis		*
C*	SZWBY		REAL		Length of wind barbs if		*
C*					oriented along y axis		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. Vilardo/RDS	10/84	GEMPLT Version 3.0			*
C* I. Graffman/RDS	 5/85	GEMPLT Version 3.1			*
C* I. Graffman/RDS	 2/88	Fix check for no device			*
C* M. desJardins/GSFC	 6/88	Documentation				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'DEVCHR.CMN'
	INCLUDE		'DEVSET.CMN'
	INCLUDE		'XYDEF.CMN'
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	Check to see if a device has been set before doing computations.
C
	IF  ( ddev .ne. ' ' )  THEN
C
C*	Calculate label, marker, text, wind arrow, and wind barb width
C*	and height in normalized device coordinates.
C
	  szmkx =  7.0 * smksz * bscalm / ABS ( andx1 )
	  szmky =  szmkx		
  	  sztxx =  7.0 * stxsz * bscalc / ABS ( andx1 )
          sztxy =  9.0 * stxsz * bscalc / ABS ( andx1 )
	  szwbx =  swbsz * bscalb / ABS ( andx1 )
	  szwby =  szwbx
C
	ELSE
	  szmkx  =  0.0
	  szmky  =  0.0
	  szwbx  =  0.0
	  szwby  =  0.0
	  sztxx  =  0.0
	  sztxy  =  0.0
	END IF
C*
	RETURN
	END
