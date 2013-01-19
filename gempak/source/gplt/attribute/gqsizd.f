	SUBROUTINE GQSIZD  ( szmk, sztx, szwb, szws, szab, szah,
     +			     iret )
C************************************************************************
C* GQSIZD								*
C*									*
C*	This subroutine	returns the size of text, markers, wind barbs,  *
C*	weather symbols, arrow bodies, and arrow heads in device        *
C*	coordinates.  The values returned are for size=1.		*
C*									*
C* GQSIZD ( SZMK, SZTX, SZWB, SZWS, SZAB, SZAH, IRET)			*
C*									*
C* Output parameters:							*
C*	SZMK		REAL		Size of markers.		*
C*	SZTX		REAL		Size of text characters.	*
C*	SZWB		REAL		Size of wind barbs.		*
C*	SZWS		REAL		Size of weather symbols.	*
C*	SZAB		REAL		Size of arrow bases.		*
C*	SZAH		REAL		Size of arrow heads.		*
C*	IRET		INTEGER		Return code.			*
C**									*
C* Log:									*
C* I. Durham/GSC	12/97		Created GQSIZD			*
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
C*	Calculate marker, text character, wind barb, weather symbol,
C*	arrow base, and arrow head sizes in normalized device coordinates.
C
	  szmk =  7.0 * bscalm
          sztx =  9.0 * bscalc
	  szwb =  bscalb
	  szws =  11.0 * bscalw
	  szab =  bscala
	  szah =  bscalh
C
	ELSE
	  szmk  =  0.0
	  sztx  =  0.0
	  szwb  =  0.0
	  szws  =  0.0
	  szab  =  0.0
	  szah  =  0.0
	END IF
C*
	RETURN
	END
