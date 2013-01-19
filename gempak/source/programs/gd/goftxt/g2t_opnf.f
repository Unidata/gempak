	SUBROUTINE G2T_OPNF ( nz, lunt, iret )
C************************************************************************
C* G2T_OPNF  								*
C*									*
C* This subroutine opens the G2T OFF text files.  The output file name	*
C* is based on the bound type (alias) and number of zone areas.		*
C*									*
C* G2T_OPEN ( NZ, LUNT, IRET )						*
C*									*
C* Input parameters:							*
C*	NZ		INTEGER		Nth zone area			*
C*									*
C* Output parameters:							*
C*	LUNT		INTEGER		LUN for output text file	*
C*	IRET		INTEGER		Return code			*
C*					 0 = normal return		*
C*					-1 = cannot open file		*
C*									*
C**									*
C* Log:									*
C* T. Lee/SAIC		 9/06						*
C* T. Lee/SAIC		02/08	
C************************************************************************
	INCLUDE		'goftxt.cmn'
	CHARACTER	outfil*12, cz*2
C-----------------------------------------------------------------------
	iret = 0
C
	CALL ST_INCH ( nz, cz, ier )
	CALL ST_LSTR ( cz, ncz, ier )
	CALL ST_LSTR ( bndtyp, len, ier )
	ipos = INDEX ( bndtyp, '_' )
	outfil = bndtyp ( ipos + 1 : len ) // '_' // cz ( : ncz )
C
	CALL FL_SWOP ( outfil, lunt, ier1 )
C
	IF  ( ier .lt. 0 )  THEN
	    iret = - 1
	    RETURN
	END IF
C*
	RETURN
	END
