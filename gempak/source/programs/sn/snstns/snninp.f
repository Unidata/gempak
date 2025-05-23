	SUBROUTINE SNNINP  ( snfile, stnfil, addstn, idntyp, iret )
C************************************************************************
C* SNNINP								*
C*									*
C* This subroutine gets input for SNSTNS.				*
C*									*
C* SNNINP  ( SNFILE, STNFIL, ADDSTN, IDNTYP, IRET )			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Rewritten				*
C************************************************************************
	CHARACTER*(*)	snfile, stnfil, idntyp
	LOGICAL		addstn
C------------------------------------------------------------------------
	CALL IP_STR  ( 'SNFILE', snfile, ier1 )
	CALL IP_STR  ( 'STNFIL', stnfil, ier2 )
	CALL IP_LOG  ( 'ADDSTN', addstn, ier3 )
	CALL IP_STR  ( 'IDNTYP', IDNTYP, ier4 )
C
	ier = ier1 + ier2 + ier3 + ier4 
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
