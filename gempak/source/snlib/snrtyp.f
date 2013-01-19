 	SUBROUTINE SN_RTYP  ( isnfln, nlev, idtype, iret )
C************************************************************************
C* SN_RTYP								*
C*									*
C* This subroutine returns the report type for each level in a		*
C* sounding.  IDTYPE will be set to 1, 2, or 3 for mandatory,		*
C* significant temperature or significant wind data.  If the		*
C* data set contains merged data, all the data flags will be set		*
C* to 1.								*
C*									*
C* SN_RTYP  ( ISNFLN, NLEV, IDTYPE, IRET )				*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*									*
C* Output parameters:							*
C*	NLEV		INTEGER		Number of levels		*
C*	IDTYPE (NLEV)	INTEGER		Report type flags		*
C*					  1 = mandatory			*
C*					  2 = sig temperature		*
C*					  3 = sig wind			*
C*	IRET		INTEGER	 	Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* M. desJardins/GSFC	 1/89	Added level types			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
	INCLUDE		'sncmn.cmn'
C*
	INTEGER		idtype (*)
C------------------------------------------------------------------------
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Get the data from the common area.
C
	nlev = isnlev ( isnfln )
	DO  i = 1, nlev
	    idtype ( i ) = mdtype ( i, isnfln )
	END DO
C*
	RETURN
	END
