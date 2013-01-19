	SUBROUTINE SN_MTYP  ( isnfln, iztype, iret )
C************************************************************************
C* SN_MTYP								*
C*									*
C* This subroutine specifies the type of interpolation to be used	*
C* for the height field in an unmerged data set.  This interpolation	*
C* adds heights to significant temperature levels.  The default merge	*
C* type is 3.  If IZTYPE is 1, the height will be interpolated with	*
C* respect to the logarithm of pressure.  If IZTYPE is 2, the moist	*
C* hydrostatic height field is computed.  If IZTYPE is 3, the moist	*
C* hydrostatic field is computed as in 2, but is scaled to retain the	*
C* height values received with the mandatory data.			*
C*									*
C* SN_MTYP  ( ISNFLN, IZTYPE, IRET )					*
C*									*
C* Input parameters:							*
C*	ISNFLN		INTEGER		Sounding file number		*
C*	IZTYPE		INTEGER		Type of height interpolation	*
C*					  1 = int wrt log p		*
C*					  2 = moist hydrostatic comp	*
C*					  3 = scaled moist hydro comp	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -4 = file not open		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'sncmn.cmn'
C------------------------------------------------------------------------
C*	Check that the file is open.
C
	CALL SN_CHKF  ( isnfln, iret )
	IF  ( iret .ne. 0 )  RETURN
C
C*	Check that the input value is in the correct range.
C
	IF  ( ( iztype .lt. 1 ) .or. ( iztype .gt. 3 ) )  THEN
	    imrtyp ( isnfln ) = 3
	  ELSE
	    imrtyp ( isnfln ) = iztype
	END IF
C*
	RETURN
	END
