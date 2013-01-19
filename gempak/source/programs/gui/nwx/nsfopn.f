	SUBROUTINE NSFOPN  ( datatype, filnam, iflno, iret )
C************************************************************************
C* NSFOPN								*
C*									*
C* This subroutine opens a surface file for NWX c routines.		*
C*									*
C* NSFOPN  ( DATATYPE, FILNAM, IFLNO, IRET )				*
C*									*
C* Input parameters:							*
C*	DATATYPE	CHAR*		Data Type (SFC_HRLY|SND_DATA)	*
C*	FILNAM		CHAR*		Input data file name		*
C*									*
C* Output parameters:							*
C*	IFLNO		INTEGER		Surface file number		*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C*					 -6 = file open error		*
C**									*
C* Log:									*
C* D.W.Plummmer/NCEP	 9/96						*
C* T. Piper/SAIC	04/05	Added datatype parameter and SN_OPNF	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	LOGICAL		mrgflg
	CHARACTER       datatype*(*), filnam*(*)
	CHARACTER       parms(MMPARM)*4
C----------------------------------------------------------------------
C
C*	Open file and check for error.
C
	IF ( datatype .eq. "SND_DATA" )  THEN
	    CALL SN_OPNF ( filnam, .false., iflno, isrc, nparm, 
     +		   parms, ivert, mrgflg, ier )
	ELSE
	    CALL SF_OPNF ( filnam, .false., iflno, isrc, nparm,
     +			   parms, ier )
	END IF
C
  	iret = ier
C*
	RETURN
	END
