	SUBROUTINE SHN_BFSQ  ( r8ary, nr8ary, bfrstr, iret )
C************************************************************************
C* SHN_BFSQ								*
C*									*
C* This subroutine stores an array of data values into the current	*
C* SHEF BUFR report.  The values are stored as a sequence corresponding	*
C* to string BFRSTR.							*
C*									*
C* SHN_BFSQ  ( R8ARY, NR8ARY, BFRSTR, IRET )				*
C*									*
C* Input parameters:							*
C*	R8ARY(NR8ARY)	REAL*8		Array of data values		*
C*	NR8ARY		INTEGER		Number of data values to store	*
C*					from within R8ARY 		*
C*	BFRSTR		CHAR*		Mnemonic string associated	*
C*					with values in R8ARY		*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code:			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* J. Ator/NCEP		04/05						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'BRIDGE.PRM'
	INCLUDE		'shncmn.cmn'
C
	CHARACTER*(*)	bfrstr
C*
	REAL*8		r8ary (*)
C*
	CHARACTER	bfrsqstr*10
C*-----------------------------------------------------------------------
	iret = 0
C
	bfrsqstr = '<SH' // bfrstr // 'SQ>'
C
C*	Have data values already been stored into the report for this
C*	particular sequence?
C
	CALL UT_BFRI  ( iunbfo * (-1), bfrsqstr, reps, ierbri )
	IF  ( INT ( reps ) .eq. 0 )  THEN
C
C*	    NO, so allocate space to store them within the report.
C
	    CALL DRFINI  ( iunbfo, 1, 1, bfrsqstr )
	END IF
C
C*	Store the sequence of data values into the report.  If previous
C*	values were stored for this sequence, they will	be overwritten.
C
	IF  ( nr8ary .eq. 3 )  THEN
	   ipt = 2
	ELSE
	   ipt = 1
	END IF
	CALL UFBSEQ  ( iunbfo, r8ary ( ipt ), nr8ary, 1, ierusq,
     +		       bfrsqstr(2:9) )
C*
	RETURN
	END
