	INTEGER FUNCTION NVXSAE  ( xlin, xele, xdum, xlat, ylon, zdum  )
C************************************************************************
C* NVXSAE								*
C*									*
C* This subroutine calls the routine which converts image coordinates	*
C* (line, element) to earth coordinates (lat, lon) for MCIDAS		*
C* projections								*
C*									*
C* INTEGER NVXSAE  ( XLIN, XELE, XDUM, XLAT, YLON, ZDUM  )		*
C*									*
C* Input parameters:							*
C*	XLIN		REAL		Image line			*
C*	XELE		REAL		Image element			*
C*	XDUM		REAL		Dummy arg			*
C*									*
C* Output Parameters:							*
C*	XLAT		REAL		Returned lat			*
C*	YLON		REAL		Returned lon			*
C*	ZDUM		REAL		Returned dummy			*
C*									*
C* Return value:							*
C*			 0		OK				*
C*			-1		Unable to navigate point	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 1/95	Added to allow GVAR nav			*
C* J. Cowie/COMET	12/00	Added RADR projection type		*
C* S. Chiswell/Unidata	 2/02	Added MSAT, MOLL, GMSX projection type	*
C* S. Chiswell/Unidata	 8/06	Added MSG				*
C* M. James/Unidata	10/14	Added RECT				*
C* M. James/Unidata	02/18	Added ABIN				*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'SATDEF.CMN'
C
	INTEGER		GOSSAE, GVRSAE, RDRSAE, MSATSAE, GMSXSAE,
     +			MOLLSAE, MSGSAE, ABINSAE
C------------------------------------------------------------------------
C
C*	Call appropriate routine
C
	IF ( nvtypa .eq. 'MCGOES' ) THEN
	   iret = GOSSAE ( xlin, xele, xdum, xlat, ylon, zdum  )
	ELSE IF ( nvtypa .eq. 'MCGVAR' ) THEN
	   iret = GVRSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE IF ( nvtypa .eq. 'MCRADR' ) THEN
	   iret = RDRSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE IF ( nvtypa .eq. 'MCMSAT' ) THEN
	   iret = MSATSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE IF ( nvtypa .eq. 'MCMSG' ) THEN
	   iret = MSGSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE IF ( nvtypa .eq. 'MCGMSX' ) THEN
	   iret = GMSXSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE IF ( nvtypa .eq. 'MCMOLL' ) THEN
	   iret = MOLLSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE IF ( nvtypa .eq. 'MCRECT' ) THEN
	   iret = RECTSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE IF ( nvtypa .eq. 'MCABIN' ) THEN
	   iret = ABINSAE ( xlin, xele, xdum, xlat, ylon, zdum )
	ELSE
	   iret = -1
	END IF
C
	NVXSAE = iret
C
	RETURN
	END
