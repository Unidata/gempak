	INTEGER FUNCTION NVXEAS  ( xlat, ylon, zdum, xlin, xele, xdum  )
C************************************************************************
C* NVXEAS								*
C*									*
C* This subroutine calls the routine which converts earth coordinates	*
C* (lat, lon) to image corrdinates (line, element).			*
C*									*
C* INTEGER NVXEAS  ( XLAT, YLON, ZDUM, XLIN, XELE, XDUM  )		*
C*									*
C* Input parameters:							*
C*	XLAT		REAL		Latitude			*
C*	YLON		REAL		Longitude			*
C*	ZDUM		REAL		Dummy arg			*
C*									*
C* Output Parameters:							*
C*	XLIN		REAL		Image line			*
C*	XELE		REAL		Image element			*
C*	XDUM		REAL		Dummy arg			*
C*									*
C* Return value:							*
C*			 0		OK				*
C*			-1		Unable to navigate point	*
C**									*
C* Log:									*
C* J. Cowie/COMET	 1/95	Added to allow GVAR nav			*
C* J. Cowie/COMET	12/00	Added RADR projection type		*
C* S. Chiswell/Unidata	 2/02	Added MSAT, GMSX, MOLL  projection type	*
C* S. Chiswell/Unidata	 8/06	Added MSG  projection type		*
C* M. James/Unidata	10/14	Added RECT projection type		*
C* M. James/Unidata	02/18	Added ABIN fixed grid projection type	*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'SATDEF.CMN'
C
	INTEGER		GOSEAS, GVREAS, RDREAS, MSATEAS, GMSXEAS,
     +			MOLLEAS, MSGEAS, ABINEAS
C------------------------------------------------------------------------
C
C*	Call appropriate routine
C
	IF ( nvtypa .eq. 'MCGOES' ) THEN
	   iret = GOSEAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCGVAR' ) THEN
	   iret = GVREAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCRADR' ) THEN
	   iret = RDREAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCMSAT' ) THEN
	   iret = MSATEAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCMSG' ) THEN
	   iret = MSGEAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCGMSX' ) THEN
	   iret = GMSXEAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCMOLL' ) THEN
	   iret = MOLLEAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCRECT' ) THEN
	   iret = RECTEAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE IF ( nvtypa .eq. 'MCABIN' ) THEN
	   iret = ABINEAS ( xlat, ylon, zdum, xlin, xele, xdum )
	ELSE
	   iret = -1
	END IF
C
	NVXEAS = iret
C
	RETURN
	END
