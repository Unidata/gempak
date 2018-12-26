	INTEGER FUNCTION NVXINI  ( ifunc, nav )
C************************************************************************
C* NVXINI								*
C*									*
C* This subroutine initializes the navigation for either the MCIDAS	*
C* GOES or GVAR nav blocks						*
C*									*
C* INTEGER NVXINI  ( IFUNC, NAV )					*
C*									*
C* Input parameters:							*
C*	IFUNC		INTEGER		Input to nav init routine	*
C*	NAV		INTEGER(*)	Nav block			*
C*									*
C* Return value:							*
C*			 0		OK				*
C*			-1		Error initializing nav		*
C**									*
C* Log:									*
C* J. Cowie/COMET	 1/95	Added to allow GVAR nav			*
C* J. Cowie/COMET	12/00	Added RADR nav				*
C* S. Chiswell/Unidata	 2/02	Added MSAT, GMSX, MOLL nav		*
C* S. Chiswell/Unidata	 8/06	Added MSG				*
C* T. Piper/SAIC	03/07	Changed MSGINI to MSG_INI		*
C* M. James/Unidata	10/14	Added RECT				*
C* M. James/Unidata	 2/18	Added ABIN				*
C* S. Guan/NCEP          2/18   Added GOE4                              *
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'SATDEF.CMN'
C*
	INTEGER		nav(640), GOSINI, GVRINI, RDRINI, MSATINI,
     +			GMSXINI, MOLLINI, ABININI, GOSINI4
C------------------------------------------------------------------------
C
C*	Call appropriate routine
C
	IF ( nvtypa .eq. 'MCGOES' ) THEN
	   iret = GOSINI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCGOE4' ) THEN
	   iret = GOSINI4 ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCGVAR' ) THEN
	   iret = GVRINI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCABIN' ) THEN
	   iret = ABININI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCRADR' ) THEN
	   iret = RDRINI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCMSAT' ) THEN
	   iret = MSATINI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCMSG' ) THEN
	   iret = MSG_INI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCGMSX' ) THEN
	   iret = GMSXINI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCMOLL' ) THEN
	   iret = MOLLINI ( ifunc, nav )
	ELSE IF ( nvtypa .eq. 'MCRECT' ) THEN
	   iret = RECTINI ( ifunc, nav )
	ELSE
	    iret = -1
	END IF
C
	NVXINI = iret
C
	RETURN
	END
