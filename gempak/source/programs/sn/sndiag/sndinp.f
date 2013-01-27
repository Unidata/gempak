	SUBROUTINE SNDINP  ( snfile, snoutf,   area, dattim, timstn,
     +                       wavlen, wavspd,  storm, crdrot, tropht,
     +			     trpint, cldhgt, mxdpth, squall,   delz,
     +			     filtyp, spline, output,   iret )
C************************************************************************
C* SNDINP								*
C*									*
C* This subroutine gets the input variables for SNDIAG.			*
C*									*
C* SNDINP  ( SNFILE, SNOUTF, AREA, DATTIM, TIMSTN, WAVLEN, WAVSPD,	*
C*	     STORM, CRDROT, TROPHT, TRPINT, CLDHGT, MXDPTH, SQUALL,	*
C*	     DELZ, FILTYP, SPLINE, OUTPUT, IRET )			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93		Cleaned up			*
C************************************************************************
	CHARACTER*(*)	snfile, snoutf,   area, dattim, timstn,
     +			wavlen, wavspd,  storm, crdrot, tropht, trpint,
     +			cldhgt, mxdpth, squall,   delz, filtyp, output
	LOGICAL		spline
C------------------------------------------------------------------------
C*	Get the input variables.
C
	CALL IP_STR  ( 'SNFILE', snfile, ier1  )
	CALL IP_STR  ( 'SNOUTF', snoutf, ier2  )
	CALL IP_STR  ( 'AREA',   area,   ier3  )
	CALL IP_STR  ( 'DATTIM', dattim, ier4  )
	CALL IP_STR  ( 'TIMSTN', timstn, ier5  )
	CALL IP_STR  ( 'OUTPUT', output, ier6  )
	CALL IP_STR  ( 'WAVLEN', wavlen, ier7  )
	CALL IP_STR  ( 'WAVSPD', wavspd, ier8  )
	CALL IP_STR  ( 'SYSTEM', storm,  ier9  )
	CALL IP_STR  ( 'ROTATE', crdrot, ier10 )
	CALL IP_STR  ( 'TROPHT', tropht, ier11 )
	CALL IP_STR  ( 'TRPINT', trpint, ier12 )
	CALL IP_STR  ( 'CBTOP',  cldhgt, ier13 )
	CALL IP_STR  ( 'MXDPTH', mxdpth, ier14 )
	CALL IP_STR  ( 'SQUALL', squall, ier15 )
	CALL IP_STR  ( 'DELZ',   delz,   ier16 )
	CALL IP_STR  ( 'FILTYP', filtyp, ier17 )
	CALL IP_LOG  ( 'SPLINE', spline, ier18 )
C
	iret =   ier1 +  ier2 +  ier3 +  ier4 +  ier5 +  ier6 +  ier7 +
     +		 ier8 +  ier9 + ier10 + ier11 + ier12 + ier13 + ier14 +
     +		ier15 + ier16 + ier17 + ier18
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
