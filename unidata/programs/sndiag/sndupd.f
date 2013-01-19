	SUBROUTINE SNDUPD  ( snfile, snoutf,   area, dattim, timstn,
     +                       wavlen, wavspd,  storm, crdrot, tropht,
     +			     trpint, cldhgt, mxdpth, squall,   delz,
     +			     filtyp, spline, output,   iret )
C************************************************************************
C* SNDUPD								*
C*									*
C* This subroutine updates variables for SNDIAG.			*
C*									*
C* SNDUPD  ( SNFILE, SNOUTF, AREA, DATTIM, TIMSTN, WAVLEN, WAVSPD,	*
C*	     STORM, CRDROT, TROPHT, TRPINT, CLDHGT, MXDPTH, SQUALL,	*
C*	     DELZ, FILTYP, SPLINE, OUTPUT, IRET )			*
C*									*
C**									*
C* Log:									*
C* S. Jacobs/SSAI	 4/92						*
C* J. Whistler/SSAI	 4/93						*
C************************************************************************
	CHARACTER*(*)	snfile, snoutf,   area, dattim, timstn,
     +			wavlen, wavspd,  storm, crdrot, tropht, trpint,
     +			cldhgt, mxdpth, squall,   delz, filtyp, output
	LOGICAL		spline
C------------------------------------------------------------------------
C*	Update variables.
C
C	CALL IP_USTR  ( 'SNFILE', snfile, ier )
C	CALL IP_USTR  ( 'SNOUTF', snoutf, ier )
C	CALL IP_USTR  ( 'AREA',   area,   ier )
C	CALL IP_USTR  ( 'DATTIM', dattim, ier )
C	CALL IP_USTR  ( 'TIMSTN', timstn, ier )
C	CALL IP_USTR  ( 'OUTPUT', output, ier )
C	CALL IP_USTR  ( 'WAVLEN', wavlen, ier )
C	CALL IP_USTR  ( 'WAVSPD', wavspd, ier )
C	CALL IP_USTR  ( 'SYSTEM', storm,  ier )
C	CALL IP_USTR  ( 'ROTATE', crdrot, ier )
C	CALL IP_USTR  ( 'TROPHT', tropht, ier )
C	CALL IP_USTR  ( 'TRPINT', trpint, ier )
C	CALL IP_USTR  ( 'CBTOP',  cldhgt, ier )
C	CALL IP_USTR  ( 'MXDPTH', mxdpth, ier )
C	CALL IP_USTR  ( 'SQUALL', squall, ier )
C	CALL IP_USTR  ( 'DELZ',   delz,   ier )
C	CALL IP_USTR  ( 'FILTYP', filtyp, ier )
C	CALL IP_ULOG  ( 'SPLINE', spline, ier )
C
	iret = 0
C*
	RETURN
	END
