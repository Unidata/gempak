	SUBROUTINE OABINP  ( line, device, region, garea, proj, gdfile,
     +			     iret )
C************************************************************************
C* OABINP								*
C*									*
C* This subroutine gets the input parameters for OABOX.			*
C*									*
C* OABINP  ( LINE, DEVICE, REGION, GAREA, PROJ, GDFILE, IRET )		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	11/88	GEMPAK 4.1				*
C* M. desJardins/GSFC	 9/90	BOX --> LINE; ANLAREA --> REGION	*
C************************************************************************
	CHARACTER*(*)	line, device, region, garea, proj, gdfile
C------------------------------------------------------------------------
	CALL IP_STR  ( 'LINE',    line,   ier1 )
	CALL IP_STR  ( 'DEVICE',  device, ier2 )
	CALL IP_STR  ( 'REGION',  region, ier3 )
	CALL IP_STR  ( 'GAREA',   garea,  ier4 )
	CALL IP_STR  ( 'PROJ',    proj,   ier5 )
	CALL IP_STR  ( 'GDFILE',  gdfile, ier6 )
	iret = ier1 + ier2 + ier3 + ier4 + ier5 + ier6
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
