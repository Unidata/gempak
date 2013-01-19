	SUBROUTINE GPBINP  ( line, region, device, iret )
C************************************************************************
C* GPBINP								*
C*									*
C* This subroutine gets the color and line type numbers.		*
C*									*
C* GPBINP  ( LINE, REGION, DEVICE, IRET )				*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84  	Original source				*
C* I. Graffman/RDS	 7/88	GEMPAK4 clean up			*
C* M. desJardins/GSFC	 9/90	GEMPAK5					*
C************************************************************************
	CHARACTER*(*)	region, device, line
C------------------------------------------------------------------------
	iret = 0
	CALL IP_STR  ( 'LINE',   line,   ier1 )
	CALL IP_STR  ( 'REGION', region, ier2 )
	CALL IP_STR  ( 'DEVICE', device, ier3 )
C*
	iret = ier1 + ier2 + ier3
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
