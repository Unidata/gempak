	SUBROUTINE GPCINP  ( device, colors, iret )
C************************************************************************
C* GPCINP								*
C*									*
C* This subroutine gets the input parameters for GPCOLOR.		*
C*									*
C* GPCINP  ( DEVICE, COLORS, IRET )					*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/88						*
C* M. desJardins/GSFC	 1/92	CTYPE-->TYPE				*
C* K. Brill/NMC		01/92	Remove CCOMP and TYPE			*
C************************************************************************
	CHARACTER*(*)	device, colors
C------------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1 )
	CALL IP_STR  ( 'COLORS', colors, ier2 )
C
	ier = ier1 + ier2 
	IF  ( ier .ne. 0 )  iret = -2
C*
	RETURN
	END
