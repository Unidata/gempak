	SUBROUTINE GPFINP ( device, faxfil, iret )
C************************************************************************
C* GPFINP								*
C*									*
C* This subroutine gets the input for GPFAX.				*
C*									*
C* GPFINP  ( DEVICE, FAXFIL, IRET )    			                *
C*                                                                      *
C* Input parameters:                                                    *
C*      DEVICE          CHAR*           Device name	                *
C*      FAXFIL          CHAR*           File name                       *
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                       -2 = error 	         	*
C**									*
C* Log:									*
C* R.Tian/SAIC		04/02	Modified from GPMINP                    *
C************************************************************************
	CHARACTER*(*)	device, faxfil
C-----------------------------------------------------------------------
	CALL IP_STR  ( 'DEVICE', device, ier1 )
	CALL IP_STR  ( 'FAXFIL', faxfil, ier2 )
C
	iret =  ier1 + ier2
C
	IF  ( iret .ne. 0 )  iret = -2
C*
	RETURN
	END
