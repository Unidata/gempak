	SUBROUTINE SN_MSPM  ( mandpm, sigtpm, sigwpm, troppm, maxwpm,
     +			      iret )
C************************************************************************
C* SN_MSPM								*
C* 									*
C* This subroutine returns the mandatory and significant parameter	*
C* names which are used in merging data.  The order of the parameters	*
C* is important.							*
C* 									*
C* SN_MSPM  ( MANDPM, SIGTPM, SIGWPM, TROPPM, MAXWPM, IRET )		*
C* 									*
C* Output parameters:							*
C*	MANDPM (6)	CHAR*4		Mandatory parameter names	*
C*	SIGTPM (3)	CHAR*4		Significant temp names		*
C*	SIGWPM (3)	CHAR*4		Significant wind names		*
C*	TROPPM (5)	CHAR*4		Tropopause names                *
C*	MAXWPM (3)	CHAR*4		Maximum wind names		*
C*	IRET		INTEGER		Return code			*
C* 					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 8/87						*
C* D. Kidwell/NCEP	 2/01	Added trop, max wind; corrected prologue*
C************************************************************************
	PARAMETER	( NMAND = 6, NSIGT = 3, NSIGW = 3, NTROP = 5,
     +			  NMAXW = 3 )
	CHARACTER*(*)	mandpm (*), sigtpm (*), sigwpm (*), troppm (*),
     +			maxwpm (*)
C*
	CHARACTER*4	mandpp (6), sigtpp (3), sigwpp (3), troppp (5),
     +			maxwpp (3)
	DATA		mandpp  / 'PRES', 'TEMP', 'DWPT', 
     +				  'DRCT', 'SPED', 'HGHT' /
	DATA		sigtpp  / 'PRES', 'TEMP', 'DWPT' /
	DATA		sigwpp  / 'HGHT', 'DRCT', 'SPED' /
	DATA		troppp  / 'PRES', 'TEMP', 'DWPT', 
     +				  'DRCT', 'SPED' /
	DATA		maxwpp  / 'PRES', 'DRCT', 'SPED' /
C------------------------------------------------------------------------
	iret = 0
C
C*	Return mandatory names.
C
	DO  i = 1, NMAND
	    mandpm ( i ) = mandpp ( i )
	END DO
C
C*	Return significant temperature names.
C
	DO  i = 1, NSIGT
	    sigtpm ( i ) = sigtpp ( i )
	END DO
C
C*	Return significant wind names.
C
	DO  i = 1, NSIGW
	    sigwpm ( i ) = sigwpp ( i )
	END DO
C
C*	Return tropopause names.
C
	DO  i = 1, NTROP
	    troppm ( i ) = troppp ( i )
	END DO
C
C*	Return maximum wind names.
C
	DO  i = 1, NMAXW
	    maxwpm ( i ) = maxwpp ( i )
	END DO
C*
	RETURN
	END
