	SUBROUTINE FL_APND  ( lun, iret )
C************************************************************************
C* FL_APND								*
C* 									*
C* This subroutine positions a sequential file at the end-of-file	*
C* mark so that records written after this call will be appended to	*
C* the file.								*
C* 									*
C* FL_APND  ( LUN, IRET )						*
C* 									*
C* Input parameters:							*
C*	LUN		INTEGER		Logical unit number		*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/88						*
C* S. Schotz/GSC	 8/90	Corrected read statement                *
C* B. Hebbard/NCO &	 7/19	NAWIPS-74/105: BACKSPACE after EOF pos	*
C* L. Hinson/AWC		to prevent gfortran error on subsequent *
C*				write: "Fortran runtime error		*
C*				Sequential READ or WRITE not allowed 	*
C*				after EOF marker, possibly use REWIND	*
C*				or BACKSPACE"				*
C************************************************************************
	CHARACTER	record*80
C------------------------------------------------------------------------
	iret = 0
C
C*	Read records until EOF.
C
	ier = 0
	DO WHILE  ( ier .eq. 0 )
	    READ  ( lun, 1000, IOSTAT = ier ) record
1000	    FORMAT (A)
	END DO
	BACKSPACE ( lun )
C*
	RETURN
	END
