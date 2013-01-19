	SUBROUTINE OACRMS  ( ipass, parms, nparms, rms, isn, iret )
C************************************************************************
C* OACRMS								*
C*									*
C* This subroutine writes RMS values for OABSFC.			*
C*									*
C* OACRMS  ( IPASS, PARMS, NPARMS, RMS, ISN, IRET )			*
C*									*
C* Input parameters:							*
C*	IPASS		INTEGER		Pass number			*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	RMS   (NPARMS)	REAL		RMS values			*
C*	ISN   (NPARMS)	INTEGER		Number of stations reporting	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* T. Lee		 3/99	Reformatted				*
C************************************************************************
	CHARACTER*(*)	parms ( * )
	INTEGER		isn ( NPARMS )
	REAL		rms ( NPARMS )
C------------------------------------------------------------------------
	iret = 0
C
C*	Write out title and pass number.
C
	WRITE  ( 6, 1000, IOSTAT = iostat )  ipass
1000	FORMAT ( / ' RMS values for pass ', I2 )
C
C*	Write out parameter list.
C
	WRITE  ( 6, 1010, IOSTAT = iostat ) ( parms (ip), ip=1,nparms )
1010	FORMAT ( 2X, ' PARAMETER: ', 8 ( 4X, A4 ) )
C
C*	Write RMS values.
C
	WRITE  ( 6, 1020 ) ( rms (ip), ip = 1, nparms )
1020	FORMAT ( 2X, ' RMS:       ', 8F8.2 )
C
C*	Write ISN.
C
	WRITE  ( 6, 1030 ) ( isn (ip), ip = 1, nparms )
1030	FORMAT ( 2X, ' #STN:      ', 8I8 )
C*
	RETURN
	END
