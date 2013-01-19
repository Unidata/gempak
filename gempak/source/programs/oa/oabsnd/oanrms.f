	SUBROUTINE OANRMS  ( ipass, parms, iglevl, ngrid, rms, isn, 
     +			     iret )
C************************************************************************
C* OANRMS								*
C*									*
C* This subroutine writes RMS values for OABSND.			*
C*									*
C* OANRMS  ( IPASS, PARMS, IGLEVL, NGRID, RMS, ISN, IRET )		*
C*									*
C* Input parameters:							*
C*	IPASS		INTEGER		Pass number			*
C*	PARMS (NGRID)	CHAR*		Parameters			*
C*	IGLEVL (*)	INTEGER		Levels				*
C*	NGRID		INTEGER		Number of output grids		*
C*	RMS   (NGRID)	REAL		RMS values			*
C*	ISN   (NGRID)	INTEGER		Number of stations reporting	*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* T. Lee/GSC		 3/99	Reformatted				*
C* T. PIper/GSC		 3/99	Corrected prolog			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	parms  ( * )
	INTEGER		isn    ( * )
	REAL		rms    ( * )
	INTEGER		iglevl ( * )
C------------------------------------------------------------------------
	iret = 0
C
C*	Write out title and pass number.
C
	WRITE  ( 6, 1000, IOSTAT = iostat )  ipass
1000	FORMAT ( / ' RMS values for pass ', I2 )
C
C*	Loop through writing out RMS values for 8 parameters at a time.
C
	ibegin = 0
	iend   = 0
	DO WHILE  ( iend .lt. ngrid )
C
C*	    Figure out which parameters to list.
C
	    ibegin = iend + 1
	    iend   = ibegin + 7
	    IF  ( iend .gt. ngrid ) iend = ngrid
C
C*	    Write out parameter list.
C
	    WRITE  ( 6, 1010, IOSTAT = iostat ) 
     +				( parms (ip), ip = ibegin, iend )

1010	    FORMAT ( 2X, ' PARAMETER: ', 8 ( 4X, A4 ) )
C
C*	    Write out levels.
C
	    WRITE  ( 6, 1015, IOSTAT = iostat )
     +				( iglevl (ip), ip = ibegin, iend )
1015	    FORMAT ( 2X, ' LEVEL:     ', 8I8 )
C
C*	    Write RMS values.
C
	    WRITE  ( 6, 1020 ) ( rms (ip), ip = ibegin, iend )
1020	    FORMAT ( 2X, ' RMS:       ', 8F8.2 )
C
C*	    Write ISN.
C
	    WRITE  ( 6, 1030 ) ( isn (ip), ip = ibegin, iend )
1030	    FORMAT ( 2X, ' #STN:      ', 8I8 )
	END DO
C*
	RETURN
	END
