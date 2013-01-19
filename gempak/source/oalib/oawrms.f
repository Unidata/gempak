	SUBROUTINE OA_WRMS  ( ipass, parms, nparms, levels, nlev,
     +			      rms, isn, iret )
C************************************************************************
C* OA_WRMS								*
C*									*
C* This subroutine writes RMS values from an objective analysis.	*
C*									*
C* OA_WRMS ( IPASS, PARMS, NPARMS, LEVELS, NLEV, RMS, ISN, IRET )	*
C*									*
C* Input parameters:							*
C*	IPASS		INTEGER		Pass number			*
C*	PARMS (NPARMS)	CHAR*		Parameters			*
C*	NPARMS		INTEGER		Number of parameters		*
C*	LEVELS  (NLEV)	INTEGER		Levels				*
C*	NLEV		INTEGER		Number of levels		*
C*	RMS		REAL		RMS values			*
C*	 (NPARMS,NLEV)							*
C*	ISN		INTEGER		Number of stations reporting	*
C*	 (NPARMS,NLEV)							*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 7/86						*
C* M. Linda/GSC		10/97	Corrected the prologue format		*
C************************************************************************
	CHARACTER*(*)	parms ( * )
	INTEGER		levels  ( * ), isn ( NPARMS, * )
	REAL		rms ( NPARMS, * )
C------------------------------------------------------------------------
	iret = 0
C
C*	Write out title and pass number.
C
	WRITE  ( 6, 1000, IOSTAT = iostat )  ipass
1000	FORMAT ( / 3X, 'RMS values for pass ', I2 )
C
C*	Write out values for each level and parameter.
C
	isp = 1
	DO WHILE  ( isp .le. nparms )
	    iep = isp + 7
	    IF  ( iep .gt. nparms ) iep = nparms
	    WRITE  ( 6, 1005 ) ( parms (ip), ip = isp, iep )
1005	    FORMAT ( / 18X, 8 ( A, 4X ) )
	    DO  il = 1, nlev
		WRITE  ( 6, 1010, IOSTAT = iostat )  levels (il),
     +				    ( rms ( ip, il ), ip = isp, iep )
1010		FORMAT ( ' LEVEL', I8, 8F8.2 )
		WRITE  ( 6, 1015, IOSTAT = iostat )
     +				    ( isn ( ip, il ), ip = isp, iep )
1015		FORMAT ('  #STN', 8X, 8I8 )
	    END DO
	    isp = iep + 1
	END DO
C*
	RETURN
	END
