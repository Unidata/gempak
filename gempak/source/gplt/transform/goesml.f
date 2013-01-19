	SUBROUTINE GOESML ( mproj, np, xm, ym, xl, yl, iret )
C************************************************************************
C* GOESML								*
C*									*
C* This subroutine uses satellite navigation to convert points from	*
C* latitude/longitude to linear intermediate coordinates.		*
C*									*
C* GOESML  ( MPROJ, NP, XM, YM, XL, YL, IRET )				*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Projection type			*
C*	NP		INTEGER		Number of points		*
C*	XM (NP)		REAL		Latitudes			*
C*	YM (NP)		REAL		Longitudes			*
C*									*
C* Output parameters:							*
C*	XL (NP)		REAL		Linear x coordinates		*
C*	YL (NP)		REAL		Linear y coordinates		*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/85						*
C* M. desJardins/GSFC	 3/86	Changed linear coordinates		*
C* M. Gunning/NPS	12/85	Mods to include NPGS sat nav		*
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Cowie/NPS         10/93   Added transform for MCI projection      *
C* S. Jacobs/NMC	 7/94	Removed AOI projection			*
C* J. Cowie/COMET	 2/95	Added x scaling option for MCMCI nav	*
C* M. Linda/GSC		12/95	Removed NPGS sat nav			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'SATDEF.CMN'
C*
	REAL		xl (*), yl (*), xm (*), ym (*)
	REAL		line, elem, zdum
	INTEGER		istat
C
	INTEGER*2	itype
	PARAMETER	( ITYPE = 2 )
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	MCIDAS satellite navigation.
C
	IF ( mproj .eq. MPMCI ) THEN
C
	    DO i = 1, np
C
		istat = NVXEAS ( xm (i), -ym (i), 0., line, elem, zdum )
C
		IF ( istat .ne. 0 ) THEN
		    yl (i) = RMISSD
		    xl (i) = RMISSD
		ELSE
C
C*		    If image unevenly scaled (sampled), apply scaling to x.
C
		    IF ( xiscal .ne. 1 ) THEN
			xl (i) = xibndl + (elem - xibndl) * xiscal
		    ELSE
			xl( i) = elem
		    END IF
C
		    yl (i) = line
C
		END IF
	    END DO
C
	ELSE
	    iret = NIPROJ
	END IF
C*
	RETURN
	END
