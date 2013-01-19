	SUBROUTINE GOESLM ( mproj, np, xl, yl, xm, ym, iret )
C************************************************************************
C* GOESLM								*
C*									*
C* This subroutine uses satellite navigation to convert points from	*
C* linear intermediate coordinates to latitude/longitude.		*
C*									*
C* GOESLM  ( MPROJ, NP, XL, YL, XM, YM, IRET )				*
C*									*
C* Input parameters:							*
C*	MPROJ		INTEGER		Projection type			*
C*	NP		INTEGER		Number of points		*
C*	XL (NP)		REAL		Linear x coordinates		*
C*	YL (NP)		REAL		Linear y coordinates		*
C*									*
C* Output parameters:							*
C*	XM (NP)		REAL		Latitudes			*
C*	YM (NP)		REAL		Longitudes			*
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/85						*
C* M. desJardins/GSFC	 3/86	Changed linear intermediate coordinates	*
C* M. Gunning/NPS	12/85	Included NPGS sat nav                   *
C* M. desJardins/GSFC	 6/88	Documentation				*
C* J. Cowie/NPS         10/93   Added transform for MCI projection      *
C* S. Jacobs/EAI	 2/94	Fixed error for MCI projection		*
C* S. Jacobs/NMC	 7/94	Removed AOI projection			*
C* J. Cowie/COMET	 2/95	Added x scaling option for MCMCI nav	*
C* M. Linda/GSC		12/95	Removed NPGS sat nav			*
C************************************************************************
	INCLUDE		'ERROR.PRM'
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'SATDEF.CMN'
C*
	REAL		xl (*), yl (*), xm (*), ym (*)
        REAL            rlat, rlon, zdum
        INTEGER         istat
C
	PARAMETER	( ITYPE = 1 )
C------------------------------------------------------------------------
	iret = NORMAL
C
C*	MCIDAS satellite transform. 
C
	IF ( mproj .eq. MPMCI ) THEN
C
	    DO i = 1, np
C
C*		Expand linear coords to full image if x scaling was done.
C
		IF ( xiscal .ne. 1 )
     +			xl (i) =  xibndl + ( xl (i) - xibndl ) / xiscal
C 
		istat = NVXSAE ( yl (i), xl (i), 0., rlat, rlon, zdum )
C
		IF ( istat .ne. 0 ) THEN
		    ym (i) = RMISSD
		    xm (i) = RMISSD
		ELSE
		    ym (i) = -rlon
		    xm (i) = rlat
		END IF
	    END DO
C
	ELSE
	    iret = NIPROJ
	END IF
C*
	RETURN
	END
