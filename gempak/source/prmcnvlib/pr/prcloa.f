	FUNCTION PR_CLOA  ( clcx )
C************************************************************************
C* PR_CLOA								*
C*									*
C* This function computes XCLO from CLCx.  The output is the 		*
C* fractional cloud coverage; x may be L, M, H or T.  The cloud		*
C* coverage values and the corresponding fractional equivalents are:	*
C*									*
C*         CLCx      CLOUD TYPE              XCLO			*
C*									*
C*          0        missing                 0.00			*
C*          1        clear                   0.00			*
C*          2        scattered               0.40			*
C*          3        broken                  0.75			*
C*          4        overcast                1.00			*
C*          5        obscured                1.00			*
C*          6        thin scatterd           0.25			*
C*          7        thin broken             0.60			*
C*          8        thin overcast           0.90			*
C*          9        partially obscured      0.00			*
C*									*
C* REAL PR_CLOA  ( CLCX )						*
C*									*
C* Input parameters:							*
C*	CLCX		REAL		Numeric cloud coverage		*
C*									*
C* Output parameters:							*
C*	PR_CLOA		REAL		Fractional cloud coverage	*
C**									*
C* Log:									*
C* I. Graffman/RDS	11/84						*
C* M. desJardins/GSFC	10/87	Rewritten				*
C* G. Huffman/GSC       7/88   	Documentation and error check		*
C* K. Brill/NMC		11/91	Changed cloud fractions			*
C************************************************************************
        INCLUDE		'GEMPRM.PRM'
C*
	REAL		cloud ( 0: 9 ) 
C*
	DATA		cloud / 0., 0., .4, .75, 1., 1., .25,
     +				.6, .9, 0. /
C------------------------------------------------------------------------
	icld = clcx
C
C*	If range is invalid, return no clouds.
C
	IF  ( ( icld .lt. 0 ) .or. ( icld .gt. 9 ) )  icld = 0
C
C*	Get fraction from table.
C
	PR_CLOA = cloud ( icld )
C
	RETURN
	END
