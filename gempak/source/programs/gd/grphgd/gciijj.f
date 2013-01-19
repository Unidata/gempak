	SUBROUTINE GCIIJJ ( ix, jy, ky, ndir, ij, iret )
C************************************************************************
C* GCIIJJ                                                               *
C*                                                                      *
C* This subroutine calculates the index into the directional		*
C* intersection array.							*
C*                                                                      *
C* GCIIJJ ( IX, JY, KY, NDIR, IJ, IRET )				*
C*                                                                      *
C* Input parameters:                                                    *
C*      IX              INTEGER         I (column) grid location	*
C*      JY              INTEGER         J (row) grid location		*
C*      KY              INTEGER         Number of grid rows		*
C*      NDIR            INTEGER         Direction (1 thru 8)		*
C*                                                                      *
C* Output parameters:                                                   *
C*      IJ		INTEGER		Index into directional array	*
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98                                           *
C************************************************************************
C
	IF ( ndir .eq. 1 .or. ndir .eq. 5 )  THEN
		ij = ky - jy + ix
	ELSE IF ( ndir .eq. 2 .or. ndir .eq. 6 )  THEN
		ij = jy
	ELSE IF ( ndir .eq. 3 .or. ndir .eq. 7 )  THEN
		ij = ix + jy - 1
	ELSE IF ( ndir .eq. 4 .or. ndir .eq. 8 )  THEN
		ij = ix
	END IF
C
	iret = 0
C
	RETURN
	END 
