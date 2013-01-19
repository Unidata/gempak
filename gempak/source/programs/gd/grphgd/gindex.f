	SUBROUTINE GINDEX ( i, j, ij, ndir, ffi1, ffj1, lin1, 
     +			    ffi2, ffj2, lin2, iret )
C************************************************************************
C* GINDEX                                                               *
C*                                                                      *
C* This subroutine returns intersection information given an (i,j)	*
C* location and a nominal direction.  If no intersection in direction	*
C* NDIR, locations will be set to zero with values set to RMISSD.	*
C* Note: On a compass, NDIR is as follows:				*
C*	1 - NE		2 - E		3 - SE		4 - S		*
C*	5 - SW		6 - W		7 - NW		8 - N		*
C*                                                                      *
C* GINDEX ( I, J, IJ, NDIR, FFI1, FFJ1, LIN1, FFI2, FFJ2, LIN2, IRET )	*
C*                                                                      *
C* Input parameters:                                                    *
C*      I		INTEGER		I location on grid		*
C*      J		INTEGER		J location on grid		*
C*      IJ		INTEGER		Index pointer into int arrays	*
C*      NDIR		INTEGER		Direction (1 thru 8)		*
C*                                                                      *
C* Output parameters:                                                   *
C*      FFI1		REAL		I location of first int		*
C*      FFJ1		REAL		J location of first int		*
C*      LIN1		INTEGER		Line number at first int	*
C*      FFI2		REAL		I location of second int	*
C*      FFJ2		REAL		J location of second int	*
C*      LIN2		INTEGER		Line number at second int	*
C*      IRET            INTEGER         Return code                     *
C**                                                                     *
C* Log:                                                                 *
C* D.W.Plummer/NCEP      9/98                                           *
C* D.W.Plummer/NCEP      7/03	Chgs for intersection efficiency	* 
C* D.W.Plummer/NCEP      9/03	Return line number instead of line val	*
C************************************************************************
C
        INCLUDE         'GEMPRM.PRM'
        INCLUDE         'grphgd.cmn'
C
	iret = 0
C
	ffi1 = RMISSD
	ffj1 = RMISSD
	lin1 = IMISSD
	ffi2 = RMISSD
	ffj2 = RMISSD
	lin2 = IMISSD
C
	IF ( ndir .eq. 1 )  THEN
C
		IF ( intptrs(ij,3,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,3,STPTR)+intptrs(ij,3,NINTS)-1, 
     &		        intptrs(ij,3,STPTR), -1
C
			IF ( i .lt. intsct(n,1) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	ELSE IF ( ndir .eq. 2 )  THEN
C
		IF ( intptrs(ij,2,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,2,STPTR)+intptrs(ij,2,NINTS)-1,
     &                  intptrs(ij,2,STPTR), -1
C
			IF ( i .lt. intsct(n,1) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	ELSE IF ( ndir .eq. 3 )  THEN
C
		IF ( intptrs(ij,4,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,4,STPTR)+intptrs(ij,4,NINTS)-1,
     &                  intptrs(ij,4,STPTR), -1
C
			IF ( i .lt. intsct(n,1) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	ELSE IF ( ndir .eq. 4 )  THEN
C
		IF ( intptrs(ij,1,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,1,STPTR),
     &                  intptrs(ij,1,STPTR)+intptrs(ij,1,NINTS)-1
C
			IF ( j .gt. intsct(n,2) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	ELSE IF ( ndir .eq. 5 )  THEN
C
		IF ( intptrs(ij,3,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,3,STPTR),
     &                  intptrs(ij,3,STPTR)+intptrs(ij,3,NINTS)-1
C
			IF ( i .gt. intsct(n,1) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	ELSE IF ( ndir .eq. 6 )  THEN
C
		IF ( intptrs(ij,2,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,2,STPTR),
     &                  intptrs(ij,2,STPTR)+intptrs(ij,2,NINTS)-1
C
			IF ( i .gt. intsct(n,1) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	ELSE IF ( ndir .eq. 7 )  THEN
C
		IF ( intptrs(ij,4,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,4,STPTR),
     &                  intptrs(ij,4,STPTR)+intptrs(ij,4,NINTS)-1
C
			IF ( i .gt. intsct(n,INT_X) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	ELSE IF ( ndir .eq. 8 )  THEN
C
		IF ( intptrs(ij,1,NINTS) .eq. 0 )  RETURN
C
		DO  n = intptrs(ij,1,STPTR)+intptrs(ij,1,NINTS)-1,
     &                  intptrs(ij,1,STPTR), -1
C
			IF ( j .lt. intsct(n,INT_Y) )  THEN
C
				ffi2 = ffi1
				ffj2 = ffj1
				lin2 = lin1
				ffi1 = intsct(n,INT_X)
				ffj1 = intsct(n,INT_Y)
				lin1 = intinfo(n,INT_LINE)
C
			END IF
C
		END DO
C
	END IF
C
	RETURN
	END
