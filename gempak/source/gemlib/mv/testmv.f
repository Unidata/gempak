	PROGRAM TESTMV
C************************************************************************
C* TESTMV								*
C*									*
C* This program tests the MV_ subroutine library.			*
C*									*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 2/91						*
C* K. Brill/NMC		03/92	Made this easier to use			*
C* K. Brill/NMC		09/92	Remove writing of reals			*
C* S. Jacobs/EAI	 9/92	Added input and output dump		*
C* S. Jacobs/NCEP	 7/96	Added MV_BTOI and MV_ITOB		*
C************************************************************************
	BYTE		b1 (128), b2 (128)
	INTEGER		ib1 (128), i1 (32), i2 (32)
	REAL		r1 (32), r2 (32)
C*
	LOGICAL		negflg
C*
	EQUIVALENCE	( b1, i1, r1 )
	EQUIVALENCE	( b2, i2, r2 )
C-----------------------------------------------------------------------
	CALL IN_BDTA ( iret )
        iostat = 0
        DO WHILE ( iostat .eq. 0 )
	  WRITE (6, 10)
10	  FORMAT ( 
     +  '  1 = MV_SWP4    2 = MV_SWP2    3 = MV_VE32    4 = MV_EV32'/
     +  '  5 = MV_IE32    6 = MV_EI32    7 = MV_VI32    8 = MV_IV32'/
     +  '  9 = MV_SW42   10 = MV_BTOI   11 = MV_ITOB'/
     +  ' 21 = ENTER BYTE (4)   22 = ENTER INTEGER    23 = ENTER REAL'/
     +  ' 24 = DUMP INPUT       25 = DUMP OUTPUT' )
	  CALL TM_INT ( 'Select a subroutine number', .false.,
     +			.false., 1, numsub, n, ier )
	  IF ( ier .eq. 2 ) THEN
           iostat = -1
           numsub = -1
	  END IF
C------------------------------------------------------------------------
	  IF (numsub .eq. 1) THEN
	    iret = MV_SWP4  ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 2) THEN
	    iret = MV_SWP2 ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 3) THEN
	    iret = MV_VE32  ( num, i1, i2 ) 
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 4) THEN
	    iret = MV_EV32  ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 5) THEN
	    iret = MV_IE32  ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 6) THEN
	    iret = MV_EI32  ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 7) THEN
	    iret = MV_VI32  ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 8) THEN
	    iret = MV_IV32  ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 9) THEN
	    iret = MV_SW42  ( num, i1, i2 )
	    CALL OUTPUT  ( b1, i1, b2, i2, num, iret, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 10) THEN
	    WRITE (6,*) 'Enter the start byte for the integer:'
	    READ  (5,*) istart
	    istart = istart - 1
	    WRITE (6,*) 'Enter the number of bytes in the integer:'
	    READ  (5,*) nbytes
	    WRITE (6,*) 'Enter the negative value flag (t/f):'
	    READ  (5,*) negflg
	    CALL MV_BTOI  ( i1, istart, nbytes, negflg, ivalue, iret )
	    WRITE (6,*) 'MV_BTOI: iret   = ', iret
	    WRITE (6,*) 'MV_BTOI: ivalue = ', ivalue
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 11) THEN
	    WRITE (6,*) 'Enter the integer to pack:'
	    READ  (5,*) ivalue
	    WRITE (6,*) 'Enter the start byte for the integer:'
	    READ  (5,*) istart
	    istart = istart - 1
	    WRITE (6,*) 'Enter the number of bytes in the integer:'
	    READ  (5,*) nbytes
	    CALL MV_ITOB  ( ivalue, istart, nbytes, i1, iret )
	    WRITE (6,*) 'MV_ITOB: iret   = ', iret
	    CALL OUTPT2 ( b1, i1, r1, 32, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 21)  THEN
	    WRITE  (6,*) 'ENTER # OF 4-BYTE WORDS (32 MAX ):'
	    READ   (5,*)  num
	    istrt = 1
	    istop = 4
	    IF ( num .gt. 32 )  num = 1
	    itot = num * 4
	    WRITE  (6,*) 'Enter 4 BYTE values for EACH WORD ( ',
     +			 itot, ' values ): '
	    READ   (5,*) ( IB1(k), k = 1, itot )
	    DO ij = 1, itot
                 b1 (ij) = ib1 (ij)
	    END DO
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 22)  THEN
	    WRITE  (6,*) 'ENTER NUMBER OF INTEGERS (32 MAX): '
	    READ   (5,*)  num
	    IF  ( ( num .gt. 32 ) .or. ( num .lt. 0 ) )  THEN
		i1 (1) = num
		num = 1
	      ELSE
		WRITE  (6,*) 'ENTER all ', num, ' INTEGERS:'
		READ   (5,*)  ( I1 (i), i=1, num )
	    END IF
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 23)  THEN
	    WRITE  (6,*) 'ENTER NUMBER OF REALS (32 MAX): '
	    READ   (5,*)  num
	    IF  ( ( num .gt. 32 ) .or. ( num .lt. 0 ) )  THEN
		i1 (1) = num
		num = 1
	      ELSE
		WRITE  (6,*) 'ENTER all ', num, ' REALS:'
		READ   (5,*)  ( R1 (i), i=1, num )
	    END IF
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 24)  THEN
	      WRITE ( 6, * ) 'Input values:'
	      CALL OUTPT2 ( b1, i1, r1, num, ier )
C------------------------------------------------------------------------
	  ELSE IF (numsub .eq. 25)  THEN
	      WRITE ( 6, * ) 'Output values:'
	      CALL OUTPT2 ( b2, i2, r2, num, ier )
C------------------------------------------------------------------------
	  END IF
	END DO
C*
	END
C========================================================================
	SUBROUTINE OUTPUT ( b1, i1, b2, i2, nout, mverr, iret )
C************************************************************************
C* OUTPUT								*
C*									*
C* This subroutine does printouts for TESTMV.				*
C*									*
C* Input paramters:							*
C* B1		BYTE		Byte values that were input		*
C* I1		INTEGER		Integer values that were input		*
C* B2		BYTE		Byte values that were output		*
C* I2		INTEGER		Integer values that were output		*
C* NOUT		INTEGER		Number of sets of values to print	*
C* MVERR	INTEGER		Return code from the MV routine		*
C*									*
C* Output parameters:							*
C* IRET		INTEGER		Return code				*
C*									*
C**									*
C* Log:									*
C* K. Brill/NMC		03/92						*
C* K. Brill/NMC		09/92	Takeout writing of reals		*
C* S. Jacobs/EAI	 9/92	Added input and output dump		*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	BYTE		b1 (*), b2 (*)
	INTEGER		i1 (*), i2 (*)
C------------------------------------------------------------------------
	iret = 0
	WRITE (6,6) mverr
6	FORMAT ( /, '  IRET = ', I5 )
	WRITE (6,7)
7	FORMAT ( 18X, 'INPUT', 36X, 'OUTPUT' )
	WRITE (6,8)
8	FORMAT ( 2 ( 9X, 'BYTES', 13X, 'INTEGER', 7X ) )
	istrt = 1
	istop = 4
	DO iout = 1, nout
	    WRITE  (6,9) (b1 (ij), ij=istrt,istop ), i1 (iout),
     +             	 (b2 (ij), ij=istrt,istop ), i2 (iout)
9	    FORMAT ( 2 ( 4(1X,I4), 4X, I11, 6X ) )
	    istrt = istrt + 4 
   	    istop = istop + 4
	END DO
	PRINT *
	RETURN
	END
C========================================================================
	SUBROUTINE OUTPT2 ( b, i, r, nout, iret )
C************************************************************************
C* OUTPUT								*
C*									*
C* This subroutine does printouts for TESTMV.				*
C*									*
C* Input paramters:							*
C* B		BYTE		Byte values				*
C* I		INTEGER		Integer values				*
C* R		REAL		Real values				*
C* NOUT		INTEGER		Number of sets of values to print	*
C*									*
C* Output parameters:							*
C* IRET		INTEGER		Return code				*
C*									*
C**									*
C* Log:									*
C* K. Brill/NMC		03/92						*
C* K. Brill/NMC		09/92	Takeout writing of reals		*
C* S. Jacobs/EAI	 9/92	Added input and output dump		*
C* M. Linda/GSC		 9/97	Changed a key word in the prologue	*
C************************************************************************
	BYTE		b (*)
	INTEGER		i (*)
	REAL		r (*)
C------------------------------------------------------------------------
	iret = 0
	WRITE (6,11)
11	FORMAT ( 9X, 'BYTES', 13X, 'INTEGER', 10X, 'REAL' )
	istrt = 1
	istop = 4
	DO iout = 1, nout
	    WRITE  (6,12) (b (ij), ij=istrt,istop ), i (iout), r (iout)
12	    FORMAT ( 4(1X,I4), 4X, I11, 4X, 1PE12.5 )
	    istrt = istrt + 4 
   	    istop = istop + 4
	END DO
	PRINT *
	RETURN
	END
