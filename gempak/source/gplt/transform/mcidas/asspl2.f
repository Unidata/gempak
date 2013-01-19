C Copyright(c) 1997, Space Science and Engineering Center, UW-Madison
C Refer to "McIDAS Software Acquisition and Distribution Policies"
C in the file  mcidas/data/license.txt

      SUBROUTINE ASSPL2(N,X,Y,C)
C *** $Id: asspl2.f,v 1.6 1997/10/10 20:16:37 dglo Exp $ ***
CNP   ASSPL2
C * CALLED BY 'SPLINE'
C $ CALL ASSPL2(N, X, Y, C)  (DAS)
C $ CUBIC SPLINE GENERATOR.  SECOND DERIV. NOT CONTINUOUS.  THE DEGREES
C $   OF FREEDOM GAINED ARE USED IN AN ATTEMPT TO AVOID  OSCILLATIONS.
C $   FIT TO THE POINTS (X(I),Y(I)) I=1,...,N .
C $ INPUT:
C $   N = (I) THE NUMBER OF DATA POINTS
C $   X = (R) ARRAY OF THE ABSCISSA OF POINTS
C $   Y = (R) ARRAY OF THE ORDINATE OF POINTS
C $ OUTPUT:
C $   C = (R) ARRAY OF CUBIC SPLINE COEFFICIENTS
C $$ ASSPL2 = COMPUTATION
      DIMENSION X(N),Y(N),C(4,N)
      M=N-1
      DO 150 I=1,M
      IF(I .EQ. 1) GO TO 110
      T=(Y(I+1)-Y(I-1))/(X(I+1)-X(I-1))
      GO TO 120
  110 W=(Y(2)+Y(3))/2.0
      Z=(X(2)+X(3))/2.0
      T=(W-Y(1))/(Z-X(1))
      T=2.0*(Y(2)-Y(1))/(X(2)-X(1))-T
  120 IF(I .EQ. M) GO TO 130
      S=(Y(I+2)-Y(I))/(X(I+2)-X(I))
      GO TO 140
  130 W=(Y(N-1)+Y(N-2))/2.0
      Z=(X(N-1)+X(N-2))/2.0
      S=(Y(N)-W)/(X(N)-Z)
      S=2.0*(Y(N)-Y(N-1))/(X(N)-X(N-1))-S
  140 U=Y(I+1)
      V=Y(I)
      W=(X(I+1)+X(I))/2.0
      Z=(X(I+1)-X(I))/2.0
      ZS=Z*Z
      ZQ=Z*ZS
      WS=W*W
      WQ=W*WS
      AA=.5*(U+V)-.25*Z*(S-T)
      BA=.75*(U-V)/Z-.25*(S+T)
      CA=.25*(S-T)/Z
      DA=.25*(S+T)/ZS-.25*(U-V)/ZQ
      C(1,I)=AA-BA*W+CA*WS-DA*WQ
      C(2,I)=BA-2.0*CA*W+3.0*DA*WS
      C(3,I)=CA-3.0*DA*W
      C(4,I)=DA
  150 CONTINUE
      DO 44 J=1,4
      C(J,N)=C(J,N-1)
44    CONTINUE
      RETURN
      END
      FUNCTION FVAL(N,U,X,C)
C $ FVAL(N, U, X, C)  (WBH)
C $ EVALUATES THE CUBIC SPLINE COEFFICIENTS AT A POINT
C $ N = (I) INPUT  NUMBER OF DATA POINTS
C $ U = (R) INPUT POINT TO EVALUATE
C $ X = (R) INPUT  ARRAY OF ABSCISSA OF POINTS
C $ C = (R) OUTPUT  ARRAY OF CUBIC SPINE COEFFICIENT
C $$ FVAL = COMPUTATION
      REAL U,X(N),C(4,N)
      DATA I /1/
      IF (I .GE. N) I=1
      IF (U .LT. X(I))GO TO 10
      IF (U .LE. X(I+1))GO TO 30
   10 I= 1
      J= N + 1
   20 K= (I+J)/2
      IF (U .LT. X(K)) J=K
      IF (U .GE. X(K)) I=K
      IF (J .GT. I+1)GO TO 20
   30 CONTINUE
      FVAL=C(1,I)+U*(C(2,I)+U*(C(3,I)+U*C(4,I)))
      RETURN
      END
