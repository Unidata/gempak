      SUBROUTINE UNPK_TEMP42(KFILDO,IPACK,ND5,IS4,NS4,L3264B,
     1                       LOCN,IPOS,IER,*)
C
C        MARCH    2000   LAWRENCE  GSC/TDL    ORIGINAL CODING
C        JANUARY  2001   GLAHN     COMMENTS; CHANGED TO STANDARD 
C                                  RETURN SEQUENCE; CHANGED IER = 14
C                                  TO 402  
C        FEBRUARY 2001   GLAHN     CHECKED TEMPLATE NUMBER; COMMENTS
C
C        PURPOSE
C            UNPACKS TEMPLATE 2, A DERIVED FORECAST BASED ON ALL
C            EMSEMBLE MEMBERS TEMPLATE, FROM THE PRODUCT DEFINITION 
C            SECTION OF A GRIB2 MESSAGE.  IT IS THE RESPONSIBILITY
C            OF THE CALLING ROUTINE TO UNPACK THE FIRST 9 OCTETS 
C            IN SECTION 4.
C
C        DATA SET USE
C           KFILDO - UNIT NUMBER FOR OUTPUT (PRINT) FILE. (OUTPUT)
C
C        VARIABLES
C              KFILDO = UNIT NUMBER FOR OUTPUT (PRINT) FILE. (INPUT)
C            IPACK(J) = THE ARRAY THAT HOLDS THE ACTUAL PACKED MESSAGE
C                       (J=1,ND5). (INPUT/OUTPUT)
C                 ND5 = THE SIZE OF THE ARRAY IPACK( ). (INPUT)
C              IS4(J) = THE PRODUCT DEFINITION INFORMATION FOR
C                       TEMPLATE 4.2 IS WRITTEN TO ELEMENTS 10 THROUGH
C                       36 AS IT IS UNPACKED FROM IPACK( ) (J=1,NS4).
C                       (INPUT/OUTPUT)
C                 NS4 = SIZE OF IS4( ). (INPUT)
C              L3264B = THE INTEGER WORD LENGTH IN BITS OF THE MACHINE
C                       BEING USED. VALUES OF 32 AND 64 ARE
C                       ACCOMMODATED. (INPUT)
C                LOCN = THE WORD POSITION FROM WHICH TO UNPACK THE
C                       NEXT VALUE. (INPUT/OUTPUT)
C                IPOS = THE BIT POSITION IN LOCN FROM WHICH TO START
C                       UNPACKING THE NEXT VALUE.  (INPUT/OUTPUT)
C                 IER = RETURN STATUS CODE. (OUTPUT)
C                         0 = GOOD RETURN.
C                       6-8 = ERROR CODES GENERATED BY UNPKBG. SEE THE 
C                             DOCUMENTATION IN THE UNPKBG ROUTINE.
C                       402 = IS4( ) HAS NOT BEEN DIMENSIONED LARGE
C                             ENOUGH TO CONTAIN THE ENTIRE TEMPLATE. 
C                       403 = NOT THE CORRECT TEMPLATE. 
C                   * = ALTERNATE RETURN WHEN IER NE 0. 
C
C             LOCAL VARIABLES
C             MINSIZE = THE SMALLEST ALLOWABLE DIMENSION FOR IS4( ).
C                   N = L3264B = THE INTEGER WORD LENGTH IN BITS OF
C                       THE MACHINE BEING USED. VALUES OF 32 AND
C                       64 ARE ACCOMMODATED.
C
C        NON SYSTEM SUBROUTINES CALLED
C           UNPKBG, UNPK_TEMP40
C
      PARAMETER(MINSIZE=36)
C
      DIMENSION IPACK(ND5),IS4(NS4)
C
      N=L3264B
      IER=0
C
C        CHECK TO MAKE SURE THAT THIS IS TEMPLATE 4.2. 
C
      IF(IS4(8).NE.2)THEN
D        WRITE(KFILDO,10)IS4(8)
D10      FORMAT(/' TEMPLATE ',I4,' INDICATED BY IS4(8)'/
D    1           ' IS NOT CORRECT IN UNPK_TEMP42.'/)
         IER=403
         GO TO 900
      ENDIF
C
C        CHECK THE DIMENSIONS OF IS4( ).
C
      IF(NS4.LT.MINSIZE)THEN
D        WRITE(KFILDO,20)NS4,MINSIZE
D20      FORMAT(/' IS4( ) IS CURRENTLY DIMENSIONED TO CONTAIN'/
D    1           ' NS4=',I4,' ELEMENTS. THIS ARRAY MUST BE'/
D    2           ' DIMENSIONED TO AT LEAST ',I4,' ELEMENTS'/
D    3           ' TO CONTAIN ALL OF THE DATA IN PRODUCT'/
D    4           ' DEFINITION TEMPLATE 4.2.'/)
         IER=402
         GO TO 900
      ENDIF
C
C        SINCE THIS TEMPLATE SHARES THE SAME INFORMATION
C        AS TEMPLATE 4.0, CALL THE UNPK_TEMP40 ROUTINE
C        TO RETRIEVE OCTETS 10 THROUGH 34.
C
      CALL UNPK_TEMP40(KFILDO,IPACK,ND5,IS4,NS4,L3264B,LOCN,IPOS,
     1               *900)
C
C        UNPACK THE DERIVED FORECAST CODE 
      CALL UNPKBG(KFILDO,IPACK,ND5,LOCN,IPOS,IS4(35),8,N,IER,*900)
C
C        UNPACK THE NUMBER OF FORECASTS IN ENSEMBLE
      CALL UNPKBG(KFILDO,IPACK,ND5,LOCN,IPOS,IS4(36),8,N,IER,*900)
C
C       ERROR RETURN SECTION
C
 900  IF(IER.EQ.0)RETURN 1
C
      RETURN
      END
