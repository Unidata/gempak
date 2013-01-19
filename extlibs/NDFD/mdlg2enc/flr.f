      REAL FUNCTION FLR(VALUE)
C
      IF(VALUE.GE.0)THEN
         FLR=AINT(VALUE)
      ELSE
C
         IF(AMOD(VALUE,1.0).EQ.0)THEN
            FLR=VALUE
         ELSE
            FLR=AINT(VALUE)-1 
         ENDIF
C
      ENDIF
C
      RETURN
      END
