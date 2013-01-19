	SUBROUTINE TF_GVTM ( carstr, lens, ivbday, iveday, 
     +                       ivbhr, ivehr, iret )
C************************************************************************
C* TF_GVTM								*
C*									*
C* This routine returns valid day and vaild hour from input report time.*
C* The carstr may contains the report time in YYGGgg or YYGG/yygg.  	*
C*									*
C* TF_GVTM ( CARSTR, LENS, IVBDAY, IVEDAY, IVBHR, IVEHR, IRET )		*
C*									*
C* Input parameters:							*
C*	CARSTR    	CHAR*  		Report time for a bulletin 	*
C*	LENS  		INTEGER		The length of carstr    	*
C*									*
C* Output parameters:							*
C*	IVBDAY    	INTEGER		The begining valid day          *
C*	IVEDAY    	INTEGER		The ending valid day            *
C*	IVBHR     	INTEGER		The valid begining hours        *
C*	IVEHR     	INTEGER		The valid ending hours          *
C*	IRET      	INTEGER		=-1, not a valid time           *
C*									*
C**									*
C* Log:									*
C* L.LIN/NCEP		4/08	Created                                 *
C************************************************************************
C*
	CHARACTER*(*)   carstr
C*
        iret = 0
C
        IF ( lens .eq. 6 ) THEN
C
C*      Valid time format is in YYGGgg with 6 charaters
C
           CALL ST_INTG ( carstr ( :2 ), ivbday, ier )
           IF ( ivbday .gt. 31 ) iret = -1
C
           CALL ST_INTG ( carstr ( 3:4 ), ivbhr, ier )
           IF ( ivbhr .lt. 24 ) THEN
             ELSE IF ( ivbhr .eq. 24 ) THEN
               ivbhr = 0
             ELSE
               iret = -1
           END IF
C
           CALL ST_INTG ( carstr ( 5:6 ), ivehr, ier )
           IF ( ( ( ivehr .gt.  0 ) .and. ( ivehr .le. 24 ) ) .or.
     +          ( ( ivehr .gt. 50 ) .and. ( ivehr .le. 74 ) ) ) THEN
             ELSE IF ( ( ivehr .eq. 0 ) .or. ( ivehr .eq. 50 ) ) THEN
               ivehr = ivehr + 24
             ELSE
               iret = -1
           END IF
        ELSE IF ( lens .eq. 9 ) THEN
C
C*      Valid time format is in YYGG/YYgg with 9 charaters
C
           CALL ST_INTG ( carstr ( :2 ), ivbday, ier )
           IF ( ivbday .gt. 31 ) iret = -1
C
           CALL ST_INTG ( carstr ( 3:4 ), ivbhr, ier )
           IF ( ivbhr .lt. 24 ) THEN
             ELSE IF ( ivbhr .eq. 24 ) THEN
               ivbhr = 0
             ELSE
               iret = -1
           END IF
C
           CALL ST_INTG ( carstr ( 6:7 ), iveday, ier )
           IF ( iveday .gt. 31 ) iret = -1
C
           CALL ST_INTG ( carstr ( 8:9 ), ivehr, ier )
           IF ( ( ( ivehr .gt.  0 ) .and. ( ivehr .le. 24 ) ) .or.
     +          ( ( ivehr .gt. 50 ) .and. ( ivehr .le. 74 ) ) ) THEN
             ELSE IF ( ( ivehr .eq. 0 ) .or. ( ivehr .eq. 50 ) ) THEN
               ivehr = ivehr + 24
             ELSE
               iret = -1
           END IF
        END IF
C*
        RETURN
        END
