	SUBROUTINE TF_VNUM ( carstr, lens, numerc )
C************************************************************************
C* TF_VNUM								*
C*									*
C* This routine returns a logical true (numerc) if input string (carstr)*
C* contains a valid report time in format YYGGgg or YYGG/yygg.      	*
C*									*
C* TF_VNUM ( CARSTR, LENS, NUMERC)       				*
C*									*
C* Input parameters:							*
C*	CARSTR    	CHAR*  		Report time for a bulletin 	*
C*	LENS  		INTEGER		The length of carstr    	*
C*									*
C* Output parameters:							*
C*	numerc    	LOGICAL		Flag for decoding the repo time *
C*									*
C**									*
C* Log:									*
C* L.LIN/NCEP		4/08	Created                                 *
C************************************************************************
C*
	CHARACTER*(*)   carstr
	LOGICAL         numerc
C
        IF ( lens .eq. 6 ) THEN
C
C*         Valid time format is in (old) YYGGgg with 6 charaters
C*         Look for the valid period (2 digit day, 2 digit beginning
C*         time, 2 digit ending time).
C
            numerc = .true.
            DO jj = 1, 6
                CALL ST_ALNM ( carstr (jj:jj), ityp, ier )
                IF ( ityp .ne. 1 ) numerc = .false.
            END DO
C
C*          Valid time format is in YYGG/YYgg with 9 charaters
C
        ELSE IF ( lens .eq. 9 ) THEN
            numerc = .true.
C
C*          check the first 4 characters YYGG
C
            DO jj = 1, 4
                CALL ST_ALNM ( carstr (jj:jj), ityp, ier )
                IF ( ityp .ne. 1 ) numerc = .false.
            END DO
C
C*          check the next 4 characters YYgg
C
            DO jj = 6, 9
                CALL ST_ALNM ( carstr (jj:jj), ityp, ier )
                IF ( ityp .ne. 1 ) numerc = .false.
            END DO
C
C*          check the fifth character '/'
C
            IF ( carstr (5:5) .ne. '/' ) then
               numerc = .false.
            END IF
        ELSE
            numerc = .false.
        END IF
C*
        RETURN
        END
