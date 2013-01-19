        SUBROUTINE OABSTM  ( timall, otimall, ogdattm, iret )
C************************************************************************
C* OABSTM                                                               *
C*                                                                      *
C* This subroutine checks the format of an input time string otimall. 	*
C* If the format is yymmdd/hhmm, or yymmdd/hhmmFhhh or Fhhh, it sets    *
C* the output variable ogdattm to otimall. Otherwise, it sets ogdattm	*
C* to timall.							        *
C*                                                                      *
C* OABSTM ( TIMALL, OTIMALL, OGDATTM, IRET )				*
C*                                                                      *
C* Input parameters:                                                    *
C*      TIMALL          CHAR*           input date/time                 *
C*      OTIMALL         CHAR*           input grid date/time            *
C*                                                                      *
C* Output parameters:                                                   *
C*      OGDATTM         CHAR*           ouput grid date/time            *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal                    *
C**                                                                     *
C* Log:                                                                 *
C* B. Yin/SAIC		06/05	Created					*
C* B. Yin/SAIC		08/05	Check if forecast hour is f# or f0#	*
C************************************************************************
        CHARACTER*(*)   timall, otimall, ogdattm
	CHARACTER	fhours*4, dattm*20
C*	
	LOGICAL		badf
C------------------------------------------------------------------------
C*
	iret = 0
C
C*	Search for character 'F'
C
	iindex = INDEX ( otimall, 'F' )
C*
	IF  ( iindex .eq. 0 ) THEN
C
C*	    in format yymmdd/hhmm
C
	    CALL ST_LSTR ( otimall, lentm, ier )
	    CALL TI_FORM ( otimall, ogdattm, ier )
C*
	    IF  ( ( lentm .ne. 11 ) .or. ( ier .ne. 0 ) ) THEN
	        ogdattm = timall
	    END IF
C*
	  ELSE IF  ( iindex .eq. 1 ) THEN
C
C*	    in format Fhhh
C	
	    CALL ST_LSTR ( otimall, lenhr, ier )
C*
	    badf = .false.
C*
	    IF ( ( lenhr .lt. 2 ) .or. ( lenhr .gt. 4 ) ) THEN
		badf = .true.
	    END IF
C*
	    ii = 2
	    DO WHILE ( .not. badf .and. ( ii .le. lenhr ) )
		IF ( ( otimall ( ii:ii ) .lt. '0' ) .or. 
     +		     ( otimall ( ii:ii ) .gt. '9' ) ) THEN
			badf = .true.
		END IF
		ii = ii + 1
	    END DO
C*
	    IF  ( badf ) THEN 
C*
		ogdattm = timall
C*
	      ELSE 
C*
		ogdattm = timall ( 1 : 11 ) // otimall
C*
	    END IF		 
C*
	  ELSE
C
C*	    in format yymmdd/hhmmFhhh
C
	    dattm = otimall ( 1 : iindex -1 )
	    fhours = otimall ( iindex : )
C*
	    CALL ST_LSTR ( fhours, lenhr, ier )
C*
	    CALL ST_LSTR ( dattm, lentm, ier )
C*
	    CALL TI_FORM ( dattm, ogdattm, ier )
C*
	    badf = .false.
C*
	    IF ( ( lenhr .lt. 2 ) .or. ( lenhr .gt. 4 ) ) THEN
		badf = .true.
	    END IF
C*
	    ii = 2
	    DO WHILE ( .not. badf .and. ( ii .le. lenhr ) )
		IF ( ( otimall ( ii:ii ) .lt. '0' ) .or. 
     +		     ( otimall ( ii:ii ) .gt. '9' ) ) THEN
			badf = .true.
		END IF
		ii = ii + 1
	    END DO
C*
	    IF  ( ( lentm .ne. 11 ) .or. ( ier .ne. 0 ) .or. 
     +	  	  badf ) THEN
C*
		ogdattm = timall
C*
	      ELSE
C*		
		ogdattm = otimall
C*
	    END IF
	END IF
C*
        RETURN
        END
