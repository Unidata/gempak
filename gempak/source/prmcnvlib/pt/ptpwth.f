	CHARACTER*(*) FUNCTION PT_PWTH  ( pwwm )
C************************************************************************
C* PT_PWTH								*
C*									*
C* This character function converts a numeric WMO past weather code,	*
C* PWWM, into a character weather code:					*
C*									*
C*                    PWTH = PT_PWTH ( PWWM )				*
C*									*
C* The values for the numeric values are:				*
C*	0 = Cloud covering less than 1/2 sky				*
C*	1 = Cloud covering more than 1/2 sky during part of period	*
C*	    and less than 1/2 during part of period			*
C*	2 = Cloud covering more than 1/2 sky				*
C*	3 = Sandstorm, dust storm or blowing snow			*
C*	4 = Fog, ice fog, thick haze or thick smoke			*
C*	5 = Drizzle							*
C*	6 = Rain							*
C*	7 = Snow, mixed rain and snow, or ice pellets			*
C*	8 = Showers							*
C*	9 = Thunderstorm with or without precipitation			*
C*									*
C* The conversion is:							*
C*									*
C*    0 = ' '           5 = L						*
C*    1 = ' '           6 = R						*
C*    2 = ' '           7 = S						*
C*    3 = BD            8 = RW						*
C*    4 = F             9 = T						*
C*									*
C* PT_PWTH  ( PWWM )							*
C*									*
C* Input parameters:							*
C*	PWWM		REAL		Numeric past weather code	*
C*									*
C* Output parameters:							*
C* 	PT_PWTH		CHAR*		Character past weather		*
C**									*
C* Log:									*
C* I. Graffman/RDS	12/84						*
C************************************************************************
	CHARACTER*8 wthr (10) 
	DATA	    wthr / 3*' ', 'BD', 'F', 'L', 'R', 'S', 'RW', 'T'/
C----------------------------------------------------------------------------
	icode = pwwm
	IF  ( ( icode .lt. 0 ) .or. ( icode .gt. 9 ) )  THEN
	    PT_PWTH = ' '
	  ELSE
	    PT_PWTH = wthr ( icode + 1 )
	ENDIF
C*
	RETURN
	END
