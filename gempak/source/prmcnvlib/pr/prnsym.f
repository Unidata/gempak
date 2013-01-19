	FUNCTION PR_NSYM ( wnum )
C************************************************************************
C* PR_NSYM								*
C*									*
C* This function converts the GEMPAK weather code WNUM to the WMO 	*
C* weather code, WWMO, which is used to plot weather symbols.		*
C*									*
C* REAL PR_NSYM  ( WNUM ) 						*
C*									*
C* Input parameters:							*
C*	WNUM		REAL		GEMPAK numeric code		*
C*									*
C* Output parameters:							*
C*	PR_NSYM		REAL		Weather symbol number		*
C**									*
C* LOG:									*
C* S. Schotz/GSC	4/90		GEMPAK5				*
C************************************************************************
	CHARACTER	wcod*12, PT_WCOD*12
C------------------------------------------------------------------------
C*  	Convert weather number to character weather code
C
	wcod = PT_WCOD ( wnum )
C
C*      Convert weather character code to weather symbol number
C
	PR_NSYM = PT_WSYM ( wcod )
C*
	RETURN
	END
