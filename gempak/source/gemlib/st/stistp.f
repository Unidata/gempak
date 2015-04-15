	SUBROUTINE ST_ISTP ( string, istp, iret )
C************************************************************************
C* ST_ISTP								*
C*									*
C* This routine determine if the input string is a file name template.	*
C*									*
C* Valid substrings for the template include:				*
C*									*
C*	YYYY		Year with the century				*
C*	YY		Year without the century			*
C*	MMM		Month 3 letter abbreviation			*
C*	NNN		Month 3 letter abbreviation, all caps		*
C*	MM		Month number					*
C*	DD		Day						*
C*	HH		Hour						*
C*	NN		Minute						*
C*	DWK		Day of the week 3 letter abbreviation		*
C*	DWU		Day of the week 3 letter abbreviation, all caps	*
C*	fFFFFF		5-character representation of forecast hour/min	*
C*	fFFF		3-character representation of forecast hour	*
C*	fFF		2-character representation of forecast hour	*
C*									*
C* ST_ISTP ( STRING, ISTP, IRET )             				*
C*									*
C* Input parameters:							*
C*	STRING		CHAR*		String				*
C*									*
C* Output parameters:							*
C*	ISTP  		LOGICAL		Is the template			*
C*	IRET		INTEGER		Return code			*
C*                                      -1 - Not a file name template 	*
C*									*
C**									*
C* Log:									*
C* M. Li/SAIC		02/02						*
C* S. Jacobs/NCEP	 8/14	Added support for FFFFF template 	*
C************************************************************************
	CHARACTER*(*)	string
C*
	LOGICAL		istp
C*
C------------------------------------------------------------------------
	iret = 0
C
	istp = .false.
C
C*      Check if YYYY is in the string.
C
        ip = INDEX ( string, 'YYYY' )
        IF  ( ip .ne. 0 )  THEN
	    istp = .true.
	    RETURN
        END IF
C
C*      Check if YY is in the string.
C
        ip = INDEX ( string, 'YY' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if MMM is in the string.
C
        ip = INDEX ( string, 'MMM' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if NNN is in the string.
C
        ip = INDEX ( string, 'NNN' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if MM is in the string.
C
        ip = INDEX ( string, 'MM' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if DD is in the string.
C
        ip = INDEX ( string, 'DD' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if HH is in the string.
C
        ip = INDEX ( string, 'HH' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if NN is in the string.
C
        ip = INDEX ( string, 'NN' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if DWK is in the string.
C
        ip = INDEX ( string, 'DWK' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if DWU is in the string.
C
        ip = INDEX ( string, 'DWU' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if FFFFF is in the string.
C
        ip = INDEX ( string, 'FFFFF' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if FFF is in the string.
C
        ip = INDEX ( string, 'FFF' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
C*      Check if FF is in the string.
C
        ip = INDEX ( string, 'FF' )
        IF  ( ip .ne. 0 )  THEN
            istp = .true.
            RETURN  
        END IF
C
 	iret = -1
C*
	RETURN
	END
