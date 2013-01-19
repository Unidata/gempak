	SUBROUTINE SFLHDR  ( nlun, lun, ncprm, cparm, iret )
C************************************************************************
C* SFLHDR								*
C*									*
C* This subroutine writes out the parameter name header.		*
C*									*
C* SFLHDR  ( NLUN, LUN, NCPRM, CPARM, IRET )				*
C*									*
C* Input parameters:							*
C*	NLUN		INTEGER		Number of devices 		*
C*	LUN   (NLUN)	INTEGER		LUNs of output devices		*
C*	NCPRM		INTEGER		Number of output parameters	*
C*	CPARM  (NCPRM)	CHAR*4		Output parameter names		*
C*									*
C* Output parameters:							*
C* 	IRET		INTEGER		 Return code			*
C*					   0 = normal return		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	10/87	GEMPAK4					*
C* M. desJardins/GSFC	 6/88	Print 6 parameters on a line		*
C* T. Lee/GSC		 5/97	Changed format spaces			*
C************************************************************************
	INTEGER		lun  (*)
	CHARACTER*(*)	cparm (*)
C*
	CHARACTER	output*80
C------------------------------------------------------------------------
	iret = 0
C
C*	First write list of parameter names; use only 80 characters.
C
	output = 'PARM = '
	i = 8
	j = 1
	DO WHILE  ( j .le. ncprm )
C
C*	    Add semicolon if this is not the first parameter.
C
	    IF  ( j .ne. 1 )  THEN
		output ( i: i ) = ';'
		i = i + 1
	    END IF
C
C*	    Check to see if string is too long.
C
	    IF  ( i .ge. 76 )  THEN
C
C*		Write output buffer.
C
		DO  ilun = 1, nlun
		    WRITE  ( lun ( ilun ), 1000 ) output
1000		    FORMAT ( 1X, A )
		END DO
		output = ' '
		i = 8
	    END IF
C
C*		Add parameter name.
C
	    output ( i: ) = cparm ( j )
	    i = i + 4
	    j = j + 1
	END DO
C
C*	Write list of parameters to each device.
C
	DO  ilun = 1, nlun
	    WRITE  ( lun ( ilun ), 1001 ) output
1001	    FORMAT ( 1X, A / )
	END DO
C
C*	Write names of parameters as a title.
C
	i1 = 1
	DO  WHILE  ( i1 .le. ncprm )
C
C*	    Write out 6 parameters at a time.
C
	    i2 = MIN  ( i1 + 5, ncprm )
C
C*	    Write station and time if this is the first list.
C
	    DO  ilun = 1, nlun
		IF  ( i1 .eq. 1 )  THEN
		    WRITE  ( lun ( ilun ), 2000 ) ( cparm (i), i=i1,i2 )
2000		    FORMAT ( '    STN    YYMMDD/HHMM ', 6 ( 5X, A4 ) )
		  ELSE
		    WRITE  ( lun ( ilun ), 2100 ) ( cparm (i), i=i1,i2 )
2100		    FORMAT ( 23X, 6 ( 5X, A4 ) )
		END IF
	    END DO
C
C*	    Increment pointer to parameter names.
C
	    i1 = i2 + 1
	END DO
C*
	RETURN
	END
