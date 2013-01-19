	SUBROUTINE MS_MISS ( imiss, line, iprms, ifcstm, rdata, iret )
C************************************************************************
C* MS_MISS                                                              *
C*									*
C* This subroutine checks for missing data and stores a line of decoded *
C* integer data into the real GEMPAK data array.                        *
C*                                                                      *
C* MS_MISS ( IMISS, LINE, IPRMS, IFCSTM, RDATA, IRET )                  *
C*									*
C* Input parameters:							*
C*	IMISS		INTEGER		MOS value for missing data      *
C*	LINE (*)	INTEGER		Forecast data line              *
C*	IPRMS		INTEGER		Position of parameter in output *
C*	IFCSTM		INTEGER		Number of forecast times        *
C*									*
C* Output parameters:							*
C*	RDATA		REAL		Forecast data                   *
C*	  (IFCSTM,MMPARM)                                               *
C*	IRET		INTEGER		Return code			*
C**									*
C* Log:									*
C* D. Kidwell/NCEP	 9/00						*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		line (*)
	REAL		rdata (ifcstm,MMPARM)
C------------------------------------------------------------------------
	iret = 0
C
C*	Check for missing data and store values to GEMPAK array.
C
	DO ii = 1, ifcstm
	    IF ( line ( ii ) .ne. imiss ) THEN
		rdata ( ii, iprms ) = FLOAT ( line ( ii ) ) 
	      ELSE
		rdata ( ii, iprms ) = RMISSD
	    END IF
	END DO
C*
	RETURN
	END
