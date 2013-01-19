	SUBROUTINE CPCGOUT (output, time, nstn, istnm, iflag, iclr, iret)
C************************************************************************
C* CPCGOUT								*
C*									*
C* This subroutine writes the data to the requested devices.		*
C*									*
C* CPCGOUT  ( OUTPUT, TIME, NSTN, ISTNM, IFLAG, ICLR, IRET )		* 
C*									*
C* Input parameters:							*
C*	OUTPUT(LLMXLN)  CHAR*		Output variable			*
C*	TIME		CHAR*20		gempak time			*
C*	NSTN		INTEGER		Number of stations		*
C*	ISTNM(LLSTFL)	INTEGER		Stations id number		*
C*	IFLAG(LLSTFL)	INTEGER		Inside/outside flag		*
C*	ICLR(LLSTFL)	INTEGER		Polygon color			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal			*
C**									*
C* Log:									*
C* M. Li/SAIC		08/01	Create					*
C* M. Li/SAIC		09/01	Modified the parameter in IN_OUTT	*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	INTEGER		lun (LLMXTM), istnm (LLSTFL), iflag(LLSTFL), 
     +                       iclr(LLSTFL) 

	CHARACTER	time*20 
C*
	CHARACTER	output*(LLMXLN), outdev (4)*1
C------------------------------------------------------------------------
	iret = 0

	CALL IN_OUTT  ( output, 'CPCGSTN', lun, nlun, outdev, ier )
C
C*	Write the header
C
	DO  ilun = 1, nlun
           WRITE (lun(nlun), *) 'PARM = FLAG;ICLR'
	   WRITE (lun(nlun), *) ' '
	   WRITE (lun(nlun), *) '   STN    YYMMDD/HHMM    FLAG    ICLR'
	END DO
C
C* 	Write data to requested luns
C
	DO  ilun = 1, nlun
	   DO ii = 1, nstn
	      WRITE (lun(nlun), 1000)
     +			istnm(ii), time, iflag(ii), iclr(ii)
1000          FORMAT( 1X, I6, 4X, A11, 2(4X, I4) )
	   END DO
	END DO

C*
	RETURN
	END
