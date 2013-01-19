	SUBROUTINE HC_CTIM ( istarr, temptim, strmtim, iret )
C************************************************************************
C* HC_CTIM 								*
C*									*
C* This subroutine converts UTC valid times for tropical distubances to *
C* GEMPAK times in UTC.                                                 *
C*                                                                      *
C* HC_CTIM ( ISTARR, TEMPTIM, STRMTIM, IRET )				*
C*									*
C* Input parameters:							*
C*	ISTARR (*)      INTEGER		Time array			*
C*	TEMPTIM		CHAR*		Position time			*
C*									*
C* Output parameters:							*
C*	STRMTIM		CHAR*     	Position time;GEMPAK format	*
C*	IRET		INTEGER		Return code			*
C*				 	  0 = normal return             *
C*									*
C**									*
C* Log:									*
C* A. Hardy/GSC		 9/99						*
C* A. Hardy/GSC		 6/00		Corrected prolog		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
C*
	CHARACTER*(*)	temptim, strmtim
        INTEGER		istarr(*)
C*
	INTEGER         jotarr(5)
C------------------------------------------------------------------------
	iret = 0
C
C*	
C
          jotarr (1) = istarr(1)
          jotarr (2) = istarr(2)
          CALL ST_NUMB ( temptim(1:2), jotarr (3), ier )
          CALL ST_NUMB ( temptim(4:5), jotarr (4), ier )
          CALL ST_NUMB ( temptim(6:7), jotarr (5), ier )

          CALL TI_ITOC ( jotarr, strmtim, iret )

C*
	RETURN
	END
