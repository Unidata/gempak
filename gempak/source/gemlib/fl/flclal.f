	SUBROUTINE FL_CLAL ( iret )
C************************************************************************
C* FL_CLAL								*
C* 									*
C* This subroutine closes all open files.				*
C* 									*
C* FL_CLAL  ( IRET )							*
C* 									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*					  0 = normal return		*
C*					 -2 = cannot close file		*
C**									*
C* Log:									*
C* K. Brill/NMC         8/90   						*
C* S. Maxwell/GSC      12/96   	Modified return code; removed FL_IRET	*
C************************************************************************
	INCLUDE        	'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'
C------------------------------------------------------------------------
	DO ilun = 1, 10
C*	    
   	    IF ( lungem ( ilun ) .ne. 0 ) THEN
C
C* 	        Close the file, get return code.
C
	        lun = ilun + 10
	        CLOSE ( UNIT = lun, IOSTAT = iostat )
C*
	        IF ( iostat .ne. 0 ) THEN
	            iret = -2
	        ELSE
	            iret = 0
	        END IF
C
C*	       Free the logical unit number.
C
	       CALL FL_FLUN ( lun, ier )
	    END IF
	END DO
C*
	RETURN
	END
