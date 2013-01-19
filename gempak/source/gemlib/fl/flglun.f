	SUBROUTINE FL_GLUN  ( lun, iret )
C************************************************************************
C* FL_GLUN								*
C* 									*
C* This subroutine gets a logical unit number that can be used for	*
C* file access.  It is used to eliminate conflicts in assigning		*
C* logical unit numbers.						*
C* 									*
C* FL_GLUN  ( LUN, IRET )						*
C* 									*
C* Output parameters:							*
C*	LUN		INTEGER		Logical unit number 		*
C*	IRET		INTEGER		Return code			*
C*					   0 = normal return		*
C*					-21 = no more luns		*
C**									*
C* Log:									*
C* G.C.Chatters/RDS	 3/84						*
C* M. desJardins/GSFC	 3/86	Changed comments			*
C* I. Graffman/RDS	10/87	Call to SS_GLUN				*
C* M. desJardins/GSFC	 7/90	Eliminate system service		*
C* M. desJardins/NMC	 8/94	Documentation				*
C* K. Tyle/GSC		12/95	Changed initial iret to -101		*
C* G. Krueger/EAI	 8/96	Changed iret value			*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'GMBDTA.CMN'	
C------------------------------------------------------------------------
	iret = -21
C
C*	Get logical unit number from table.
C
	lun  = -1
	knt  = 1
	DO WHILE  ( ( knt .le. 10 ) .and. ( lun .eq. -1 ) )
	    IF  ( lungem ( knt ) .eq. 0 )  THEN
		lun = 10 + knt
		lungem (knt) = 1
		iret = 0
	      ELSE
		knt = knt + 1
	    END IF
	END DO
C*
	RETURN
	END
