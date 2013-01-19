	SUBROUTINE AF_ARFG  ( irfn, field, lenf, isnf, ienf, iret )
C************************************************************************
C* AF_ARFG								*
C*									*
C* Given original report group number IRFN, this subroutine assembles	*
C* (into FIELD) the original report group consisting of all "like-type"	*
C* groups with that original report group number.  The indicees of the	*
C* starting and	ending "like-type" groups which make up the original	*
C* report group	are also returned in, respectively, ISNF and IENF.	*
C*									*
C* AF_ARFG  ( IRFN, FIELD, LENF, ISNF, IENF, IRET )			*
C*									*
C* Input parameters:							*
C*	IRFN		INTEGER		Original report group number 	*
C*									*
C* Output parameters:							*
C*	FIELD		CHAR*		Original report group 		*
C*	LENF		INTEGER		Length of FIELD 		*
C*	ISNF		INTEGER		Index of "like-type" group which*
C*					contains start of FIELD 	*
C*	IENF		INTEGER		Index of "like-type" group which*
C*					contains end of FIELD 		*
C*	IRET		INTEGER		Return code 			*
C*					  0 = normal return 		*
C*					 -1 = group number IRFN was	*
C*					      not found			*
C*					 +1 = FIELD not big enough to	*
C*					      hold group number IRFN 	*
C**									*
C* Log:									*
C* J. Ator/NP12		09/96						*
C* J. Ator/NP12		08/97	New interface format, style changes	*
C* J. Ator/NCEP		11/99	Declare field variable locally		*
C************************************************************************
	INCLUDE		'GEMPRM.PRM'
	INCLUDE		'afcmn.cmn'
C*
	CHARACTER*(*)	field
C*
	LOGICAL		done
C-----------------------------------------------------------------------
	iret = 0 
	lenf = 0
	field = ' '
C
C*	Find the first "like-type" group which is in the original
C*	report group.
C
	isnf = 0
	ii = 1
C
	DO WHILE  ( ( isnf .eq. 0 ) .and. ( ii .le. nflds ) )
	    IF  ( irfnsf (ii) .eq. irfn )  THEN
		isnf = ii
	    ELSE
		ii = ii + 1
	    END IF
	END DO
C
	IF  ( isnf .eq. 0 )  THEN
	    iret = -1
	    RETURN
	END IF
C
C*	Assemble the original report group by stringing end-to-end
C*	all "like-type" groups with the given original report group
C*	number.
C
	done = .false.
	ii = isnf
C
	DO WHILE  ( ( ii .le. nflds ) .and. ( .not. done ) )
	    IF  ( irfnsf (ii) .eq. irfn )  THEN
		IF  ( ( lenf + lensf (ii) ) .le. LEN ( field ) )  THEN
		    field ( ( lenf + 1 ) : ( lenf + lensf (ii) ) ) =
     +				( fields (ii) ( 1 : lensf (ii) ) )
		    lenf = lenf + lensf (ii)
		    ii = ii + 1
		ELSE
		    iret = 1
		    RETURN
		END IF
	    ELSE
		done = .true.
	    END IF
	END DO
C
	ienf = ii - 1
C*
	RETURN
	END
