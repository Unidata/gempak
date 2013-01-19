	SUBROUTINE IP_LIST  ( pname, list, iret )
C************************************************************************
C* IP_LIST								*
C*									*
C* This subroutine lists the current value for the variable.  If	*
C* PNAME is blank, all the variables for the current program will	*
C* be listed.								*
C*									*
C* IP_LIST  ( PNAME, LIST, IRET )					*
C*									*
C* Input parameters:							*
C*	PNAME		CHAR*		Variable name			*
C*	LIST		LOGICAL		List flag			*
C*									*
C* Output parameters:							*
C*	IRET		INTEGER		Return code			*
C*				   	  0 = normal return 		*
C**									*
C* Log:									*
C* M. desJardins/GSFC	 6/88	Added non-TAE subs			*
C* M. desJardins/GSFC	10/90	Added DISPLAY function			*
C* J. Whistler/SSAI	 4/91	Fixed DISPLAY write out for long cparmv	*
C* M. desJardins/NMC	 1/92	Eliminate attempt to wrap nicely for D	*
C* K. Brill/NMC		 5/93	Eliminated call to ST_LCUC		*
C* K. Tyle/GSC		 7/96	Renamed from NT_LIST			*
C* T. Piper/GSC		11/98	Updated prolog				*
C* T. Piper/GSC		 6/00	Added checks for lenc = 0		*
C************************************************************************
	INCLUDE		'ipcmn.cmn'
C*
	CHARACTER*(*)	pname
	LOGICAL		list
C*
	CHARACTER	parm*8, ppp*8
C-----------------------------------------------------------------------
	iret = 0
C
C*	See if input value is blank.
C
	CALL ST_LDSP  ( pname, ppp, np, ier )

	IF  ( np .eq. 0 )  THEN
C
C*	    Write out each variable.
C
	    i = ihead
	    DO WHILE  ( i .gt. 0 )
		IF ( cparmn (i) (1:1) .ne. '$')  THEN
		    parm = cparmn (i)
		    CALL ST_LSTR  ( cparmv (i), lenc, ier )
C
C  The following line was added because when lenc = 0
C      it results in a subscript out of range error
C
		    IF  ( lenc .eq. 0 ) lenc = 1
		    IF  ( list )  THEN
			WRITE  ( 6, 1000 ) parm, cparmv (i) ( :lenc )
1000			FORMAT ( 1X, A, ' = ', A )
		      ELSE
			WRITE  ( 6, 1002 ) parm, chelp1 (i), 
     +					   cparmv (i) ( 1:lenc )
1002			FORMAT ( 1X, A, 2X, A, 2X, A )
		    END IF
		END IF
		j = iplink (i)
		i = j
	    END DO
C
C*	    Otherwise, list value for one parameter.
C
	  ELSE
C
C*	    Find this parameter in the list checking first for an
C*	    abbreviation.
C
	    CALL IP_FVAR  ( ppp, ilist, ier )
	    IF  ( ier .ne. 0 )  RETURN
	    IF  ( ilist .eq. 0 )  THEN
		DO  i = 1, ncparm
		    IF  ( cparmn (i) .eq. ppp )  ilist = i
		END DO
	      ELSE
		ppp = cparmn (ilist)
	    END IF
C
C*	    Write error message and return if this parameter is not
C*	    in list.
C
	    IF  ( ilist .eq. 0 )  THEN
		ier = -7
		CALL ER_WMSG  ( 'IP', ier, ppp, ierr )
	      ELSE
C
C*		Write parameter and value.
C
		CALL ST_LSTR  ( cparmv (ilist), lenc, ier )
C
C  The following line was added because when lenc = 0
C      it results in a subscript out of range error
C
		IF  ( lenc .eq. 0 ) lenc = 1
		IF  ( list )  THEN
		    WRITE  ( 6, 1000 ) ppp, cparmv (ilist) ( :lenc )
		  ELSE
		    WRITE  ( 6, 1002 ) ppp, chelp1 (ilist), 
     +				       cparmv (ilist) (1:lenc)
		END IF
	    END IF
	END IF
C*
	RETURN
	END
