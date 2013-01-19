        SUBROUTINE TB_PARM ( prmlst, parms, colors, iret )
C************************************************************************
C* TB_PARM                                                              *
C*                                                                      *
C* This subroutine reads a table of parameter aliases and associated    *
C* parameter and color lists.  Semicolon-separated lists for the        *
C* parameters and colors are returned.                                  *
C*                                                                      *
C* TB_PARM ( PRMLST, PARMS, COLORS, IRET )                              *
C*                                                                      *
C* Input parameters:                                                    *
C*      PRMLST          CHAR*		Alias name                      *
C*                                                                      *
C* Output parameters:                                                   *
C*      PARMS           CHAR*		Parameters                      *
C*      COLORS          CHAR*		Colors                          *
C*      IRET            INTEGER         Return code                     *
C*                                         0 = normal return            *
C*                                         2 = no parm alias found      *
C*					 -13 = no parameters found	*	 
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* S. Maxwell/GSC       3/97                                            *
C* S. Maxwell/GSC	6/97	Added logic to read additional strings  *
C*				after alias name  			*
C* T. Piper/GSC		11/98	Updated prolog				*
C************************************************************************
        CHARACTER*(*)   prmlst, parms, colors
C*
        CHARACTER       prmuc*72, buffer*80, cuprm*72, cparm*72, 
     +		        ccolor*72, type*12, alias*80
        LOGICAL         found, done
C*
	SAVE		cuprm, cparm, ccolor
C------------------------------------------------------------------------
        iret = 0
C
C*      Convert alias to upper case.
C
        CALL ST_LCUC ( prmlst, prmuc, ier )
C
	IF ( cuprm .eq. prmuc ) THEN
	    parms  = cparm
	    colors = ccolor
	    RETURN
	END IF
C
C*      Open table.
C
        CALL FL_TBOP ( 'prmlst.tbl', 'config', lun, ier )
        IF ( ier .ne. 0 ) THEN
            CALL ER_WMSG  ( 'FL', ier, 'prmlst.tbl', ierr )
            RETURN
        END IF
C
2       FORMAT ( A )
C
	parms  = ' '
	colors = ' '
C
        found  = .false.
        iostat = 0
        j      = 0
C
C*      Match alias with its parameter list.
C
        DO WHILE  (( iostat .eq. 0 ) .and. ( .not. found ))
            READ ( lun, 2, IOSTAT = iostat ) buffer
            IF (( iostat .eq. 0 ) .and. ( buffer (1:1) .eq. '^' ))
     +          THEN
		ibpos = INDEX ( buffer, '|' )
C
		IF ( ibpos .eq. 0 ) THEN
		    alias = buffer (2:)
		ELSE IF ( ibpos .ge. 3 ) THEN
		    alias = buffer ( 2: (ibpos-1))
		ELSE 
		    alias = ' '
		END IF
C
                IF ( alias .eq. prmuc ) THEN
                    found = .true.
C
C*                  Put parameters and colors into the arrays.
C
		    done = .false.
C
		    DO WHILE (( iostat .eq. 0) .and. ( .not. done ))
			READ ( lun, 2, IOSTAT = iostat ) buffer
			IF (( iostat .ne. 0 ) .or. 
     +		           (buffer (1:1) .eq. '^' )) THEN
				    done = .true.
			ELSE
			   CALL ST_LCUC ( buffer, buffer, ier )
			   ieq = INDEX ( buffer, '=' )
			   IF ( ieq .gt. 1 ) THEN
			      type = buffer (1:ieq -1)
			      IF (( type .eq. 'SFPARM' ) .or. 
     +			         ( type .eq. 'SNPARM' )) THEN
				      parms = buffer (ieq+1:)
			      ELSE IF ( type .eq. 'COLORS' ) THEN
				      colors = buffer (ieq+1:)
			      END IF
			   END IF
			END IF
		    END DO
                END IF
            END IF
        END DO
C
C*      Close table.
C
        CALL FL_CLOS ( lun, ier )
C
	cuprm  = ' '
	cparm  = parms
	ccolor = colors
C
	IF ( .not. found ) THEN
	   iret = 2
	ELSE
	   IF ( parms .eq. ' ' ) THEN
	      iret  = -13
	   ELSE
	      cuprm = prmuc
	   END IF
	END IF
C*
	RETURN
	END
