        SUBROUTINE GH_BKPL ( ibkpts, numb, ddev, iarea, icnt, iret )
C************************************************************************
C* GH_BKPL                                                              *
C*                                                                      *
C* This subroutine calls plotting routines to plot sets of breakpoints	*
C* depending on their area type.					*
C*                                                                      *
C* GH_BKPL ( IBKPTS, NUMB, DDEV, IAREA, ICNT, IRET )                    *
C*                                                                      *
C* Input parameters:                                                    *
C*      IBKPTS (4,*)	INTEGER         Array of breakpoint sequence 	*
C*					numbers 			*
C*      NUMB (*)	INTEGER         Count of breakpoints sets	*
C*      DDEV		CHAR*           Device                          *
C*      IAREA(4,*)      INTEGER         Array of geographic area 	*
C*					designators      		*
C*      ICNT(4,*)      	INTEGER         Number of breakpoints in each	*
C*					set by type and severity	*
C*                                                                      *
C* Output parameters:                                                   *
C*      IRET            INTEGER         Return code                     *
C*                                        0 = normal return             *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* m.gamazaychikov/SAIC	04/04						*
C* B. Yin/SAIC		05/04	Modified variables passed to GH_BKAT	*
C* m.gamazaychikov/SAIC	08/04	Modified to allow the call to GH_BKPT	*
C* D. Kidwell/NCEP	 4/05	Added IKEYS as pairs type               *
C************************************************************************
        INCLUDE         'ghcmn.cmn'
C*
        INTEGER         ibkpts (4,*), numb (*), iarea(4,*), icnt(4,*)
        CHARACTER*(*)   ddev
C*
        INTEGER		idumb(2), iduma(2), iloc(50)
C------------------------------------------------------------------------
        iret = 0
C
C*      Query color and line attributes.
C
        CALL GH_SAVE ( ier ) 
C
C*      Loop over the four watch/warning types.
C
        DO ii = 1, 4
C
C*          Set line and color attributes and the order of plotting
C
            CALL GH_BKAT ( ii, numb, ddev, jpri, ier )
            jj = 1
            DO iset = 1, numb(jpri)
C
C*              For pairs of points
C
                IF ( iarea(jpri, jj) .eq. IUSGEC .or.
     +               iarea(jpri, jj) .eq. IMXCSA .or.
     +               iarea(jpri, jj) .eq. IPACIF .or.
     +		     iarea(jpri, jj) .eq. IKEYS ) THEN 
                    idumb(1) = ibkpts (jpri, jj)
                    idumb(2) = ibkpts (jpri, jj+1)
                    iduma(1) = iarea  (jpri, jj)
                    iduma(2) = iarea  (jpri, jj+1)
                    CALL GH_BKPP ( idumb, iduma, iret) 
                    jj = jj + 2
C
C*              For ordered pairs of points
C
                ELSE IF ( iarea(jpri, jj) .eq. ICUBA .or.
     +                    iarea(jpri, jj) .eq. IHISP  .or.
     +                    iarea(jpri, jj) .eq. IPRICO ) THEN 
                    idumb(1) = ibkpts (jpri, jj)
                    idumb(2) = ibkpts (jpri, jj+1)
                    iar      = iarea  (jpri, jj)
                    CALL GH_BKPO ( idumb, iar, iret)
                    jj = jj + 2
C
C*              For lists of points
C
                ELSE IF ( iarea(jpri, jj) .eq. IOTHER .or. 
     +                    iarea(jpri, jj) .eq. IWATER ) THEN 
                    iar      = iarea  (jpri, jj)
                    DO mm = 1, icnt (jpri, iset)
                        iloc (mm) = ibkpts (jpri, jj)
                        jj = jj + 1
                    END DO
                    CALL GH_BKPT ( iloc, icnt (jpri, iset), iar, iret )
                END IF
            END DO
        END DO
C
C*      Restore color and line attributes.
C
        CALL GH_REST ( ier )
C*
        RETURN
        END
