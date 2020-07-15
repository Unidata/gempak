      MODULE IP_READ
        USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_CHAR, C_NULL_CHAR,
     +     C_PTR, C_F_POINTER
        IMPLICIT NONE
  
        INTERFACE
          SUBROUTINE RL_GETS(prompt, line) BIND(C, NAME="rl_get")
            IMPORT :: C_CHAR, C_PTR
            CHARACTER(KIND=C_CHAR), DIMENSION(*) :: prompt
            TYPE(C_PTR) :: line
          END SUBROUTINE RL_GETS
        END INTERFACE
  
      CONTAINS

        SUBROUTINE GET_INPUT_LINE(p, c)
C**********************************************************************
C* GET_INPUT_LINE                                                     *
C*                                                                    *
C* This subroutine uses the GNU Readline library to provide the       *
C* GEMPAK prompt with bash/emacs-type features.  The arrow keys       *
C* work, for instance.                                                *
C*                                                                    *
C* GET_INPUT_LINE  ( p, c )                                           *
C*                                                                    *
C* Input parameters:                                                  *
C*      p       CHARACTER(*)          Prompt text                     *
C*                                                                    *
C* Output parameters:                                                 *
c*      c       CHARACTER(*)          String input by user            *
C**                                                                   *
C* Log:                                                               *
C* S. Decker/Rutgers   7/20                                           *
C**********************************************************************
          CHARACTER(*), INTENT(IN) :: p
          CHARACTER(*), INTENT(OUT) :: c

          CHARACTER(KIND=C_CHAR, LEN=LEN(p)+1) :: prompt    
          TYPE(C_PTR) :: res
          CHARACTER, POINTER, DIMENSION(:) :: cp
          INTEGER :: n, i
c------------------------------------------------------------------    
          n = LEN(c)
          prompt = p // C_NULL_CHAR

          CALL RL_GETS(prompt, res)
          CALL C_F_POINTER(res, cp, [n])

          c = ""
          DO i = 1, n
            IF (cp(i) == C_NULL_CHAR) EXIT
            c(i:i) = cp(i)
          END DO
    
        END SUBROUTINE GET_INPUT_LINE
      END MODULE IP_READ
