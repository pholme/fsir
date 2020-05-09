! ===========================================================================
! Module containing random number generator. Based on the
! PCG RNG v0.94 http://www.pcg-random.org under the Apache License 2.0
! http://www.apache.org/licenses/LICENSE-2.0
! with 32-bit Output, 64-bit State: PCG-XSH-RS

MODULE pcg_rng
    USE,INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    INTEGER(KIND=INT64) :: p_state
    INTEGER(KIND=INT32) :: mem
    LOGICAL :: generate_new = .TRUE.

    PRIVATE :: mem, generate_new

    CONTAINS

! ---------------------------------------------------------------------------
    FUNCTION pcg_32 ()
        USE,INTRINSIC :: ISO_FORTRAN_ENV
        IMPLICIT NONE
        INTEGER(KIND=INT32) :: value, rot, pcg_32
        INTEGER(KIND=INT64) :: state

        state = p_state
        p_state = p_state * 6364136223846793005_INT64 + 1442695040888963407_INT64
        value = ISHFT(XOR(ISHFT(state,-18),state),-27)
        rot = ISHFT(state,-59)
        pcg_32 = OR(ISHFT(value,-rot),ISHFT(value,AND(-rot,31)))
    END FUNCTION

! ---------------------------------------------------------------------------
    ! unlike the C version this uses only 31 bits and is thus slower
    ! it also returns values from 1,bound rather than 0,bound-1
    FUNCTION pcg_32_bounded (bound)
        INTEGER(KIND=INT32) :: threshold, r, pcg_32_bounded, bound

        threshold = (2147483647_INT32 / bound) * bound

        DO WHILE (.TRUE.)
            r = AND(pcg_32(),2147483647_INT32)
            IF (r.LT.threshold) EXIT
        END DO

        pcg_32_bounded = MOD(r,bound) + 1
    END FUNCTION

! ---------------------------------------------------------------------------
    ! 32-bit number from 1 to 2^16
    FUNCTION pcg_16 ()
        IMPLICIT NONE
        INTEGER(KIND=INT32) :: pcg_16

        IF (generate_new) THEN
            mem = pcg_32()
            generate_new = .FALSE.
            pcg_16 = ISHFT(mem,-16) + 1
        ELSE
            generate_new = .TRUE.
            pcg_16 = AND(mem,65535_INT32) + 1
        END IF

    END FUNCTION

END MODULE

! ===========================================================================
