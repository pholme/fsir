! ===========================================================================

PROGRAM sir
    USE pcg_rng
    USE epi
    USE nwk
    IMPLICIT NONE
    INTEGER :: i
    CHARACTER(LEN=128) :: arg

    IF (COMMAND_ARGUMENT_COUNT().NE.3) THEN
        WRITE (*,*) 'usage: ./sir [nwk file] [beta] [seed]'
        CALL EXIT(1)
    END IF

    ! reading input
    CALL GET_COMMAND_ARGUMENT(1, arg)
    CALL read_nwk(arg)
    CALL alloc_heap(n_n)

    CALL GET_COMMAND_ARGUMENT(2, arg)
    READ (arg,*) e_beta

    CALL GET_COMMAND_ARGUMENT(3, arg)
    READ (arg,"(I20)") p_state

    ! initializing
    CALL epi_init()

    ! running the simulation
    DO i = 1,e_navg
        CALL run_sir()
        CALL sum_s()
    END DO

    !printing and exiting
    CALL print_stats()
    CALL clean_nwk()
    CALL clean_heap()

END PROGRAM

! ===========================================================================
