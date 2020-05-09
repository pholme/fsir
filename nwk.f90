! ===========================================================================
! reading and forgetting the network

MODULE nwk
    IMPLICIT NONE
    INTEGER :: n_n, n_m
    INTEGER, DIMENSION (:), ALLOCATABLE :: n_first, n_last, n_nb
    REAL, DIMENSION (:), ALLOCATABLE :: n_time

    CONTAINS

! ---------------------------------------------------------------------------
! reading the network
    SUBROUTINE read_nwk (fname)
        IMPLICIT NONE
        INTEGER :: i, me, you
        CHARACTER(LEN=128) :: fname

        OPEN(10, FILE = fname, STATUS = 'old')
        READ(10,*) n_n, n_m ! first the sizes

        ALLOCATE(n_time(n_n))
        ALLOCATE(n_first(n_n))
        ALLOCATE(n_last(n_n))
        ALLOCATE(n_nb(2*n_m))

        DO i = 1,n_n ! then the indices of every node in the array of neighbors of the nodes
            READ(10,*) me, you
            n_first(i) = me
            n_last(i) = you
        END DO

        DO i = 1,2*n_m ! then the actual neighbors
            READ(10,*) me
            n_nb(i) = me
        END DO

        CLOSE(10)

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE clean_nwk () ! just deallocating
        DEALLOCATE(n_time)
        DEALLOCATE(n_first)
        DEALLOCATE(n_last)
        DEALLOCATE(n_nb)
    END SUBROUTINE

END MODULE

! ===========================================================================
