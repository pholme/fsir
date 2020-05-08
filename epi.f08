! ===========================================================================
! Module for the actual disease simulations and statistical analysis

MODULE epi
    USE nwk
    USE heap
    USE pcg_rng
    IMPLICIT NONE
    REAL :: e_beta, sr(2,2), r(2) ! results: outbreak size & time to extinction, sr sums r & r**2
    REAL, DIMENSION (65536) :: rexp
    INTEGER, PARAMETER :: e_navg = 100000, i_or_r = -1, none = -2

    PRIVATE :: r, sr, rexp, i_or_r, none

    CONTAINS
! ---------------------------------------------------------------------------
    SUBROUTINE epi_init () ! initializing relevant quantities
        IMPLICIT NONE
        INTEGER :: i

        sr = 0.0

        DO i = 1, 65536
            rexp(i) = -LOG(i / 65536.0) / e_beta
        END DO

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE sum_s () ! summing stuff for statistics
        IMPLICIT NONE
        INTEGER :: i, j

        DO i = 1,2
            DO j = 1,2
                sr(i,j) = sr(i,j) + r(i) ** j
            END DO
        END DO

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE print_stats () ! printing the final result
        IMPLICIT NONE
        INTEGER :: i, j

        DO i = 1,2
            DO j = 1,2
                sr(i,j) = sr(i,j) / e_navg
            END DO
            sr(i,2) = SQRT((sr(i,2) - sr(i,1) * sr(i,1)) / (e_navg - 1.0))
        END DO

        WRITE (*,*) '     OUTBREAK SIZE', sr(1,1), sr(1,2)
        WRITE (*,*) 'TIME TO EXTINCTION', sr(2,1), sr(2,2)

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE infect () ! doing the book keeping relating to the infection of one node
        IMPLICIT NONE
        INTEGER :: i, me, you
        REAL :: t, now

        me = h_heap(1)
        now = n_time(me)

        CALL del_root()

        h_ninx(me) = i_or_r
        n_time(me) = n_time(me) + rexp(pcg_32_bounded(65536)) * e_beta

        r(1) = r(1) + 1.0
        IF (r(2).LT.n_time(me)) r(2) = n_time(me)

        DO i = n_first(me),n_last(me)
            you = n_nb(i)
            IF (h_ninx(you).NE.i_or_r) THEN
                t = now + rexp(pcg_32_bounded(65536))
                ! if a new infection time of you is earlier than the current and earlier than the recovery time of me, then put the infection on the heap
                IF ((t.LT.n_time(me)).AND.(t.LT.n_time(you))) THEN
                    n_time(you) = t
                    IF (h_ninx(you).EQ.none) THEN
                        h_nheap = h_nheap + 1
                        h_heap(h_nheap) = you
                        h_ninx(you) = h_nheap
                        CALL up_heap(h_ninx(you))
                    END IF
                END IF
            END IF
        END DO

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE run_sir () ! managing one run of one outbreak simulation
        IMPLICIT NONE
        INTEGER :: source

        r = 0.0

        h_ninx = none
        n_time = HUGE(r(1))

        source = pcg_32_bounded(n_n)
        n_time(source) = 0.0
        h_ninx(source) = 1
        h_nheap = 1
        h_heap = source

        DO WHILE (h_nheap > 0)
            CALL infect()
        END DO
        
    END SUBROUTINE

END MODULE

! ===========================================================================
