! ===========================================================================
! implementing the binary heap, to keep track of future infection events
! these routines are more or less standard (see the C code for some more notes)

MODULE heap
    USE nwk
    IMPLICIT NONE
    INTEGER :: h_nheap
    INTEGER, DIMENSION (:), ALLOCATABLE :: h_heap, h_ninx ! h_ninx corresponds to node[].heap in the C code

    CONTAINS

! ---------------------------------------------------------------------------
    RECURSIVE SUBROUTINE down_heap (here)
        IMPLICIT NONE
        INTEGER :: itmp, smallest, left, right, here

        smallest = here
        left = here * 2
        right = left + 1

        IF ((left.LE.h_nheap).AND.(n_time(h_heap(left)).LT.n_time(h_heap(smallest)))) smallest = left

        IF ((right.LE.h_nheap).AND.(n_time(h_heap(right)).LT.n_time(h_heap(smallest)))) smallest = right

        IF (smallest.NE.here) THEN
            itmp = h_heap(smallest)
            h_heap(smallest) = h_heap(here)
            h_heap(here) = itmp

            h_ninx(h_heap(smallest)) = smallest
            h_ninx(h_heap(here)) = here

            CALL down_heap(smallest)
        END IF

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE up_heap (start)
        IMPLICIT NONE
        INTEGER :: above, here, mem, start

        here = start
        mem = h_heap(start)

        DO WHILE (here.GT.1)
            above = ISHFT(here,-1)

            IF (n_time(mem).GE.n_time(h_heap(above))) EXIT

            h_heap(here) = h_heap(above)
            h_ninx(h_heap(here)) = here

            here = above
        END DO

        h_heap(here) = mem
        h_ninx(mem) = here

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE del_root ()
        IMPLICIT NONE
        INTEGER :: i

        i = h_heap(h_nheap)
        h_nheap = h_nheap - 1
        h_heap(1) = i
        h_ninx(i) = 1
        CALL down_heap(1)

    END SUBROUTINE

! ---------------------------------------------------------------------------
    SUBROUTINE alloc_heap (n)
        IMPLICIT NONE
        INTEGER :: n

        ALLOCATE(h_heap(n))
        ALLOCATE(h_ninx(n))

    END SUBROUTINE


! ---------------------------------------------------------------------------
    SUBROUTINE clean_heap ()
        
        DEALLOCATE(h_heap)
        DEALLOCATE(h_ninx)

    END SUBROUTINE

END MODULE

! ===========================================================================
