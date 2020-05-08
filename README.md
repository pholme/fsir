# fsir
This is Fortran implementation of my network SIR code: https://github.com/pholme/sir/

Compile the code like this (with gfortran)

`gfortran -Ofast -march=native -c pcg_rng.f08 nwk.f08 heap.f08 epi.f08 sir.f08``
gfortran -Ofast -march=native -o fsir pcg_rng.f08 nwk.f08 heap.f08 epi.f08 sir.f08`

The input network format is a bit unusual. The nodes need to be encoded by numbers from 1 to N. The first line gives the number of nodes and links. The following N lines gives the first and last index of the neighbors of the nodes in the array of neighbors. The following 2M lines gives the neighbors. A o-o-o network could thus be encoded as:

`3 3`
`1 1`
`2 3`
`4 4`
`2`
`1`
`3`
`2`

It follows the original C code as close as reasonably possible. Most functions/subroutines have the same names, but there are some caveates: To get random numbers of a bounded interval, the C code uses a trick that needs unsigned ints, so here it is basically based on 31-bit numbers which makes the output slighly different (even though the state progresses in the same way).
