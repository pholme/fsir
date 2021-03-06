# fsir
This is Fortran implementation of my network SIR code: https://github.com/pholme/sir/

Compile the code (I tried ifort as well as gfortran):

```
gfortran -Ofast pcg_rng.f90 nwk.f90 heap.f90 epi.f90 sir.f90 -o fsir
```

The input network format is a bit unusual. The nodes need to be encoded by numbers from 1 to N. The first line gives the number of nodes and links. The following N lines give the first and last index of the neighbors of the nodes in the array of neighbors. The following 2M lines give the neighbors. A o-o-o network could thus be encoded as:

```
3 2
1 1  
2 3  
4 4  
2  
1  
3  
2
```

If that is the content of a file named ooo.txt, you can run the program as for example:

```
./fsir ooo.txt 1.0 -8023442006145916107
```

The arguments are: the network file name, the infection rate (in units of the recovery rate), a 64-bit number to seed the RNG. The seed should have as high entropy as possible, i.e. be a 64-bit number itself generated by a RNG.

The code follows the original C code as close as possible. Most functions/subroutines have the same names, but there are some caveates: To get random numbers of a bounded interval, the C code uses a trick that needs unsigned ints, so here it is basically based on 31-bit numbers which makes the output slighly different (even though the state progresses in the same way).
