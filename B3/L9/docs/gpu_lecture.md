---
title:  GPU programming
event:  ESiWACE3-WarmWorld Summer School on HPC for Climate and Weather Applications
author: Claudia Frauen and Cristian-Vasile Achim
lang:   en
---

This course is based on the course "Introduction to GPU computing" by Marek Jacob (DWD)

![](img/acc_icon_marek.png){.center width=75%}

# Goals of this session

* What is a GPU and why do we want to use it?
* What is OpenACC and why do we use it?
* OpenACC basics

# What is accelerator computing?

* GPU = Graphical Processing Unit
* GPU computing: Use of a GPU to offload (compute intensive) parts of an application, while the remainder is computed on the CPU
* GPUs have thousands of compute cores: Need to express fine-grain parallelism 
* Until now GPU and CPU have separate physical memory (this is changing with the latest generation of GPUs)
  * requires specific data management
  * data transfer may be a performance issue (slow transfer via PCI bus)

# Why do we want to use GPUs?

* Because all major HPC systems now get the majority of their FLOPS/s from GPUs (so we kind of have to use them)
* GPU systems have typically a better energy efficiency
* Green500 list June 2024: Top 20 systems all use GPUs
* Levante has 16% of its FLOPS/s in GPU power

# How can we port code to GPUs?

* There are different options how code can be ported to GPUs:
  * Use directive-based programming models like OpenACC or OpenMP
  * Use C++ abstraction libraries like Kokkos or RAJA
  * Use vendor-specific language extensions like CUDA (NVIDIA), HIP (AMD) or SYCL (Intel)

# What are compiler directives?

* Compiler directives are comments or pragmas which can be inserted into existing code
* When a compiler directive is encountered the compiler/runtime will ...
  1. Generate parallel code for GPU
  2. Allocate GPU memory and copy input data
  3. Execute parallel code on GPU
  4. Copy output data to CPU and deallocate GPU memory
* Directives are ignored on architectures without GPU

# What is OpenACC?

* OpenACC is a specification for high-level compiler directives to express parallelism for accelerators in Fortran, C, and C++
  * **Aims** to be performance portable to a wide range of accelerators: Multiple vendors, multiple devices, one specification
* The OpenACC specification was first released in November 2011
  * Compilers: NVIDIA, Cray, gcc
* Official website: http://www.openacc.org

# Why do we often use OpenACC for weather and climate models?

* Weather and climate models are still mostly written in Fortran, have often been developed over decades, and can have more than a million lines of code
* Directives provide a relatively easy way to port these code to GPUs while keeping the code easily readable also for non HPC experts
* When the GPU port of ICON was started OpenMP was not an option
* For a long time NVIDIA was basically the only vendor of GPUs and invested significantly in the development of OpenACC, so that it can give relatively good performance on NVIDIA GPUs

# Disadvantages of OpenACC?

* The promise of performance portability could not be kept by OpenACC
* OpenACC is not supported by AMD or Intel who are now also providing GPUs
* Cray stopped OpenACC development for a while and only recently got back into it

# How does OpenMP compare to OpenACC?

* Since OpenMP 5.0 it offers comparable functionality to OpenACC and now many compilers are also supporting it
* Offers more portability than OpenACC since it is also supported on AMD and Intel GPUs
* However, on NVIDIA it might not give the same performance as OpenACC
* Many modelling groups are now looking at ways to transfer their codes from OpenACC to OpenMP

# OpenACC directive syntax

<span style="color: green;">
`!$ACC DIRECTIVE [CLAUSE1 [[,] CLAUSE2 [,] …]]` 
</span>
<br>
  ... often paired with a matching end directive surrounding a structured code block: 
<br>
<span style="color: green;">
`!$ACC END DIRECTIVE`
</span>

# Parallel and loop constructs

* <span style="color: green;"> `!$ACC PARALLEL` </span> starts parallel execution of the following section until the <span style="color: green;"> `!$ACC END PARALLEL` </span> 
* <span style="color: green;"> `!$ACC LOOP [{VECTOR, WORKER, GANG}]` </span> applies to the immediately following loop, and describes the type of accelerator parallelism (vector, worker, gang; more on this later) to use to execute the iterations of the loop. As a general rule all loops within a parallel region should have an <span style="color: green;"> `!$ACC LOOP` </span>
* <span style="color: green;"> `!$ACC LOOP SEQ` </span> executes the loop sequentially
* <span style="color: green;"> `!$ACC LOOP` </span> with no clause allows the compiler to choose the type of parallelism

# Example

```Fortran
SUBROUTINE saxpy(n, a, x, y)
   REAL :: x(n), y(n), a
   INTEGER :: n, i
   !$ACC PARALLEL
   !$ACC LOOP
   DO i=1,n
      y(i) = a*x(i)+y(i)
   END DO
   !$ACC END PARALLEL
END SUBROUTINE saxpy
```

* Date management:
  * Arrays `x` and `y` are automatically copied to the GPU
  * `y` is automatically copied back to the CPU after the kernel execution
  * scalars are automatically copied to the GPU

# OpenACC massive parallelism

```Fortran
DO jt = 1, n_tracers
   ! [...] some CPU-only management code
   !$ACC PARALLEL LOOP
   DO ic = 1, n_cells
      q_tracer(ic, jt) = q_tracer(ic, jt) + ddt(ic, jt)
   END DO
END DO
```
* Parallelize frequent loop (>10000 iterations) -> `ncells`

# OpenACC massive parallelism

```Fortran
DO ic = 1, n_cells
   ! [...] some CPU-only management code
   !$ACC PARALLEL LOOP
   DO jt = 1, n_tracers
      q_tracer(jt, ic) = q_tracer(jt, ic) + ddt(jt, ic)
   END DO
END DO
```
* `n_tracers=O(10)` is too small

# OpenACC massive parallelism

```Fortran
! [...] some CPU-only management code
!$ACC PARALLEL LOOP COLLAPSE(2)
DO ic = 1, n_cells
   DO jt = 1, n_tracers
      q_tracer(jt, ic) = q_tracer(jt, ic) + ddt(jt, ic)
   END DO
END DO
```
* `COLLAPSE(2)` tells the compiler to combine both loops and exposes maximum parallelism

# Hands-on exercises I

# Data transfer

<div class=column style=width:45%>

```Fortran
   PROGRAM p1A
   ! [variable declarations]

9    !$ACC PARALLEL LOOP
     do i = 1 , n
        a(i) = i
     end do
13   !$ACC END PARALLEL

15   !$ACC PARALLEL LOOP
     do i = 1 , n
        a(i) = 2*a(i)
     end do
19   !$ACC END PARALLEL

     s = 0.
```
</div>
<div class=column style=width:45%>
```Fortran
22   !$ACC PARALLEL
     !$ACC LOOP REDUCTION(+: s)
     do i = 1 , n
        s = s + a(i)
     end do
26   !$ACC END PARALLEL

     print * , s
  END PROGRAM
```
</div>

# Data transfer

<div class=column style=width:32%>

```Fortran
   PROGRAM p1A
   ! [variable declarations]

9    !$ACC PARALLEL LOOP
     do i = 1 , n
        a(i) = i
     end do
13   !$ACC END PARALLEL

15   !$ACC PARALLEL LOOP
     do i = 1 , n
        a(i) = 2*a(i)
     end do
19   !$ACC END PARALLEL

     s = 0.
```
</div>
<div class=column style=width:65%>
```
> NVCOMPILER_ACC_TIME=1 ./p1A
Accelerator Kernel Timing data
/work/k20200/k202137/ACC_examples/p1A.f90
  p1a  NVIDIA  devicenum=0
    time(us): 76
    9: compute region reached 1 time
        9: kernel launched 1 time
            grid: [8]  block: [128]
            elapsed time(us): total=29 max=29 min=29 avg=29
    9: data region reached 2 times
        13: data copyout transfers: 1
             device time(us): total=28 max=28 min=28 avg=28
```
</div>

# Data transfer

<div class=column style=width:32%>

```Fortran
   PROGRAM p1A
   ! [variable declarations]

9    !$ACC PARALLEL LOOP
     do i = 1 , n
        a(i) = i
     end do
13   !$ACC END PARALLEL

15   !$ACC PARALLEL LOOP
     do i = 1 , n
        a(i) = 2*a(i)
     end do
19   !$ACC END PARALLEL

     s = 0.
```
</div>
<div class=column style=width:65%>
```
> NVCOMPILER_ACC_TIME=1 ./p1A
Accelerator Kernel Timing data
/work/k20200/k202137/ACC_examples/p1A.f90
  p1a  NVIDIA  devicenum=0
    time(us): 76
    ...
    
    15: compute region reached 1 time
        15: kernel launched 1 time
            grid: [8]  block: [128]
            elapsed time(us): total=27 max=27 min=27 avg=27
    15: data region reached 2 times
        15: data copyin transfers: 1
             device time(us): total=12 max=12 min=12 avg=12
        19: data copyout transfers: 1
             device time(us): total=9 max=9 min=9 avg=9
```
</div>

# Data transfer

<div class=column style=width:32%>

```Fortran
22   !$ACC PARALLEL
     !$ACC LOOP REDUCTION(+: s)
     do i = 1 , n
        s = s + a(i)
     end do
26   !$ACC END PARALLEL

     print * , s
  END PROGRAM

```
</div>
<div class=column style=width:65%>
```
> NVCOMPILER_ACC_TIME=1 ./p1A
Accelerator Kernel Timing data
/work/k20200/k202137/ACC_examples/p1A.f90
  p1a  NVIDIA  devicenum=0
    time(us): 76
    ...

    22: compute region reached 1 time
        22: kernel launched 1 time
            grid: [8]  block: [128]
            elapsed time(us): total=19 max=19 min=19 avg=19
        22: reduction kernel launched 1 time
            grid: [1]  block: [256]
            elapsed time(us): total=19 max=19 min=19 avg=19
    22: data region reached 2 times
        22: data copyin transfers: 2
             device time(us): total=20 max=13 min=7 avg=10
        26: data copyout transfers: 1
             device time(us): total=7 max=7 min=7 avg=7

```
</div>



# Data clauses

| <span style="font-size:75%;"> Clause </span> | <span style="font-size:75%;"> Behavior </span> |
| - | ------ |
| <span style="font-size:75%;"> COPYIN </span> | <span style="font-size:75%;"> Check if variables are already present on device; if not create space and copy data to device </span> |
| <span style="font-size:75%;"> COPYOUT </span> | <span style="font-size:75%;"> Check if variables are already present on device; if not create space and copy resulting data from device </span> |
| <span style="font-size:75%;"> COPY </span> | <span style="font-size:75%;"> Behaves like both COPYIN and COPYOUT </span> |
| <span style="font-size:75%;"> CREATE </span> | <span style="font-size:75%;"> Check if variables are already present on device; if not create empty space </span> |
| <span style="font-size:75%;"> PRESENT </span> | <span style="font-size:75%;"> Assume that variables are already present on device </span> |

<span style="font-size:75%;"> Remark: Scalars are first private by default, they rarely appear in a data clause </span>

# Update clauses

Within data regions host and device memory can be updated with the <span style="color: green;"> `UPDATE` </span> clause

<br>

| <span style="font-size:75%;"> Clause </span> | <span style="font-size:75%;"> Behavior </span> |
| - | ------ |
| <span style="font-size:75%;"> UPDATE DEVICE </span> | <span style="font-size:75%;"> Copy data from host to device </span> |
| <span style="font-size:75%;"> UPDATE SELF </span> <br> <span style="font-size:75%;"> UPDATE HOST </span> | <span style="font-size:75%;"> Copy data from device to host </span> |

# Optimization: Device data management

<div class=column style=width:32%>

```Fortran
PROGRAM p1B
! [variable declarations]
   !$ACC DATA CREATE(a)

   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = i
   end do
   !$ACC END PARALLEL
   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = 2*a(i)
   end do
   !$ACC END PARALLEL
   s = 0.
```
</div>

<div class=column style=width:65%>

```Fortran
   !$ACC PARALLEL
   !$ACC LOOP REDUCTION(+ : s)
   do i = 1 , n
      s = s + a(i)
   end do
   !$ACC END PARALLEL
   !$ACC END DATA
   print * , s
END PROGRAM
```
* In order to avoid excessive data movement between the CPU and the GPU the programmer can manually allocate and transfer data on the GPU. Data can than be kept on the GPU and reused between different kernels

</div>

# Optimization: Device data management

* Syntax: 
<br>
<span style="color: green;">`!$ACC DATA [data_clause]`</span>
<br>
<span style="color: green;">`!$ACC END DATA`</span>

* data_clause: e.g. <span style="color: green;"> `CREATE` </span>, <span style="color: green;"> `COPYIN` </span>

# Optimization: Device data management

<div class=column style=width:48%>
```
> NVCOMPILER_ACC_TIME=1 ./p1A
Accelerator Kernel Timing data
/work/k20200/k202137/ACC_examples/p1A.f90
  p1a  NVIDIA  devicenum=0
    time(us): 76
```
</div>

<div class=column style=width:48%>
```
> NVCOMPILER_ACC_TIME=1 ./p1B
Accelerator Kernel Timing data
/work/k20200/k202137/ACC_examples/p1B.f90
  p1b  NVIDIA  devicenum=0
    time(us): 33
```
</div>

# Optimization: Device data management

```
> NVCOMPILER_ACC_TIME=1 ./p1B
Accelerator Kernel Timing data
/work/k20200/k202137/ACC_examples/p1B.f90
  p1b  NVIDIA  devicenum=0
    time(us): 33
    8: data region reached 2 times
    9: compute region reached 1 time
        9: kernel launched 1 time
            grid: [8]  block: [128]
            elapsed time(us): total=30 max=30 min=30 avg=30
    15: compute region reached 1 time
        15: kernel launched 1 time
            grid: [8]  block: [128]
            elapsed time(us): total=19 max=19 min=19 avg=19
    22: compute region reached 1 time
        22: kernel launched 1 time
            grid: [8]  block: [128]
            elapsed time(us): total=28 max=28 min=28 avg=28
        22: reduction kernel launched 1 time
            grid: [1]  block: [256]
            elapsed time(us): total=19 max=19 min=19 avg=19
    22: data region reached 2 times
        22: data copyin transfers: 1
             device time(us): total=5 max=5 min=5 avg=5
        26: data copyout transfers: 1
             device time(us): total=28 max=28 min=28 avg=28
```

# Explicit data transfer

<div class=column style=width:30%>

```Fortran
PROGRAM p2
! [variable declarations]
! Initialization on CPU
! e.g. read from Grib
   do i = 1 , n
      a(i) = i
   end do
   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = 2*a(i)
   end do
   !$ACC END PARALLEL
   s = 0.
```
</div>

<div class=column style=width:68%>

```Fortran
   !$ACC PARALLEL
   !$ACC LOOP REDUCTION(+ : s)
   do i = 1 , n
      s = s + a(i)
   end do
   !$ACC END PARALLEL
   print * , s
END PROGRAM 
```

* This example gives the correct result. How does it work?

</div>

# Explicit data transfer

<div class=column style=width:30%>

```Fortran
PROGRAM p2
! [variable declarations]
! Initialization on CPU
! e.g. read from Grib
   do i = 1 , n
      a(i) = i
   end do
   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = 2*a(i)
   end do
   !$ACC END PARALLEL
   s = 0.
```
</div>

<div class=column style=width:68%>

```Fortran
   !$ACC PARALLEL
   !$ACC LOOP REDUCTION(+ : s)
   do i = 1 , n
      s = s + a(i)
   end do
   !$ACC END PARALLEL
   print * , s
END PROGRAM
```

* This example gives the correct result. How does it work?
* How can we optimize the data management in the two remaining kernels?

</div>

# Explicit data transfer version A

<div class=column style=width:30%>

```Fortran
PROGRAM p2A
! [variable declarations]
! Initialization on CPU
! e.g. read from Grib
   do i = 1 , n
      a(i) = i
   end do
   !$ACC DATA CREATE(a)
   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = 2*a(i)
   end do      
   !$ACC END PARALLEL
   s = 0.
```
</div>

<div class=column style=width:68%>

```Fortran
   !$ACC PARALLEL
   !$ACC LOOP REDUCTION(+ : s)
   do i = 1 , n
      s = s + a(i)
   end do
   !$ACC END PARALLEL
   print * , s
END PROGRAM
```

* How can we optimize the data management in the two remaining kernels?
  * Try <span style="color: green;"> `!$ACC DATA CREATE(a)` </span>
</div>

# Explicit data transfer version A

<div class=column style=width:30%>

```Fortran
PROGRAM p2A
! [variable declarations]
! Initialization on CPU
! e.g. read from Grib
   do i = 1 , n
      a(i) = i
   end do
   !$ACC DATA CREATE(a)
   !$ACC UPDATE DEVICE(a)
   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = 2*a(i)
   end do
   !$ACC END PARALLEL
   s = 0.
```
</div>

<div class=column style=width:68%>

```Fortran
   !$ACC PARALLEL
   !$ACC LOOP REDUCTION(+ : s)
   do i = 1 , n
      s = s + a(i)
   end do
   !$ACC END PARALLEL
   print * , s
END PROGRAM
```

* How can we optimize the data management in the two remaining kernels?
  * Try <span style="color: green;"> `!$ACC DATA CREATE(a)` </span>
    * Only CREATE before 2nd DO leads to wrong result due to outdated data in a
  * Solution: <span style="color: green;"> `!$ACC UPDATE DEVICE(a)` </span>
</div>

# Explicit data transfer version B

<div class=column style=width:30%>

```Fortran
PROGRAM p2B
! [variable declarations]
! Initialization on CPU
! e.g. read from Grib
   do i = 1 , n
      a(i) = i
   end do
   !$ACC DATA COPYIN(a)
   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = 2*a(i)
   end do
   !$ACC END PARALLEL
   s = 0.
```
</div>

<div class=column style=width:68%>

```Fortran
   !$ACC PARALLEL
   !$ACC LOOP REDUCTION(+ : s)
   do i = 1 , n
      s = s + a(i)
   end do
   !$ACC END PARALLEL
   print * , s
END PROGRAM
```

  * Alternative: <span style="color: green;"> `!$ACC DATA COPYIN(a)` </span>

</div>

# Explicit data transfer version does-not-work

<div class=column style=width:30%>

```Fortran
PROGRAM pX
! [variable declarations]
! Initialization on CPU
! e.g. read from Grib
   do i = 1 , n
      a(i) = i
   end do
   !$ACC DATA CREATE(a)
   !$ACC DATA COPYIN(a)
   !$ACC PARALLEL
   !$ACC LOOP
   do i = 1 , n
      a(i) = 2*a(i)
   end do
   !$ACC END PARALLEL
   s = 0.
```
</div>

<div class=column style=width:68%>

```Fortran
   !$ACC PARALLEL
   !$ACC LOOP REDUCTION(+ : s)
   do i = 1 , n
      s = s + a(i)
   end do
   !$ACC END PARALLEL
   print * , s
END PROGRAM
```

  * Attention: COPYIN after CREATE does not work!!

</div>

# Unstructured data constructs

* Data regions are not feasible in all cases
  * (Pseudo) Object Oriented code
  * Data management across multiple files
* <span style="color: green;"> ENTER DATA </span> and <span style="color: green;"> EXIT DATA </span> directives can be used to manage unstructured data
regions
  * e.g. <span style="color: green;"> !$ACC ENTER DATA CREATE(a) </span>
* Additional data clause <span style="color: green;"> DELETE </span>
  * <span style="color: green;"> !$ACC EXIT DATA DELETE(var) </span>
  * Deallocates var on GPU

# Example enter exit data

<div class=column style=width:48%>
```Fortran
PROGRAM p3_enter_data
! [variable declarations]
   
   call construct (a, n)
   call assign (a)
   call vec_scalar_mult (a, 2.0)
   
   s = sum_vec (a)
   print *, s
   
   call destruct (a)

END PROGRAM
```
</div>

<div class=column style=width:48%>
```Fortran
MODULE mo_vec
...
   type t_vec
      integer :: n
      real, pointer :: x(:)
   end type t_vec

   subroutine construct (a, n)
   [… variable declaration… ]
      !$ACC ENTER DATA CREATE(a)
      a% n = n
      !$ACC UPDATE DEVICE(a% n)
      allocate (a% x(n))
      !$ACC ENTER DATA CREATE(a% x)
   end subroutine construct
```
</div>

# Example enter exit data

<div class=column style=width:48%>
```Fortran
PROGRAM p3_enter_data
! [variable declarations]
   
   call construct (a, n)
   call assign (a)
   call vec_scalar_mult (a, 2.0)
   
   s = sum_vec (a)
   print *, s
   
   call destruct (a)

END PROGRAM
```
</div>

<div class=column style=width:48%>
```Fortran
... [continue mo_vec] ...
   subroutine destruct (a)
      type(t_vec), intent(inout) :: a
      !$ACC EXIT DATA DELETE(a% x)
      deallocate (a% x)
      !$ACC EXIT DATA DELETE(a)
   end subroutine destruct
```
</div>

# Example enter exit data

<div class=column style=width:48%>
```Fortran
PROGRAM p3_enter_data
! [variable declarations]
 
   call construct (a, n)
   call assign (a)
   call vec_scalar_mult (a, 2.0)

   s = sum_vec (a)
   print *, s

   call destruct (a)   

END PROGRAM
```
</div>

<div class=column style=width:48%>
```Fortran 
... [continue mo_vec] ...
   subroutine assign (a)
      type(t_vec),intent(inout) :: a
      integer :: i
      !$ACC PARALLEL LOOP DEFAULT(present)
      do i = 1, a% n
         a% x(i) = real(i)
      end do
      !$ACC END LOOP
   end subroutine assign
```
</div>

# Example enter exit data

<div class=column style=width:48%>
```Fortran
PROGRAM p3_enter_data
! [variable declarations]

   call construct (a, n)
   call assign (a)
   call vec_scalar_mult (a, 2.0)

   s = sum_vec (a)
   print *, s

   call destruct (a) 

END PROGRAM
```
</div>

<div class=column style=width:48%>
```Fortran
... [continue mo_vec] ...
   subroutine vec_scalar_mult (a, s)
      type(t_vec),intent(inout) :: a
      real ,intent(in) :: s
      integer :: i
      !$ACC PARALLEL LOOP DEFAULT(present)
      do i = 1, a% n
         a% x(i) = s * a% x(i)
      end do
      !$ACC END PARALLEL LOOP
   end subroutine vec_scalar_mult
...
END MODULE
```
</div>

# PRESENT and DEFAULT clauses

* With a <span style="color: green;"> PRESENT(vars) </span> clause we can signal the compiler that a variable is already present on the device and no data movement is necessary
* Instead of providing a list of all variables we can set <span style="color: green;"> DEFAULT(PRESENT) </span>
* <span style="color: green;"> DEFAULT(PRESENT) </span> helps to detect undesired data transfers
* If <span style="color: green;"> DEFAULT(PRESENT) </span> is given, but variable is not on device -> program is terminating with an error
* always use <span style="color: green;"> DEFAULT(PRESENT) </span> (no data PRESENT() required)
* <span style="font-size:75%;"> Also in the standard: DEFAULT(NONE): all variables must be declared explicitly (e.g. PRESENT(), PRIVATE(), FIRSTPRIVATE(), COPYIN|OUT(), …). “all” includes scalars. DEFAULT(NONE) use is advised against. </span>

# IF clause

<div class=column style=width:58%>

* Activate and deactivate OpenACC code via
  * IF clause in OpenACC pragma
  * Preprocessor variable _OPENACC
* <span style="color: green;"> IF(condition) </span>: if condition evaluates to .TRUE. execute kernel on GPU, otherwise execute on CPU.
* Preprocessor macro _OPENACC holds version information of OpenACC
  * Not defined if acc is not active

</div>

<div class=column style=width:40%>

```Fortran
logical :: lacc
!$ACC PARALLEL LOOP IF(lacc)
do i = 1, n
   a(i) = i
end do
!$ACC END PARALLEL
#ifdef _OPENACC
   ! some post processing for GPU
#endif
```

</div>

# Kernels construct

<div class=column style=width:68%>

* Acc <span style="color: green;"> KERNELS</span> are similar to <span style="color: green;"> PARALLEL</span> regions
* A kernels region gives the compiler maximum freedom of the parallel implementation
* Might be better or much worse than manual parallelization
* For ICON we don’t recommend the use of kernels
* Implicit Fortran loops are possible in kernels, but might be slower than explicit loops in a parallel region (try example p_kernels)

</div>

<div class=column style=width:30%>

```Fortran
!$ACC KERNELS
do i = 1, n
   a(i) = 2.0 * b(i)
end do
!$ACC END KERNELS
```
```Fortran
!$ACC KERNELS
a(:) = 0.0
!$ACC END KERNELS
!$ACC PARALLEL LOOP
do i = 1, n
   a(i) = 0.0
end do
!$ACC END PARALLEL
```

</div>

# Levels of parallelism

* <span style="color: green;"> GANG</span> clause
  * Independent parallelism
  * Threads in a gang can operate on same memory
* <span style="color: green;"> WORKER</span> clause
  * Group of threads that can operate vector instructions
* <span style="color: green;"> VECTOR</span> clause
  * Threads that work in SIMT (SIMD) fashion
  * Operate on same cache
* <span style="color: green;"> SEQ</span> clause
  * No parallelism
* The optimal choice of parallelism can be hardware dependent

# Hands-on exercises II

# Shared memory

* Parallel execution can lead to potentially incorrect results
* Different threads may access shared variables in unpredictable ways
  * Leading to so called race conditions
  * Solution: <span style="color: green;"> PRIVATE</span>, <span style="color: green;"> FIRSTPRIVATE</span>, <span style="color: green;"> REDUCTION</span> clauses

# Shared memory

* <span style="color: green;"> PRIVATE(var1, var2) </span>: copies of variables are created
  * Loop iterators are private by default
* <span style="color: green;"> FIRSTPRIVATE</span>: (only on <span style="color: green;"> PARALLEL</span> and <span style="color: green;"> SERIAL</span>)
  * A copy of the variable is created for each gang and initialized with the CPU value
  * Any scalar is first private by default if not specified otherwise (and not <span style="color: green;"> DEFAULT(NONE)</span>)
* <span style="color: green;"> REDUCTION</span>:
  * Each thread uses a private copy of the variable
  * At the end of the region the variable is reduced to a final result
  * <span style="color: green;"> REDUCTION(operator: variable)</span> with operators: +, *, min, max, .and., .or., …

# Debugging Tips

* Is the CPU version correct?
* Set environment variables:
  * NVCOMPILER_ACC_NOTIFY=31
    * 1: kernel launches
    * 2: data transfers
    * 4: region entry/exit
    * 8: wait operations or synchronizations with the device
    * 16: device memory allocates and deallocates
    * -> use sum for combinations
  * NVCOMPILER_ACC_SYNCHRONOUS=1 disables async
  * NVCOMPILER_ACC_TIME=1 Accelerator Kernel Timing data

# Debugging Tips

* Dump current GPU memory allocations
  * call acc_present_dump()
  * Available in openacc module (use openacc)
* Use compute-sanitizer

# Tool: compute-sanitizer

* compute-sanitizer is a functional correctness checking suite by Nvidia
* Can detect and track down (some) errors
<br>
  `> compute-sanitizer –tool <tool> ./my_program`
* `<tool>`:
  * memcheck : Memory access checking (default)
  * racecheck : Shared memory hazard checking
  * synccheck : Synchronization checking
  * initcheck : Global memory initialization checking

# Hands-on exercises III

# OpenACC - OpenMP

* Most OpenACC directives can be directly translated to OpenMP target directives:

| OpenACC | OpenMP |
| --- | --- |
| `!$ACC PARALLEL` | `!$OMP TARGET TEAMS DISTRIBUTE` |
| `!$ACC PARALLEL LOOP` | `!$OMP TARGET TEAMS DISTRIBUTE PARALLEL FOR` |
| `!$ACC DATA` | `!$OMP TARGET DATA` |
| `!$ACC DATA COPYIN` | `!$OMP TARGET DATA MAP(TO:)` |
| `!$ACC UPDATA HOST` | `!$OMP TARGET UPDATE FROM()` |

# Any questions or comments? 

# Further reading

* [OpenACC reference guide](https://www.openacc.org/sites/default/files/inline-files/API%20Guide%202.7.pdf)
* [OpenACC best practices guide](https://openacc-best-practices-guide.readthedocs.io/en/latest/)

