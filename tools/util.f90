
      subroutine rot_mat(jot)   ! 2*pi*dcos(theta)*d^J_{i,j}
      use input4
      implicit none

      integer :: jot
      integer :: ng
      integer :: ier1,ier3
      real(KIND(0.0d0)) :: beta,dr1

      do ng = 1,nt
         beta = dacos ( xg ( ng ) )
         call dmatrx(beta,jot, 3, 3,dr1,ier1)   ! Edmonds convention
         djw(ng,3,3) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, 3, 1,dr1,ier1)
         djw(ng,3,1) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, 3,-1,dr1,ier1)
         djw(ng,3,-1) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, 1, 3,dr1,ier1)
         djw(ng,1,3) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, 1, 1,dr1,ier1)
         djw(ng,1,1) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, 1,-1,dr1,ier1)
         djw(ng,1,-1) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, -1, 3,dr1,ier1)
         djw(ng,-1,3) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, -1, 1,dr1,ier1)
         djw(ng,-1,1) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, -1,-1,dr1,ier1)
         djw(ng,-1,-1) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot,-3,3,dr1,ier1)
         djw(ng,-3,3) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot, -3, 1,dr1,ier1)
         djw(ng,-3,1) = dr1*wg(ng)*tpi
         call dmatrx(beta,jot,-3,-1,dr1,ier1)
         djw(ng,-3,-1) = dr1*wg(ng)*tpi
      end do

      return
      end subroutine rot_mat

!=======================================================================!

!c maximum number of quadrature points
!      integer maxq
!      parameter (maxq = 96)

!c maximal dimension of matrix A, Ainv
!      integer maxdim
!      parameter (maxdim = 2 * (maxq + 1))
!      integer wspace
!      parameter (wspace = maxdim*maxdim + 4*maxdim)
!      real*8 error
!      complex*16 work(wspace)

!c actual size of the matrices A, Ainv
!      integer ndi


!----------------------------------------------------------------------
!c     invert from unilib
!         call invert (a, ndi, maxdim, ainv, maxdim, work, .true., error)


      subroutine invert(a, n, np, ainv, npi, work, refine, error)

!  dummy variables
      integer :: n, np, npi
      complex(kind(0.0d0)) :: a(np, np), ainv(npi, npi), work(*)
      real(kind(0.0d0)) :: error
      logical :: refine

!  ----------------------------------------------------------------------
!  Inverts a complex ceneral matrix using lneqir and lneqnb.
!  On exit ainv contains the inverse of a
!  If A-1 = (x1 ... xN) and I = (i1 ... iN) then solve  Axn = in, n = 1, ... N
!  Iterative refinement is done based on the value of refine.
!  If iterative refinement is performed, error is set to the absolute
!  value of the largest correction.
!  work should be at least n**2 + 4*n  long if iterative refinement
!  is used, at least 2*n if not
!  For use of work array see lneqir.
!  ----------------------------------------------------------------------

!  variables
      integer :: i, j
      real(kind(0.0d0)) :: err

!  subroutines
!      external lneqnb, lneqir

!  intrinsics
!      intrinsic max

!  begin { invert } ++++++++++++++++++++++++++++++++++++++++++++++++++
      if (npi .lt. n) then
         write (*, *) 'Insufficient space for inverse in invert'
         call exit(1)
      else
         do 10 i = 1, n
            do 20 j = 1, n
               ainv(i, j) = 0.d0
 20         continue
            ainv(i, i) = 1.d0
 10      continue

         call lneqir (a, n, np, ainv(1, 1), work, refine, error)
         do 30 i = 2, n
            call lneqnb (a, n, np, ainv(1, i), work, refine, err)
            error = dmax1(err, error)
 30      continue
      end if

      return
      end subroutine invert

      
!========================================================================
      subroutine lneqir (a, n, np, b, work, refine, error)

!  dummy variables
      integer :: n, np
      complex(kind(0.0d0)) :: a(np, np), b(n), work(*)
      real(kind(0.0d0)) :: error
      logical :: refine

!  lneqir = LiNear EQuation solver with Iterative Refinement.
!  Given matrix a, vector b, this solves ax = b, placing the result in b.
!  Iterative refinement is done based on the value of refine.
!  If iterative refinement is performed, error is set to the absolute
!  value of the largest correction.
!  work should be at least n**2 + 4*n  long if iterative refinement
!  is used, at least 2*n if not
!  work is used as follows:
!             work(1...n):  indx - permutation index vector (integer part)
!                                  used by ludcmp, lubksb
!          work(n+1...2n):  scale - scaling vector (real part)
!                                   used by ludcmp
!         work(2n+1...3n):  r - correction vector
!                               used by mprove
!         work(3n+1...4n):  b - a copy of b
!                               used by mprove
!    work(4n+1...n**2+4n):  a - a copy of a
!                               used by mprove
!  These vectors are accessed by statement functions

!  variables
      integer :: i, j
      real(kind(0.0d0)) :: d

!  statement functions
      integer :: ir, ib, ia
      ir(i) = 2*n + i
      ib(i) = 3*n + i
      ia(i, j) = 4*n + i + (j - 1) * n

!  begin { lneqir } ++++++++++++++++++++++++++++++++++++++++++++++++++
      if (refine) then
         do 10 i = 1, n
            do 20 j = 1, n
               work(ia(i, j)) = a(i, j)
 20         continue
            work(ib(i)) = b(i)
 10      continue
      end if
      call ludcmp (a, n, np, work, d)
      call lubksb (a, n, np, b, work)
      if (refine) then
         call mprove (a, n, np, b, work)
         error = abs(work(ir(1)))
         do 30 i = 2, n
            error = dmax1(error, abs(work(ir(i))))
 30      continue
      end if

      return
      end subroutine lneqir

!========================================================================

      subroutine lneqnb (a, n, np, b, work, refine, error)

!  dummy variables
      integer :: n, np
      complex(kind(0.0d0)) :: a(np, np), b(n), work(*)
      real(kind(0.0d0)) :: error
      logical :: refine

!  lneqnb = LiNear EQuation solver with New B.
!  Providing a and work have not been changed since a previous call to
!  lneqir, this solves ax = b with a new right hand side, b.  Refinement
!  can only be requested if it was in the initial call to lneqir.  See
!  lneqir for a description of work usage.
!  If iterative refinement is performed, error is set to the absolute
!  value of the largest correction.
!  work should be at least n**2 + 4*n  long if iterative refinement
!  is used, at least 2*n if not

!  variables
      integer :: i

!  statement functions
      integer :: ir, ib
      ir(i) = 2*n + i
      ib(i) = 3*n + i

!  begin { lneqnb } ++++++++++++++++++++++++++++++++++++++++++++++++++
      if (refine) then
         do 10 i = 1, n
            work(ib(i)) = b(i)
 10      continue
      end if
      call lubksb (a, n, np, b, work)
      if (refine) then
         call mprove (a, n, np, b, work)
         error = abs(work(ir(1)))
         do 30 i = 2, n
            error = dmax1(error, abs(work(ir(i))))
 30      continue
      end if

      return
      end subroutine lneqnb

!  ============================================================

      subroutine lubksb (a, n, np, b, work)

!  dummy variables
      integer :: n, np
      complex(kind(0.0d0)) :: a(np, np), b(n), work(n**2 + 4*n)

!  Solves ax = b given b and an LU decomposed matrix a.  ie, a and indx
!  are the output from ludcmp.  a, n, np and indx are not altered by
!  this routine so can solve several rhs's.  Allows for b possibly
!  begining with several zero elements => useful in matrix inversion.
!  indx has been incorporated into work, accessed by ii.
!  Uses first n elements of work only.

!  constants
      real(kind(0.0d0)),parameter :: tiny = 1.0d-20

!  variables
      integer :: j, inz, i, ll
      complex(kind(0.0d0)) :: sum

!  intrinsics
!      intrinsic nint, abs

!  statement functions
      integer :: ii
      ii(i) = i

!  begin { lubksb } ++++++++++++++++++++++++++++++++++++++++++++++
!  inz will be 0 until encounter non zero b then set to that element
      inz = 0
      do 10 i = 1, n
         ll = int(real(work(ii(i))))
         sum = b(ll)
         b(ll) = b(i)
         if (inz .ne. 0) then
            do 20 j = inz, i - 1
               sum = sum - a(i, j) * b(j)
20          continue
         else if (abs(sum) .gt. tiny) then
            inz = i
         end if
         b(i) = sum
10    continue

!  now do back substitution
      do 30 i = n, 1, -1
         sum = b(i)
         if (i .lt. n) then
            do 40 j = i + 1, n
               sum = sum - a(i, j) * b(j)
40          continue
         end if
         b(i) = sum / a(i, i)
30    continue

      return
      end subroutine lubksb

!  ===========================================================================

      subroutine ludcmp (a, n, np, work, d)

!  dummy variables
      integer :: n, np
      complex(kind(0.0d0)) :: a(np, np), work(*)
      real(kind(0.0d0)) :: d

!  Given an n x n matrix a with physical dimension np, this routine
!  replaces it by the LU decomposition of a rowwise permutation of
!  itself.  a and n are input. a is output in upper and lower diagonal
!  form; indx is an output vector which records the row permutation
!  effected by the partial pivoting; scale is essentially a work array
!  but indicates the scaling used for each row.
!  d will be +1 if even number of permutations, -1 if odd - used for
!  calculating determinant
!  indx and scale have been incorporated into work.  They are accessed
!  by functions ii and is respectively.
!  uses first 2n elements of work only

!  constants
      real(kind(0.0d0)),parameter :: tiny = 1.0d-20

!  variables
      integer :: i, j, k, imax
      real(kind(0.0d0)) :: aamax, dum
      complex(kind(0.0d0)) :: cdum, sum

!  intrinsics
!      intrinsic abs, dble

!  statement functions
      integer :: ii, is
      ii(i) = i
      is(i) = n + i

!  begin { ludcmp } ++++++++++++++++++++++++++++++++++++++++++++++++++
      d = 1.d0

!  loop over rows to get scaling
      do 10 i = 1, n
         aamax = 0.d0
         do 20 j = 1, n
            if (abs(a(i,j)) .gt. aamax) aamax = abs(a(i,j))
20       continue
         if (aamax .lt. tiny) then
            write(kwirte,*) 'Singular matrix'
            stop
         end if
         work(is(i)) = 1.d0 / aamax
10    continue

!  loop over columns of Crout's method
      do 30 j = 1, n
         if (j .gt. 1) then
            do 40 i = 1, j - 1
               sum = a(i, j)
               if (i .gt. 1) then
                  do 50 k = 1, i - 1
                     sum = sum - a(i, k) * a(k, j)
50                continue
                  a(i, j) = sum
               end if
40          continue
         end if

         aamax = 0.d0
         do 60 i = j, n
            sum = a(i, j)
            if (j .gt. 1) then
               do 70 k = 1, j - 1
                  sum = sum - a(i, k) * a(k, j)
70             continue
               a(i, j) = sum
            end if

!  is this the best pivot so far?
            dum = dble(work(is(i))) * abs(sum)
            if (dum .ge. aamax) then
               imax = i
               aamax = dum
            end if
60       continue

!  do we need to interchange rows?
         if (j .ne. imax) then
            do 80 k = 1, n
               cdum = a(imax, k)
               a(imax, k) = a(j, k)
               a(j, k) = cdum
80          continue
            d = -d
            work(is(imax)) = work(is(j))
         end if
         work(ii(j)) = imax

!  now divide by pivot element
         if (j .ne. n) then
            if (abs(a(j, j)) .lt. tiny) then
               cdum = 1.d0 / tiny
            else
               cdum = 1.d0 / a(j, j)
            end if
            do 90 i = j + 1, n
               a(i, j) = a(i, j) * cdum
90          continue
         end if
30    continue
      if (abs(a(n, n)) .lt. tiny) a(n, n) = tiny

      return
      end subroutine ludcmp

!  ===========================================================================
      subroutine mprove (alud, n, np, x, work)

!  dummy variables
      integer :: n, np
      complex(kind(0.0d0)) :: alud(np, np), x(n), work(*)

!  Improves a solution vector x of the set of equations ax = b. a, b, x
!  and alud (LU decompsed form of a) are input.  On output x is modified
!  to reflect corrections and r is a vector containing the corrections.
!  a, b and r have been incorporated into work, accessed by ia, ib and ir.
!  Uses all n**2 + 4n elements of work.

!  variables
      integer :: i, j
      complex(kind(0.0d0)) :: sum, temp
!  use double precision for sum if possible

!  statement funtions
      integer :: ir, ib, ia
      ir(i) = 2*n + i
      ib(i) = 3*n + i
      ia(i, j) = 4*n + i + (j - 1) * n

!  begin { mprove } +++++++++++++++++++++++++++++++++++++++++++++++
      do 10 i = 1, n
         sum = - work(ib(i))
         do 20 j = 1, n
!  next line multi-precision if possible
            sum = sum + work(ia(i, j)) * x(j)
20       continue
         work(ir(i)) = sum
10    continue

!  need a vector in call to lubksb to solve for r
!  so swap contents of x and r
      do 25 i = 1, n
         temp = x(i)
         x(i) = work(ir(i))
         work(ir(i)) = temp
 25   continue

!  solve for error term
      call lubksb(alud, n, np, x, work)

!  and subtract it from old solution, swapping x and r first
      do 30 i = 1, n
         temp = x(i)
         x(i) = work(ir(i))
         work(ir(i)) = temp
         x(i) = x(i) - work(ir(i))
30    continue

      return
      end subroutine mprove
