program test_biseccion
! el algoritmo de bisección
! el intervalo que contiene sola una raíz
! se proporciona por el usuario

implicit none
real, parameter    :: umbral = 1.0e-5
integer, parameter :: maxiter = 40
real               :: vini, vfinal, raiz, fraiz
integer            :: no_paso

vini = 1.
vfinal = 5.

call biseccion (vini, vfinal, umbral, maxiter, raiz, fraiz, no_paso)

write (*,*) raiz, fraiz, no_paso

end program ! test_biseccion

subroutine biseccion (a, b, small, imax, c, fc, iter)
real               :: a, b, c, fa, fb, fc, small
integer            :: iter, imax


do iter = 1, imax

   fa = funcion(a)
   fb = funcion(b)

!  write (*,*) "f(a)*f(b) = ", fa*fb

   c = (b+a)/2.0
   fc = funcion(c)
!  write (*,*) "c y f(c) son ", c, fc

   if (abs(fc) < small) then
!     write (*,*) "La raíz es: ", c
!     write (*,*) "En el paso ", iter
      exit
   end if

!  write (*,*) "f(a)*f(c) = ", fa*fc
!  write (*,*) "f(b)*f(c) = ", fb*fc

   if (fa*fc < 0.) then
      b = c
!     write (*,*) "La raíz está en [a,c]"
   else
      a = c
!     write (*,*) "La raíz está en [c,b]"
   end if

end do ! i = 1, 10
end subroutine ! biseccion

function funcion (x)
implicit none

real              :: x, funcion

funcion = x**3 + 2.90*x*x - 16.296*x - 29.183
end function funcion
