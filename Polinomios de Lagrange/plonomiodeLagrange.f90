module mod_subroutine

Interface

subroutine lg(renglon,columna,nlinea,i,j,fitdat,numerador,denominador)

use numerico

implicit none

integer                                       :: x

integer, intent(in)                           :: nlinea, renglon, columna

integer, intent(inout)                        :: i, j

real(dbl), intent(in)                         :: fitdat(nlinea,2)

real(dbl), intent(inout)                      :: numerador, denominador

real(dbl)                                     :: s

end subroutine

end interface

end module


!======================================================================================


program lagrange

!polinomio de lagrange, 17 jul

use numerico

!use mod_subroutine

implicit none

integer                                       :: ren, nlin, col, a, b,x

real(dbl), allocatable                        :: fit(:,:)

real(dbl)                                     :: num, den, s

     open(unit=10, file='lagrange1.dat', status='old')

         read(10,*) nlin

         write(*,*) nlin

         !colocar la memoria para fitdat

            allocate ( fit ( nlin,2 ) )

         !leer los datos de fitdat

                do ren = 1, nlin

                   read(10,*) ( fit ( ren , col), col = 1,2 )

                end do ! renglon = 1, nlinea

                write(*,*)

                do ren = 1, nlin

                   write(*,100) ( fit ( ren , col ), col = 1,2)

                end do ! renglon =1, nlinea

                              call lg ( ren, col, nlin, a, b, fit, num, den)

       !formato

                   100 format( *( f13.6) )

       !limpiar

      close(10)

            deallocate(fit)

!end program

!============================================================================================
contains
subroutine lg(renglon,columna,nlinea,i,j,fitdat,numerador,denominador)

use numerico

implicit none

integer                                       :: x

integer, intent(in)                           :: nlinea, renglon, columna

integer, intent(inout)                        :: i, j

real(dbl), intent(in)                         :: fitdat(nlinea,2)

real(dbl), intent(inout)                      :: numerador, denominador

real(dbl)                                     :: s

!aplicando el polinomio de lagrange

x = 2.0_dbl

s = 0._dbl

 do i = 1, nlinea

       numerador = 1._dbl

       denominador = 1._dbl

    do j = 1, nlinea

           if ( j == i ) cycle

               numerador = numerador * ( x - fitdat(j,1))

               denominador = denominador * ( fitdat(i,1) - fitdat(j,1) )

               write(*,*)"numerador y denominador", numerador, denominador

    end do ! j = 1, 2

               s = s + ( ( numerador / denominador ) * fitdat(i,2) )

               write(*,*) "S es igual a",s

 end do ! i = 1, 4

end subroutine
end program
