program metriz


use numerico

implicit none

real(dbl), dimension(:,:), allocatable       :: a

integer                                      :: renglon,columna, tamano

real(dbl)                                      :: nrenglon, ncolumna, suma, fz

open ( unit=10, file= 'matriz.dat', status='old' )

   read( 10 , * ) tamano

        write(*,*) 'El tamaño de la matris es', tamano

   allocate( a ( tamano , tamano ) )

 do renglon = 1 , tamano

                do columna= 1 , tamano

                     read ( 10 , * ) a ( columna , renglon )

                end do ! j=1, nlinea

 end do !i = 1 , nlinea

                do renglon = 1 , tamano

                     write ( * ,100 ) ( a ( renglon , columna ) , columna = 1 , tamano )

                end do


!--------suma renglon

    write ( * , * ) 'la suma del primer renglon es:'

            fz=0

                     do renglon=1,tamano

                               fz=suma((a(renglon,1)),fz)

                     end do

                               write(*,100) fz

!--------suma columna

   write( * , * ) 'la suma de la primera columna es:'

           fz=0

                     do columna=1,tamano

                              fz=suma((a(1,columna)),fz)

                     end do !columna=1,tamano

                              write(*,100) fz
close ( 10 )

call mtp ( tamano , renglon , columna )

100 format ( 5 ( f11.5 ) )

end program

subroutine mtp ( nlinea , i , j )

use numerico

implicit none

real(dbl), dimension(:,:), allocatable       :: a

integer                                      :: i, j, fz, nlinea

open ( unit=10 , file= 'matriz.dat' , status = 'old' )

  read ( 10 , * ) nlinea

  write( * , * ) 'La matriz transpuesta es'

  allocate ( a ( nlinea , nlinea ) )

 do i = 1 , nlinea

                do j = 1 , nlinea

                     read ( 10 , * ) a ( j , i )

                end do ! j = 1, nlinea

 end do !i = 1 , nlinea

                do i = 1, nlinea

                     write ( * , 100 ) (a ( j , i ) , j = 1 , nlinea )

                end do

close ( 10 )

100 format ( 5 ( f11.5 ) )

end subroutine !mtp (nlinea , i , j )

function suma(a,b)

use numerico

implicit none

real(dbl) :: suma

real(dbl)   :: a,b

suma=a+b

end function !suma
