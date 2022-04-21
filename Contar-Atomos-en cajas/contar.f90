program contar

implicit none
integer, parameter                     :: rk = selected_real_kind(15,307)!real kind
integer, parameter                     :: MAXLINEA = 1000000
character(2)                           :: nombre
integer                                :: errorIO, i, j, nLinea, nat,nConf,izq,der,z,k
real(rk), dimension(:), allocatable    :: box
real(rk), dimension(:,:,:), allocatable :: config
integer,  dimension(:), allocatable    :: conteo
real, dimension(:), allocatable    :: y


open(unit=10, file = "tray10.xyz", status = 'old')

! determinar el número de línea

nLinea = 0

do i = 1, MAXLINEA
    read(10, *, iostat=errorIO)
    if(errorIO /= 0) exit
    if (nlinea > MAXLINEA) then
        write(*,*) "Error: superó el MAXLINEA"
        write(*,*) "Salir programa"
        stop
    end if
    nLinea = nLinea + 1
end do

write(*, *) nLinea
rewind(10)


read(10, *) nat
write(*,*) "El número de átomos es " ,nat

nConf= nLinea/(2+nat)

write(*,*) "El número de configuraciones es ", nConf


rewind(10)


allocate(config(3,nat,nConf))
allocate(box(3))
allocate(conteo(nat))
allocate(y(nat))
conteo=0
y=0

read(10,*)
read(10,*) box(1), box(2), box(3)
write(*,*)"Las medidias de la caja son ",  box(1), box(2), box(3)

izq=0
der=0
rewind(10)

do j=1, nConf

read (10,*)
 read(10,*)
   do i =1, nat
     read(10,*) nombre, config(1,i,j) , config(2,i,j), config(3,i,j)
!     write(*,*) config(1,i,j) , config(2,i,j), config(3,i,j)
        if (config(3,i,j) .le. box(3)/2) then
                izq=izq+1
            else
                der=der+1
        endif
   enddo


enddo

write(*,*) "izq es ", izq
write(*,*) "der es ", der





!        do k=1,nat
!        if (k.eq.izq) then
!               y(k)=y(k)+1
!        endif
        !conteo(int(y(i)))=conteo(int(y(i))) + 1
!       write(*,*) k, izq
!        enddo

!write(*,*) "el numero de atomos a la izquierda es:", izq
!write(*,*) "el numero de atomos a la derecha es:", der



!write(*,*) conteo

end program
