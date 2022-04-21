module numerico

!alonso, aqui defino la presicion doble:17 decimales
!y algunas constantes como pi, hbar ..... etc

implicit none

integer, parameter                      :: dbl=selected_real_kind(17)

real(dbl), parameter                    :: pi=3.141592654_dbl

end module !numerico
