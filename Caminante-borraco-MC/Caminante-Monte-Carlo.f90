module datos
  implicit none

  integer, parameter :: rk = selected_real_kind(16,170)!real kind
  integer, parameter :: ik = selected_int_kind(8)!integer kind
  integer, parameter :: ark = selected_real_kind(14,17)!high-precision real

Contains

       subroutine seed_cal(idum)
       integer(ik)   :: timeArray(3)
       integer(ik),dimension(8)::value
       integer(ik)::idum

     !Seed calculation
     idum=-99
     call date_and_time(values=value)
     timeArray(1)=value(6)
     timeArray(2)=value(7)
     timeArray(3)=value(8)

       idum = -((timeArray(1)+1)*(timeArray(2)+1)*(timeArray(3)+1))
       idum = -INT(1.d3*ran2(idum)*timeArray(1)+1.d3*ran2(idum)&
              *timeArray(2)+1.d2*ran2(idum)*timeArray(3))

       return
       end subroutine seed_cal


       FUNCTION ran2(idum)
       integer,intent(inout)::idum
       integer(ik) IM1,IM2,IMM1,IA1,IA2,IQ1,IQ2,IR1,IR2,NTAB,NDIV
       Real (rk) ran2,AM,EPS,RNMX
       PARAMETER (IM1=2147483563,IM2=2147483399,AM=1._rk/IM1,IMM1=IM1-1,&
      &IA1=40014,IA2=40692,IQ1=53668,IQ2=52774,IR1=12211,IR2=3791,&
      &NTAB=32,NDIV=1+IMM1/NTAB,EPS=1.2e-7_rk,RNMX=1._rk-EPS)
       integer(ik) idum2,j,k,iv(NTAB),iy
       SAVE iv,iy,idum2
       DATA idum2/123456789/, iv/NTAB*0/, iy/0/
       if (idum.le.0) then
         idum=max(-idum,1)
         idum2=idum
         do 11 j=NTAB+8,1,-1

           k=idum/IQ1
           idum=IA1*(idum-k*IQ1)-k*IR1
           if (idum.lt.0) idum=idum+IM1
           if (j.le.NTAB) iv(j)=idum
 11      continue
         iy=iv(1)
       endif
       k=idum/IQ1
       idum=IA1*(idum-k*IQ1)-k*IR1
       if (idum.lt.0) idum=idum+IM1
       k=idum2/IQ2
       idum2=IA2*(idum2-k*IQ2)-k*IR2
       if (idum2.lt.0) idum2=idum2+IM2
       j=1+iy/NDIV
       iy=iv(j)-idum2
       iv(j)=idum
       if(iy.lt.1)iy=iy+IMM1
       ran2=min(AM*iy,RNMX)
       return
       END FUNCTION ran2

end module datos

program pasos
use datos
!

implicit none

real(rk) :: p, q , proba,probb,probt,fN, fnp,fnq,Wn,Promp,Promq
integer, parameter :: l=1
integer  :: izq, der, nexp, i, j, idum, m, np, nq,N
integer, parameter :: npasos=100

call seed_cal(idum)

nexp=1
izq=0
der=0
p=0.5_rk
np=0
nq=0
N=0
do j = 1, nexp

  do i = 1, npasos

    if (ran2(idum).le.p) then
       izq=izq+1
       nq=nq+1
    else
       der=der+1
       np=np+1
    end if

  enddo

p=dfloat(der)/dfloat(npasos)
q=1._rk - p
m=2*der-npasos
enddo
N=np+nq           !Otro contador para N pasos
proba=(p**np)
probb=(q**nq)
probt=proba*probb
fN=0.9189+(REAL(N)+0.5)*LOG(REAL(N))-REAL(N) !Formula de Sterling para N pasos
fnp=0.9189+(REAL(np)+0.5)*LOG(REAL(np))-REAL(np) !Formula de Sterling para n1 pasoa a la derecha
fnq=0.9189+(REAL(nq)+0.5)*LOG(REAL(nq))-REAL(nq) !Formula de Sterling para n2 pasos a la izquierda
Wn=((fN)/(fnp*fnq))*probt  !Probabilidad de Wn1


write(*,*) 'Programa del Borracho o el caminante al azar utilizando tecnicas de Monte carlo'
write(*,*) 'El numero de pasos es ', npasos
if (m.lt.0) then
   write(*,*) 'la distancia es ', abs(m*l),'pasos a la izquierda'
else if(m.gt.0)then
   write(*,*) 'la distancia es ', abs(m*l),'pasos a la derecha'
else if(m.eq.0) then
   write(*,*) 'el borracho no se movio'
end if

write(*,*) 'la probabilidad pasos a la derecha P es :',p
write(*,*) 'Pasos a la derecha :',np
write(*,*) 'la probabilidad pasos a la izquierda Q es :',q
write(*,*) 'Pasos a la izquierda :',nq
write(*,*) 'La distancia hacia el origen es: ', abs(m)*l
write(*,*)'Probabilidad W (n 1 ) es:',Wn

end program
