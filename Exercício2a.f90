program ondas
implicit none
integer, parameter :: nx = 100 , nt=600
integer L, Te, p , tmax
integer x, t, a, k
real*8 dx, dt, v, a0, x0, y(nx,nt),h, sigma
!abrindo o arquivo
character*64 fname
fname=('onda2a.dat')
open(15,file=fname,status='unknown')
!Parƒmetros
L=1.
dx=0.01
dt=0.01
Te=1.
p=1.
tmax=6.
!Condi‡äes Iniciais
a0=1d-3
x0=0.2
v=(Te/p)**0.5
sigma=1d-3
h=0.
!Condi‡äes de contorno
do x=1,100
h=x*dx
y(x,1) = a0*exp(-((h-x0)**2)/sigma)
y(x,2) = a0*exp(-((h-dt-x0)**2)/sigma)
print*, y(x,2)
end do
y(1,:)=0.
y(100,:)=0.
!Programa
   do t=2,nt-1
   do x=2,nx-1
   y(x,t+1)=(((dt*v)**2)*(((y(x+1,t)-2*y(x,t)+y(x-1,t)))/(dx)**2)+2*y(x,t)-y(x,t-1))
   end do
   end do
!plotando
do x=1,100
do t=1,600
write(15,*) dfloat(x)*dfloat(dx) , dfloat(t)*dfloat(dt) , y(x,t)
end do
write(15,*)
enddo
close(15)
pause
end program
