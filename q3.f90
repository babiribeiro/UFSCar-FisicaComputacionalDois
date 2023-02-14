program q3
implicit none
real*8 Lx, tmax, Dx, Dt, Yxx
real*8 Y(100, 600)
integer Nx, Nt, i, j
real*8 v, rho, Tr
real*8 a0, x0, sigma

!Velocidade
rho=1.
Tr=1.
v=sqrt(Tr/rho)

!informa‡äes
Nx=100
Nt=100
Lx=1
tmax=6
Dx=Lx/Nx
Dt=0.01
Nt=tmax/Dt

!
a0=1E-3
x0=0.5
sigma=1E-3
do i=1, Nx
Y(i,1)=a0*exp(-((i*Dx-x0)**2.)/sigma)
Y(i,2)=a0*exp(-(((i*Dx-v*Dt/2.)-x0)**2.)/sigma)
end do


!itera‡Æo
do j=3, Nt
do i=2, Nx
yxx=(Y(i+1, j-1)-2*Y(i, j-1)+Y(i-1, j-1))/(Dx**2)
Y(i,j)=(Dt**2.)*(V**2.)*yxx+2*Y(i,j-1)-Y(i, j-2)
end do
end do

!Escrever Resposta
open(22,file="ondas3.dat",status='unknown')
do j=1,600
do i=1,100
write(22,*) dfloat(i)*Dx, dfloat(j)*Dt, Y(i,j)
end do
write(22,*)
end do
close(22)

end
