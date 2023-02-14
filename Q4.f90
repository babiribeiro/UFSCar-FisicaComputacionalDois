program Q4
implicit none
real*8 Lx, tmax, Dx, Dt, Yxx
real*8 Y(100, 600)
integer Nx, Nt, i, j, n
real*8 v, ro, Tr
real*8 a0, x0, k

!Velocidade
ro=1.
Tr=1.
v=sqrt(Tr/ro)

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
n=1
k=(n*4*atan(1.))/Lx
do i=1, Nx
Y(i,1)=a0*sin(k*i*Dx)
Y(i,2)=Y(i,1)
end do


!itera‡Æo
do j=3, Nt
do i=2, Nx
yxx=(Y(i+1, j-1)-2*Y(i, j-1)+Y(i-1, j-1))/(Dx**2)
Y(i,j)=(Dt**2.)*(V**2.)*yxx+2*Y(i,j-1)-Y(i, j-2)
end do
end do

!Escrever Resposta
open(22,file="ondas41.dat",status='unknown')
do j=1,600
do i=1,100
write(22,*) dfloat(i)*Dx, dfloat(j)*Dt, Y(i,j)
end do
write(22,*)
end do
close(22)

!
a0=1E-3
x0=0.5
n=2
k=(n*4*atan(1.))/Lx
do i=1, Nx
Y(i,1)=a0*sin(k*i*Dx)
Y(i,2)=Y(i,1)
end do


!itera‡Æo
do j=3, Nt
do i=2, Nx
yxx=(Y(i+1, j-1)-2*Y(i, j-1)+Y(i-1, j-1))/(Dx**2)
Y(i,j)=(Dt**2.)*(V**2.)*yxx+2*Y(i,j-1)-Y(i, j-2)
end do
end do

!Escrever Resposta
open(23,file="ondas42.dat",status='unknown')
do j=1,600
do i=1,100
write(23,*) dfloat(i)*Dx, dfloat(j)*Dt, Y(i,j)
end do
write(23,*)
end do
close(23)

!
a0=1E-3
x0=0.5
n=3
k=(n*4*atan(1.))/Lx
do i=1, Nx
Y(i,1)=a0*sin(k*i*Dx)
Y(i,2)=Y(i,1)
end do


!itera‡Æo
do j=3, Nt
do i=2, Nx
yxx=(Y(i+1, j-1)-2*Y(i, j-1)+Y(i-1, j-1))/(Dx**2)
Y(i,j)=(Dt**2.)*(V**2.)*yxx+2*Y(i,j-1)-Y(i, j-2)
end do
end do

!Escrever Resposta
open(24,file="ondas43.dat",status='unknown')
do j=1,600
do i=1,100
write(24,*) dfloat(i)*Dx, dfloat(j)*Dt, Y(i,j)
end do
write(24,*)
end do
close(24)

end
