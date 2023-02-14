program Q1a
implicit none
real*8 Lx, tmax, Dx, Dt, Yxx
real*8 Y(100, 600)
integer Nx, Nt, i, j
real*8 v, ro, Tr
real*8 a0, x0, sigma

!Colocando velocidades
ro=1.
Tr=1.
v=sqrt(Tr/ro)

!Condi‡oes iniciais
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
Y(i,2)=Y(i,1)
end do


!algoritimo
do j=3, Nt
   do i=2, Nx
yxx=(Y(i+1, j-1)-2*Y(i, j-1)+Y(i-1, j-1))/(Dx**2)
Y(i,j)=(Dt**2.)*(V**2.)*yxx+2*Y(i,j-1)-Y(i, j-2)
end do
end do

!respostas
open(22,file="Q1a.dat",status='unknown')
do j=1,600
do i=1,100
write(22,*) dfloat(i)*Dx, dfloat(j)*Dt, Y(i,j)
end do
write(22,*)
end do
close(22)

end
