subroutine shabake
use var
implicit none
!******************************************************************************************************************************************
!variables
integer::ip,lp,i,k,l
real(8)::dl0,dt0,b
!******************************************************************************************************************************************
!body
!-------------------------------------------------------------------------------------------------------------------------------------------  

do i=1,nw
  tetai(i)=2d0*Pi
  betai(i)=-Pi/2D0
  li(i)=0d0
  neli(i)=0
  dli(i)=0D0
  netetai(i)=tetai(i)*ai(i)/L00+1
  dtetai(i)=tetai(i)/netetai(i)
end do 

!%%%%%%%%%%%%%%%%%%%%%

net=sum(netetai(:))
nel=0

do i=1,nw
 Mi(i)=netetai(i)+0	!tedad anasor royeh yek mileh
end do

NT=Sum(Mi)
nmax=MAXVAL(Mi)

!%%%%%%%%%%%%%%%%%%%%%
Allocate(tp(nw,nmax))
Allocate(xp(nw,nmax))
Allocate(yp(nw,nmax))
Allocate(arrayphiinc(nw,nmax))

Allocate( tk(nw,nmax,0:(2*nd-1)))
Allocate( xk(nw,nmax,0:(2*nd-1)))
Allocate( yk(nw,nmax,0:(2*nd-1)))
Allocate(nxh(nw,nmax,0:(2*nd-1)))
Allocate(nyh(nw,nmax,0:(2*nd-1)))
						  
!%%%%%%%%%%%%%%%%%%%%%

tp=0D0
xp=0D0
yp=0D0 

do ip=1,nw
 do lp=1,netetai(ip)

  tp(ip,lp)=betai(ip)+(lp-0.5D0)*dtetai(ip)
  xp(ip,lp)=xci(ip)+ai(ip)*dcos(tp(ip,lp))
  yp(ip,lp)=yci(ip)+ai(ip)*dsin(tp(ip,lp))
  dteta2i(ip)=dtetai(ip)/(2*nd-1)
  dlarray(ip,1)=ai(ip)*dteta2i(ip)

 end do
end do

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
do i=1,nw
 do l=1,netetai(i)
  do k=0,(2*nd-1)
   
   tk(i,l,k)=tp(i,l)-dtetai(i)/2d0+k*dteta2i(i)
   xk(i,l,k)=xci(i)+ai(i)*dcos(tk(i,l,k))
   yk(i,l,k)=yci(i)+ai(i)*dsin(tk(i,l,k))
   nxh(i,l,k)=dcos(tk(i,l,k))
   nyh(i,l,k)=dsin(tk(i,l,k))
      	  
  end do
 end do
end do
x0p=maxval(xk) +0.01d-3
y0p=minval(yk) +20d0*radius+0.01d-3
!y2p=maxval(yk) -2d0*radius-0.001d-6
hp=(2d0*ABS(y0p))/Np
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!tashkile noghate moshahede ruye marz
100 format( 100(F26.16,1x))
open(unit=11,file='place of nods.txt')
open(unit=12,file='xk_yk.txt')

do ip=1,nw
 do lp=1,netetai(ip)+neli(ip)
  write(11,100)xp(ip,lp),yp(ip,lp)
 end do

 do l=1,Mi(ip)
  do k=0,(2*nd-1)
   write(12,100)xk(ip,l,k),yk(ip,l,k)
  end do
 end do
end do
close(11)
close(12)
!--------------------------------------------------------------------------------------------------------------------------------------------
end subroutine shabake

