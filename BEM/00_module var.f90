
module var
implicit none
!********************************************************************************************************************************************
!-------------------------------------------------
!this variables are CONSTANT 
!real(8), parameter::lightvelocity=299792458D0							
complex, parameter::	IMG=(0d0,1d0)	
real(8), parameter::	pi=3.1415926535897932384626433832795d0
real(8), parameter::	lightvelocity=299792458d0
real(8), parameter::	mu=(4d-7)*pi
real(8), parameter::	epsilon_0=1d0/(mu*(lightvelocity**2))
real(8), parameter::	electron_charge=1.602176565D-19
real(8), parameter::	h_bar=1.054571726d-34
real(8)::				had              !1.206d-4
!-------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!
!this variables are CONSTANT 



  !250d-10 !toole taghsimate eleman ha ye antegral giri

!-------------------------------------------------
!this variables are Changeable
Real(8), parameter::	TETAinc=0d0,teta_r=0D0 * Pi / 180D0
Real(8), parameter::	E0=1d0	
integer, parameter::	nw=1 	         !tedade mile ha

Real(8), parameter::	landa0=1D-3
Real(8), parameter::	dlanda=0.001d-3
integer, parameter::	NL=0
real(8),parameter::		L00=(2d0*pi*0.05d-3)/150 !250d-10 !toole taghsimate eleman ha ye antegral giri

integer, parameter::	nd=300
																	
																	
integer, parameter::	M=100,N=100	

real(8), parameter::	x0=-4.003d-3,y0=-4.007D-3,h=8D-3/N


 !it is for calculating the transmition(baraye mohasebe obur(transmition) indez of p=parash)
integer, parameter::	Mp=0,Np=100
real(8)::				x0p,radius,y0p,hp


!------------------------------------------------
!variable of pre_sub
real(8)::				ww(0:(2*nd-1)) 
integer::				test(0:M,0:N)
real(8)::				xpm(0:M),ypn(0:N)
!----------------------------------------------------
!variable oof shabake bandi

!resanandegyi(nw),permitivityi(nw),alpha		 dakhele function ha( resanandegyi(nw),permitivityi(nw)) hesab shavand
real(8)::				xci0(1:nw),yci0(1:nw),xci(1:nw),yci(1:nw),xoi(1:nw),yoi(1:nw),ai(1:nw),hi(1:nw),li(1:nw),dli(1:nw),tetai(1:nw),dtetai(1:nw),betai(1:nw)
integer::				neli(1:nw),netetai(1:nw),Mi(1:nw),NT 
integer::				nel,net,nmax
real(8)::				dteta2i(1:nw),dl2i(1:nw),dlarray(nw,2)
real(8),   Allocatable::tp(:,:),xp(:,:),yp(:,:),tk(:,:,:),xk(:,:,:),yk(:,:,:),nxh(:,:,:),nyh(:,:,:),arrayphiinc(:,:)						
complex(8),Allocatable::X_MAT(:)
!------------------------------------------
real(8), parameter::	refractive_a=1d0
real(8)::				refractive_i(nw),Alfa_i(nw)	
Real(8)::				kax,kay,kvecta,puste			   !£
complex(8)::			kvecti(nw)

real(8)::				omega,landa,av_nofuz ,transmission
Real(8)::				kr_mad,ki_mad
complex(8)::			dielectric_cons_i(1:nw),zarib(1:nw)				 !refractive_index_i = baraye vaghtist ke (omega_p & epsilon_infinity &tow) ra mikhanad va zarib shekast va alpha ra hesab mikonad) 
real(8)::				omega_p_i(1:nw),gama_i(nw)

!------------------------------------------
!variable of Field out& pre_field
integer, parameter::	MM=Np

complex(8)::			FIELD(0:M,0:N)
Real(8)::				ABS2_FIELD(0:M,0:N),xp_parash(0:MM),yp_parash(0:MM)

complex(8)::			parash(0:MM)
real(8)::				parash_intensity(0:MM),E_intensity(0:MM)
real(8)::				deltai(nw)
!*************************************************************************************************************************************************
end module var