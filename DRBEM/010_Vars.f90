Module Var
Implicit None
!@@@@@@@@@@@@@@@@@@@    Property of Materials   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Real(8),Parameter::k_cnd=13.4D0,h_cnv=13D3,ro_dens=4554D0,c_cap=625D0,Rad=2D-3
Real(8),Parameter::Pi=3.1415926535897932384626433832795D0

!@@@@@@@@@@@@@@@@@@@    Circular region grid   @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Integer,Parameter::M_grid=10


!@@@@@@@@@@@@@@@@@@@@@    Compute the boundary integral @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Integer,Parameter::n_teta=4*M_grid,n_dteta=7*2,way=2	!n_dteta bayad zoj bashad
Integer::nj
Real(8)::tetai(0:n_teta)
Real(8),Allocatable::WI(:),Aj(:),tetaij(:,:)

!@@@@@@@@@@@@@@@@@@@  Redial Basic function    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Integer,Parameter::N=n_teta,L=4*(M_grid-1)+1+((M_grid-2)*(M_grid-1)/2)*4,m_rbf=5
Real(8)::ro_g(1:N+L),teta_g(1:N+L),x_g(1:N+L),y_g(1:N+L),c_g(1:N+L)
!Real(8),Parameter::AA=1D0,BB=2D0
!@@@@@@@@@@@@ Read Number of file for Guassian Weight for integral computing @@@@@@@@@@@@@@

Real(8),Parameter::deltai=(2D0*Pi)/n_teta
CHARACTER(LEN=9), DIMENSION(100) :: ch      !ch=WGMMM.txt
CHARACTER(LEN=9), DIMENSION(5000) :: ch_txt    !ch_txt=TMMMM.txt
!@@@@@@@@@@@@@@@@@@@@@@@  Make Matrix    @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Real(8)::Mat_LG(1:N+L,1:N+L),Mat_IG(1:N+L,1:N+L),Mat_U(1:N+L,1:N+L)
Real(8)::Mat_F(1:N+L,1:N+L),Mat_A(1:N+L,1:N+L),Mat_Finv(1:N+L,1:N+L),Mat_I(1:N+L,1:N+L)
Real(8)::Mat_UFinv(1:N+L,1:N+L),Mat_IGFinv(1:N+L,1:N+L)
Real(8)::Vct_T(1:N+L),Vct_Tp(1:N+L),Vct_Q(1:N+L),Vct_B(1:N+L),Vct_IW(1:N+L)
Real(8)::Vct_UFinvTp(1:N+L),Vct_UFinvQ(1:N+L),Vct_IGFinvTp(1:N+L),Vct_IGFinvQ(1:N+L)
Real(8)::XYT_Final(1:N+L,1:3)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  Time  @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
Integer,Parameter::Step_W=5
Real(8),Parameter::time_s=0D-6,time_e=10000D-6
Real(8),Parameter::deltat=1D-6 !time step

Real(8),Parameter::Alpha=(ro_dens*c_cap)/(k_cnd*deltat)




End Module var