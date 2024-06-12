Subroutine Output

Use       var
Implicit  None 
integer::j


Do j=1,L
XYT_Final(j,1)=x_g(j+N)
XYT_Final(j,2)=y_g(j+N)
End Do
DO j=1,N
XYT_Final(j+L,1)=x_g(j)
XYT_Final(j+L,2)=y_g(j)   
End Do 


Do j=1,L
XYT_Final(j,3)=Vct_T(j+N)
End Do             
DO j=1,N
XYT_Final(j+L,3)=Vct_T(j)
End Do    
    
   
End subroutine
	
