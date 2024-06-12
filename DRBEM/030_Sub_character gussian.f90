Subroutine	Ch_Gussian
Use         Var
Implicit	None
Integer::	i

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

open (unit=1010,file='WG.txt')

Do i =1,100
 Read(1010,*)ch(i)
End Do 

Close(1010)

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
open (unit=1013,file='TMMMM.txt')

Do i =1,5000
    Read(1013,*)ch_txt(i)
End Do 
Close(1013)

End Subroutine !Ch_gussian