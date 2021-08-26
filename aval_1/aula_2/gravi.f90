program gravidade
implicit none

!Programa para calcular a aceleração da gravidade na superficie de um planeta esférico
!Programador: Horácio Santiago Ribeiro Ferreira
!Data: 22/04/2021

real, parameter     :: G = 6.6743e-11   !constante gravitacional
real                :: ag, m, r        !variaveis da aceleração da gravidade, massa e raio, respectivamente

Write(*,*) 'Entre com os valores de massa e raio do planeta'
Write(*,*) 'Massa (em kg):'
Read(*,*) m
Write(*,*) 'Raio (em km):'
Read(*,*) r

r= r*1000. !convertendo o raio de km pra m

ag = G*(m)/(r**2)   !calculo da aceleração da gravidade

write(*,*) 'O valor da gravidade é (em m/s^2):'
Write(*,*) ag







end program gravidade