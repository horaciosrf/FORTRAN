close all;
clear all;

dd = load('dados_reta.dat');
x = dd(:,1)
y = dd(:,2)
ym = dd(:,3)
figure(1)
hold on
plot(x,y,'b')
plot(x,ym,'ro')
grid