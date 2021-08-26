close all;
clear all;

dd = load('dados_gravi.dat');
  plot(dd(:,1),dd(:,2));
  grid