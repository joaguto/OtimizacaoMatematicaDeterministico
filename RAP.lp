\ Model RAP
\ LP format - for model browsing. Use MPS format to capture full model detail.
Minimize
  assign[Sao_Paulo,Rio_de_Janeiro] + 2 assign[Sao_Paulo,Salvador]
   + 3 assign[Sao_Paulo,Aracaju] + 3.5 assign[Sao_Paulo,Maceio]
   + 4 assign[Sao_Paulo,Recife] + 4 assign[Joao_Pessoa,Rio_de_Janeiro]
   + 2 assign[Joao_Pessoa,Salvador] + 1.5 assign[Joao_Pessoa,Aracaju]
   + 1.2 assign[Joao_Pessoa,Maceio] + assign[Joao_Pessoa,Recife]
   + 6 assign[Manaus,Rio_de_Janeiro] + 4 assign[Manaus,Salvador]
   + 3.5 assign[Manaus,Aracaju] + 3 assign[Manaus,Maceio]
   + 2 assign[Manaus,Recife]
Subject To
 Capacidade_Maxima[Sao_Paulo]: assign[Sao_Paulo,Rio_de_Janeiro]
   + assign[Sao_Paulo,Salvador] + assign[Sao_Paulo,Aracaju]
   + assign[Sao_Paulo,Maceio] + assign[Sao_Paulo,Recife] <= 10000
 Capacidade_Maxima[Joao_Pessoa]: assign[Joao_Pessoa,Rio_de_Janeiro]
   + assign[Joao_Pessoa,Salvador] + assign[Joao_Pessoa,Aracaju]
   + assign[Joao_Pessoa,Maceio] + assign[Joao_Pessoa,Recife] <= 5000
 Capacidade_Maxima[Manaus]: assign[Manaus,Rio_de_Janeiro]
   + assign[Manaus,Salvador] + assign[Manaus,Aracaju]
   + assign[Manaus,Maceio] + assign[Manaus,Recife] <= 6000
 demanda[Rio_de_Janeiro]: assign[Sao_Paulo,Rio_de_Janeiro]
   + assign[Joao_Pessoa,Rio_de_Janeiro] + assign[Manaus,Rio_de_Janeiro]
   = 6000
 demanda[Salvador]: assign[Sao_Paulo,Salvador]
   + assign[Joao_Pessoa,Salvador] + assign[Manaus,Salvador] = 5000
 demanda[Aracaju]: assign[Sao_Paulo,Aracaju] + assign[Joao_Pessoa,Aracaju]
   + assign[Manaus,Aracaju] = 2000
 demanda[Maceio]: assign[Sao_Paulo,Maceio] + assign[Joao_Pessoa,Maceio]
   + assign[Manaus,Maceio] = 1000
 demanda[Recife]: assign[Sao_Paulo,Recife] + assign[Joao_Pessoa,Recife]
   + assign[Manaus,Recife] = 3000
Bounds
End
