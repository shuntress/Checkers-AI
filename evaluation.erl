-module(evaluation).

-export([evaluation/12]).


%% O = max X = min
%% A = number of O Pawns, B = number of O Kings(twice as valuable as pawns), C = number of O Safe Pawns, D = number of O Safe Kings(again, twice as valuable)
%% E = number of X Pawns, F = number of X Kings(twice as detrimental as pawns), G = number of X Safe Pawns, H = number of X Safe Kings(twice as detrimental)
%% I = number of O spaces at the back that are open (multiplied by 4 for the total number of spaces. all 4 spaces open is very bad for O)
%% J = number of O pawns 1 space from promotion, K = O pawns 2 spaces away from promotion, L = number of X pawns 1 space away from promotion (in order to be eligible for promotion there %% must be an open space
%%
%%
%%
evaluation(A, B, C, D, E, F, G, H, I, J, K, L) ->
  (A + (2*B) + C + (2*D) + (1.5*J) + K ) - (E + (2*F) + G + (2*H) + (4*I) + (1.5*L)).