% This script runs all the counterfactuals in the paper (Section 4)
%% Preparations
%clear any previous output and set the path to the current folder
close all;
clear all;
clc;
tmp = matlab.desktop.editor.getActive;
cd(fileparts(tmp.Filename));

%set the path to a subfolder with scripts
cd('./code');

% Compute the post-embargo trade costs
trade_costs;
disp('[Preparations] Computing post-embargo trade costs.')

%% Principal model with input-output linkages
disp('Principal model with input-output linkages')
% Prepare the equilibrium at the base year
disp('[Step 1/9] Computing the equilibrium in the base year (2013) with trade deficits.')
script1_base_year;
disp('[Step 2/9] Re-computing the equilibrium in the base year (2013) without trade deficits.')
script2_eliminating_trade_surplus;
disp('[Step 3/9] Computing counterfactual equilibrium (2013 with embargo) without aggregate trade deficits.')
script3_no_surplus;

% Compute welfare effects of Russian embargo (with IO linkages)
disp('[Step 4/9] Solving the model in differences for the Russian embargo.')
script4_counterfactuals;
disp('[Step 5/9] Computing welfare effects and producing output tables.')
welfare_stats(ToTL_all,VoTL_all,ToTEjn_all, ToTMjn_all,VoTXjn_all,P,pf0_all)

%% Model without IO linkages
disp('Model without IO linkages')
% Prepare the equilibrium at the base year (without IO linkages)
disp('[Step 6/9] Computing the equilibrium in the base year (2013) with trade deficits.')
script5_base_year_no_io;
disp('[Step 7/9] Re-computing the equilibrium in the base year (2013) without trade deficits.')
script6_eliminating_trade_surplus_no_io;
disp('[Step 8/9] Computing counterfactual equilibrium (2013 with embargo) without aggregate trade deficits.')
script7_no_surplus_no_io;

% Compute welfare effects of Russian embargo (without IO linkages)
disp('[Step 9/9] Solving the model in differences for the Russian embargo and computing welfare effects.')
script8_counterfactuals_no_io;