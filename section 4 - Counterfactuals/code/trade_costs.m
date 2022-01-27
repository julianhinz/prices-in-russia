% This scripts computes the post-embargo trade costs

% Load the trade costs before embargo
Tariffs=importdata('..\data\Tariffs.mat');
% Initiate matrix of trade costs after embargo
Trade_costs=array2table(Tariffs);
% Load the mapping of countries and sectors
mapping=readtable('..\data\embargo.csv');

% identify all origins
origins = mapping.origins(~cellfun(@isempty, mapping.origins));
origins = strrep(origins," ","_");
Trade_costs.Properties.VariableNames = origins;
num_countries=length(origins); %count number of countries

% identify all tradable sectors
sectors =  mapping.sectors(~cellfun(@isempty, mapping.sectors));
num_sectors=length(sectors); %count number of sectors
Trade_costs.sector=repelem(sectors,num_countries);

% identify all destinations
destinations = mapping.origins(~cellfun(@isempty, mapping.destinations));
destinations = strrep(destinations," ","_");
Trade_costs.destination=repmat(destinations,num_sectors,1);

% identify embargoed origins
origins_emb = mapping.origins(mapping.origins_embargo==1,:);
origins_emb = strrep(origins_emb," ","_");

% identify embargoed destinations
destinations_emb = mapping.origins(mapping.destinations_embargo==1,:);
destinations_emb = strrep(destinations_emb," ","_");

% identify embargoed sectors
sectors_emb = mapping.sectors(mapping.sectors_embargo==1,:);

%Set the tariffs at the prohibitif level (1000) for each cell where:
%(1)destination is included to the list of embargoed destinations, and
%(2)origin is included to the list of embargoed origins, and
%(3)sector is included to the list of embargoed sectors
Trade_costs(ismember(Trade_costs.sector, sectors_emb) & ...
    ismember(Trade_costs.destination, destinations_emb),...
ismember(Trade_costs.Properties.VariableNames, origins_emb))={1000};

Trade_costs.sector=[];
Trade_costs.destination=[];
Trade_costs=table2array(Trade_costs);

%Save the tariff matrix with embargo
save('..\data\Trade_costs.mat','Trade_costs');

close all;
clear all;
clc;