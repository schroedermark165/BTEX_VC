


#Make determination
sdf.loc[:, 'Hydrocarbons'] = False
sdf.loc[((sdf['METHANE']>0)|(sdf['ETHANE']>0)|(sdf['PROPANE']>0)|(sdf['BUTANE']>0)|(sdf['ISOBUTANE']>0)|(sdf['PENTANE']>0)|(sdf['ISOPENTANE']>0)), 'Hydrocarbons'] = True
print('Total number of results that passed have hydrocarbons: '+str(len(sdf[sdf['Hydrocarbons']==True]))) 

#Label Owen Thermogenic cases
#MARK YOU MAY NOT NEED THESE NEXT TWO LINES
# sdf.loc[(sdf['Hydrocarbons']==True), 'Sherwood Gas Type'] = 'Microbial'
# sdf.loc[(sdf.FacID.isin(sdf_lc.fac_id)), 'Sherwood Gas Type'] = 'Thermogenic'

#First look for samples that have all C4+ hydrocarbons present. These samples have some thermogenic gas and are not purely microbial
sdf.loc[:,'C4+ Present'] = False
sdf.loc[((sdf['BUTANE']>0)|(sdf['ISOBUTANE']>0)|(sdf['PENTANE']>0)|(sdf['ISOPENTANE']>0)), 'C4+ Present'] = True

# #To sort pure thermogenic from mixed, look at slope between C3-C2 and C2-C1
sdf.loc[:,'C4-C3 Slope'] = (sdf['DELTA 13C nC4']-sdf['DELTA 13C C3'])/(1/4-1/3)
sdf.loc[:,'C3-C2 Slope'] = (sdf['DELTA 13C C3']-sdf['DELTA 13C C2'])/(1/3-1/2)
sdf.loc[:,'C2-C1 Slope'] = (sdf['DELTA 13C C2']-sdf['DELTA 13C C1'])/(1/2-1)

# # - Need to consider evidence of CH4 oxidation - 

#Greatest slope for C2-C1 for a production gas is Sussex. Use that as an end member: -40.3 + 1 s.d. = -43.66
sdf.loc[:, 'Gas Type'] = 'Unknown'

#Start with C4+
sdf.loc[(sdf['C4+ Present']==True)&(sdf['Gas Type']=='Unknown'), 'Gas Type'] = 'Thermogenic present'

# **Biasing results towards thermogenic**
sdf.loc[(sdf['C4+ Present']==True)&(sdf['C2-C1 Slope']>-43.66), 'Gas Type'] = 'Thermogenic'
sdf.loc[(sdf['C4+ Present']==True)&(sdf['C2-C1 Slope']<-43.66), 'Gas Type'] = 'Mixed'


#Consider d13CC1 vs GC Plot
sdf_bern = sdf[~(pd.isnull(sdf['GAS COMP']))&~(pd.isnull(sdf['DELTA 13C C1']))]
print('Number of groundwater analytical samples with GAS COMP & DELTA 13C C1: '+str(len(sdf_bern)))
print('Number of water wells with analytical samples that have GAS COMP & DELTA 13C C1: '+str(sdf_bern.FacID.nunique()))

#Consider microbial if within bounds on bernard plot and gas type is unknown
sdf.loc[(sdf['DELTA 13C C1']<-60) & (sdf['GAS COMP']>100)&(sdf['Gas Type']=='Unknown'), 'Gas Type'] = 'Microbial'
#Consider mixed if within microbial bounds on bernard plot and c4+ hydrocarbons are present.
sdf.loc[(sdf['DELTA 13C C1']<-60) & (sdf['GAS COMP']>100)&(sdf['Gas Type']=='Thermogenic present'), 'Gas Type'] = 'Mixed'

#Consider thermogenic if within bounds on bernard plot and gas type is unknown
sdf.loc[(sdf['DELTA 13C C1']>-55) & (sdf['GAS COMP']<50)&((sdf['Gas Type']=='Unknown')|(sdf['Gas Type']=='Thermogenic present')), 'Gas Type'] = 'Thermogenic'

#Consider mixed if outside bounds for microbial/thermogenic and gas type is unknown or thermogenic present
sdf.loc[(sdf['DELTA 13C C1']<=-55) & (sdf['DELTA 13C C1']>=-60)&(sdf['GAS COMP']>=50)&(sdf['GAS COMP']<=100)&((sdf['Gas Type']=='Unknown')|(sdf['Gas Type']=='Thermogenic present')), 'Gas Type'] = 'Mixed'
sdf.loc[(sdf['DELTA 13C C1']<-60) & (sdf['GAS COMP']<100)&((sdf['Gas Type']=='Unknown')|(sdf['Gas Type']=='Thermogenic present')), 'Gas Type'] = 'Mixed'
sdf.loc[(sdf['DELTA 13C C1']>-55) & (sdf['GAS COMP']>50)&((sdf['Gas Type']=='Unknown')|(sdf['Gas Type']=='Thermogenic present')), 'Gas Type'] = 'Mixed'
sdf.loc[(sdf['DELTA 13C C1']<=-55) & (sdf['DELTA 13C C1']>=-60)&(~pd.isnull(sdf['GAS COMP']))&((sdf['Gas Type']=='Unknown')|(sdf['Gas Type']=='Thermogenic present')), 'Gas Type'] = 'Mixed'
sdf.loc[(~pd.isnull(sdf['DELTA 13C C1']))&(sdf['GAS COMP']>=50)&(sdf['GAS COMP']<=100)&((sdf['Gas Type']=='Unknown')|(sdf['Gas Type']=='Thermogenic present')), 'Gas Type'] = 'Mixed'
