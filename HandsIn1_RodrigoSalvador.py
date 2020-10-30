# -*- coding: utf-8 -*-
"""
Created on Sat Sep 12 17:42:25 2020

@author: Rodrigo Salvador
Code based on David Sumpter's 5xGModelFit.py'
"""
#Imports
import pandas as pd
import numpy as np
import json

import matplotlib.pyplot as plt
import FCPython 

import statsmodels.api as sm
import statsmodels.formula.api as smf

#Pitch dimensions in yards
pitchLengthX=120
pitchWidthY=80

#Load data from Bundesliga
#Local code unable to encode players.json content to utf-8
with open('Wyscout/events/events_Germany.json') as f:
    data = json.load(f)
with open('Wyscout/players.json', encoding='utf-8') as f2:
    players = json.load(f2)
    
#Create a data set of passes.
train = pd.DataFrame(data)
passEventId=8
passes=train[train['eventId']==passEventId]
passes_model=pd.DataFrame(columns=['Success','X','Y'])

#Select x, y and outcome
#First line suggested by Vinay Warrier on Slack group
passes_model['Success'] = passes['tags'].astype('str').str.contains('1801') 
passes_model['X'] = passes['positions'].str[0].str['x']
passes_model['Y'] = passes['positions'].str[0].str['y']

#Calculate histogram for all passes and successful passes
H_Pass=np.histogram2d(passes_model['Y'], passes_model['X'],bins=50,range=[[0, 100],[0, 100]])
sucessful_passes=passes_model[passes_model['Success']==1]
H_Suc=np.histogram2d(sucessful_passes['Y'], sucessful_passes['X'],bins=50,range=[[0, 100],[0, 100]])

#Plot the probability of sucessful passes from different points
(fig,ax) = FCPython.createPitch(pitchLengthX,pitchWidthY,'yards','black')
pos=ax.imshow(H_Suc[0]/H_Pass[0], extent=[-1,pitchLengthX+1,-1,81], aspect='auto',cmap=plt.cm.Reds)
fig.colorbar(pos, ax=ax)
ax.set_title('Probability of a sucessful passes')
plt.tight_layout()
plt.gca().set_aspect('equal', adjustable='box')
plt.show()
fig.savefig('RodrigoSalvador_ProbOfPasses.pdf', dpi=None, bbox_inches="tight") 

#Enfonce X and Y columns type to float
passes_model['X'] = passes_model['X'].astype(float)
passes_model['Y'] = passes_model['Y'].astype(float)

#Build the model that predicts success based on X and Y values
test_model = smf.glm(formula="Success ~ X + Y" , data=passes_model, 
                           family=sm.families.Binomial()).fit()
print(test_model.summary())
b=test_model.params

#Function to calculate xPass using logistic regression
model_variables=['Y','X']
def calculate_xPass(pa):    
   bsum=b[0]
   for i,v in enumerate(model_variables):
       bsum=bsum+b[i+1]*pa[v]
   xPass = 1/(1+np.exp(bsum)) 
   return xPass   

#Calculate xPass and add to the dataset
xPass=passes_model.apply(calculate_xPass, axis=1) 
passes_model = passes_model.assign(xPass=xPass)

#Create a 2D map of xPass
ppass_2d=np.zeros((pitchLengthX,pitchWidthY))
for x in range(pitchLengthX):
    for y in range(pitchWidthY):
        pa=dict()
        pa['X'] = x
        pa['Y'] = y
                
        ppass_2d[x,y] =  calculate_xPass(pa)

#Plot the probability model of sucessful passes on the pitch
(fig,ax) = FCPython.createPitch(pitchLengthX,pitchWidthY,'yards','black')
pos=ax.imshow(ppass_2d, extent=[-1,pitchLengthX+1,81,-1], aspect='auto',cmap=plt.cm.Reds,vmin=0.75, vmax=1)
fig.colorbar(pos, ax=ax)
ax.set_title('Probability of pass')
plt.gca().set_aspect('equal', adjustable='box')
plt.show()
fig.savefig('RodrigoSalvador_ModelofProbPass_xy.pdf', dpi=None, bbox_inches="tight") 

###
#Model improvement
###
#Select variables of interest
passes_model=pd.DataFrame(columns=['Success','X','Y'])
passes_model['Success'] = passes['tags'].astype('str').str.contains('1801') 
passes_model['X'] = passes['positions'].str[0].str['x']
passes_model['Y'] = passes['positions'].str[0].str['y']
passes_model['end_X'] = passes['positions'].str[1].str['x']
passes_model['end_Y'] = passes['positions'].str[1].str['y']
passes_model['playerId'] = passes['playerId']
passes_model['SimplePass'] = passes['subEventName']=='Simple pass' 
passes_model['HighPass'] = passes['subEventName']=='High pass'
passes_model['SmartPass'] = passes['subEventName']=='Smart pass'
passes_model['Cross'] = passes['subEventName']=='Cross'
passes_model['Launch'] = passes['subEventName']=='Launch'

#Define weights for each qualitative variable
#All weights are based on a percentage of occurrance of each subtype
weights=[0,0,
passes_model.SimplePass.value_counts().loc[True],
passes_model.HighPass.value_counts().loc[True],
passes_model.SmartPass.value_counts().loc[True],
passes_model.Cross.value_counts().loc[True],
passes_model.Launch.value_counts().loc[True]]

weights=[1-(x / len(passes_model.index)) for x in weights]
passes_model['SimplePassW'] = [x*weights[2] for x in passes_model['SimplePass']]
passes_model['HighPassW'] = [x*weights[3] for x in passes_model['HighPass']]
passes_model['SmartPassW'] = [x*weights[4] for x in passes_model['SmartPass']]
passes_model['CrossW'] = [x*weights[5] for x in passes_model['Cross']]
passes_model['LaunchW'] = [x*weights[6] for x in passes_model['Launch']]

#Build the model that predicts success based on selected values
model_variables=['Y','X','SimplePassW', 'HighPassW','SmartPassW','CrossW','LaunchW']
mv=''
for v in model_variables[:-1]:
    mv = mv + v + ' + '
mv = mv + model_variables[-1]
test_model = smf.glm(formula="Success ~ " + mv , data=passes_model, 
                           family=sm.families.Binomial()).fit()
print(test_model.summary())
b=test_model.params

#Calculate xPass and add to the dataset
xPass=passes_model.apply(calculate_xPass, axis=1) 
passes_model = passes_model.assign(xPass=xPass)

#Function to return median of the end points,
# or the actual point if no registers
def calc_median(data,x,y):
    if len(data.index) > 0:
        mx=data['X'].median()
        my=data['Y'].median()
        return [mx,my]
    return [x,y]

#Create a 2D map of xPass
ppass_2d=np.zeros((pitchLengthX,pitchWidthY))
for x in range(pitchLengthX):
    for y in range(pitchWidthY):
        pa=dict()
        ref=passes_model[
            (passes_model['X']==float(round(x/pitchLengthX*100))) & 
            (passes_model['Y']==float(round(y/pitchWidthY*100)))]
        pa['X'] = x
        pa['Y'] = y
        end = calc_median(passes_model[
            (passes_model['X']==float(round(x/pitchLengthX*100))) & 
            (passes_model['Y']==float(round(y/pitchWidthY*100)))],
            x,y)
        pa['end_X'] = end[0]
        pa['end_Y'] = end[1]
        pa['SimplePassW'] = ref['SimplePassW'].mean()
        pa['SmartPassW'] = ref['SmartPassW'].mean()
        pa['HighPassW'] = ref['HighPassW'].mean()
        pa['CrossW'] = ref['CrossW'].mean()
        pa['LaunchW'] = ref['LaunchW'].mean()
        
        ppass_2d[x,y] =  calculate_xPass(pa)

#Plot the probability model of sucessful passes on the pitch
#Note: for some unknown reason, the plot looks rotated -90 degrees.
(fig,ax) = FCPython.createPitch(pitchLengthX,pitchWidthY,'yards','black')
pos=ax.imshow(ppass_2d, extent=[-1,pitchLengthX+1,pitchWidthY+1,-1], aspect='auto',cmap=plt.cm.Reds,vmin=0, vmax=1)
fig.colorbar(pos, ax=ax)
ax.set_title('Probability of sucessful pass')
plt.gca().set_aspect('equal', adjustable='box')
plt.show()
fig.savefig('RodrigoSalvador_ModelofProbPass_upgraded.pdf', dpi=None, bbox_inches="tight") 

#Rank players in order of higher xPass
df1 = passes_model[passes_model.groupby("playerId")['playerId'].transform('size') > 1000]
rank_players = df1['xPass'].groupby(df1["playerId"])
top_players_index = list(rank_players.median().sort_values(ascending=False).index)
top_players_grades = rank_players.median().sort_values(ascending=False)

#Print the top 5 players of a position
i=0
position_of_interest = 'Midfielder'
top5=[]
for p in players:
    if p['wyId'] in top_players_index:
        if p['role']['name'] == position_of_interest:
            i+=1
            top5.append({
                'Player': p['shortName'] + " - " + p['role']['name'],
                'Grade':top_players_grades[p['wyId']]})
        if i>4:
            break
print(pd.DataFrame(top5))
