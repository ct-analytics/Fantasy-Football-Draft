from pulp import *
import csv

in_file = "Data//ESPN-Projections.csv"
reader = csv.DictReader(open(in_file, 'rb'))

# Read in the csv file
d = []
for i, row in enumerate(reader):
    t = {k: v for k, v in row.iteritems()}
    t['id'] = i
    d.append(t)

# Create an empty LP
IP = LpProblem("Football Optimization", LpMaximize)

# Create a list of the teams and player IDs
teams = [i for i in xrange(10)]
players = [p['id'] for p in d]

# Create binary decision variables for which player ends up on which team
possible_assignments = LpVariable.dicts("x", (teams, players), 0, 1, LpBinary)

# Add in the objective function
IP += sum([float(p['total.points']) * possible_assignments[t][p['id']] for p in d for t in teams])

# Add in the constraints
for p in d:
    IP += lpSum([possible_assignments[t][p['id']] for t in teams]) == 1, "one team per player "+str(p)

for t in teams:
    IP += lpSum([possible_assignments[t][p['id']] for p in d if p['pos'] == 'QB']) >= 1, 'at least one QB per team '+str(t)
    IP += lpSum([possible_assignments[t][p['id']] for p in d if p['pos'] == 'WR']) >= 2, 'at least two WR per team '+str(t)
    IP += lpSum([possible_assignments[t][p['id']] for p in d if p['pos'] == 'RB']) >= 2, 'at least two RB per team '+str(t)
    IP += lpSum([possible_assignments[t][p['id']] for p in d if p['pos'] == 'DEF']) >= 1, 'at least one DEF per team '+str(t)
    IP += lpSum([possible_assignments[t][p['id']] for p in d if p['pos'] == 'K']) >= 1, 'at least one K per team '+str(t)
    IP += lpSum([possible_assignments[t][p['id']] for p in d if p['pos'] == 'TE']) >= 1, 'at least one TE per team '+str(t)
    IP += lpSum([possible_assignments[t][p['id']] for p in d if p['pos'] in ('TE', 'RB', 'WR')]) >= 1, 'at least one FLEX per team '+str(t)
    IP += lpSum([possible_assignments[t][p['id']] for p in d]) == 16, '16 players per team '+str(t)

# Write out the LP formulation
IP.writeLP('simple optimization.lp')

# Solve the LP
IP.solve()

# Print out the LP solution status
print("Status:", LpStatus[IP.status])

# Print out some results
print("The players chosen for team 1 are:")
for p in d:
    if possible_assignments[0][p['id']].value() == 1.0:
        print(p['name']+" ("+p['pos']+')')