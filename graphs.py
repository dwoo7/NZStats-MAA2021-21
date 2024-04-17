"""
File: graphs.py
Date: Feb 2, 2024
Description: This script generates Kaplan-Meier curves graphs using output data from a NZ Stats project on Drivers Licence stages acquisitions.
"""

# Import modules
import matplotlib.pyplot as plt
import numpy as np
import os

# Set plot destination folder
folder_name = 'plots'

# Dictionary for changine varible names
clean_title = {
    'eth_european' : 'European',
    'eth_maori' : 'Maori',
    'eth_pasifika' : 'Pasifika',
    'eth_asian' : 'Asian',
    'eth_melaa' : 'MELAA',
    'dep_index_ind' : 'Deprivation Index',
    'apc_employed_ind' : 'Employment',
    'snz_sex_gender_code' : 'Gender',
    'rural_ind' : 'Rural',
    'is_benefit' : 'Benefit',
    'addr_transience' : 'Address Transience',
    'young_mother_ind' : 'Young Mothers',
    'young_mother_before_19_ind' : 'Young Mothers (Before 19)',
    'NZDep2018_1_3' : 'Deprivation Index (1-3)',
    'NZDep2018_8_10' : 'Deprivation Index (8-10)',
    'is_study_ind' : 'Studying'
}

def load_data(filename):
    # Load data from the file
    data = np.loadtxt(filename, delimiter=',', skiprows=1, dtype='str', quotechar ='"')   

    # Create a dictionary to store the results
    seperated_data = dict()

    # Run through each line in the file
    for line in data:

        # Find the first and second hald of the stratas (comma for bivariate)
        strata0 = ' & '.join([clean_title[_.split('=')[0].strip()] for _ in line[1].split(',')])
        strata1 = ' & '.join([_.split('=')[1].strip() for _ in line[1].split(',')])

        # Generate the title of the graph from data (dictionary key)
        title = f'{line[2]} - {strata0}'

        # Add accoringly to the dictionary
        if title in seperated_data:

            # Check for value in the embeded list
            found = False
            for i in range(len(seperated_data[title])):

                # If found add to list
                if seperated_data[title][i][0] == strata1:
                    found = True
                    seperated_data[title][i][1].append(line)
                    break

            # If not found create new list
            if not found:
                seperated_data[title].append([strata1,[line]])
        else:

            # Create new entry in dictionary
            seperated_data[title] = [[strata1,[line]]]
    
    # Return the sepereated data
    return seperated_data

def create_graph(title, data):
    # Create plot
    fig, ax = plt.subplots()

    # Plot each line
    for strata1_data in data:
        # Remove unknown varibles
        if 'Unk' in strata1_data[0]:
            continue

        # Calculate survival probabilies and error
        xs = np.array([int(value[3].replace('S', '0')) for value in strata1_data[1]])
        events = np.array([float(value[4].replace('S', '0')) for value in strata1_data[1]])
        at_risk = np.array([float(value[6].replace('S', '0')) for value in strata1_data[1]])
        error = np.array([float(value[7].replace('S', '0')) for value in strata1_data[1]]) * 1.962
        surv_prob = np.cumprod(1 - np.where(at_risk == 0, 0,  (events/at_risk)))

        # Clean Label
        label_filtered = strata1_data[0].replace('_', '-')
        if label_filtered == '0' or label_filtered == 'FALSE':
            label_filtered = 'False'
        elif label_filtered == '1' or label_filtered == 'TRUE':
            label_filtered = 'True'

        # Calculate error
        y1 = np.exp(np.log(surv_prob) + error)
        y2 = np.exp(np.log(surv_prob) - error)

        # Plot line and error line
        ax.fill_between(xs, y1, y2, alpha=.5, linewidth=0)
        ax.plot(xs, surv_prob, linewidth=2, label=label_filtered)

        # Create a horozontal and vertical line
        index = np.unravel_index(np.argmin(np.abs(surv_prob - 0.5)), surv_prob.shape)

        # Get line intersections
        x_intersect = xs[index]
        y_intersect = 0.5

        # Plot horizontal line
        plt.plot([0, x_intersect], [y_intersect, y_intersect], linestyle='--', color='black', linewidth=0.8)

        # Plot vertical line
        plt.plot([x_intersect, x_intersect], [0, y_intersect], linestyle='--', color='black', linewidth=0.8)

    # Set graph limits
    ax.set(ylim=(0, 1), yticks=np.arange(0.1, 1.2, 0.1),
        xlim=(0, None))
    
    # Set labels
    ax.set_title(title)
    ax.set_xlabel("Months")
    ax.set_ylabel("Survival Probability")
    ax.legend()

    # Show grid
    plt.grid(True)

    # Save graph
    plt.savefig(f'{folder_name}/{title}.png', dpi=300, bbox_inches='tight')

    # Delete graph instance
    plt.close()

# Get the data
data = load_data('data.csv')

# Create folder directory
os.makedirs(folder_name, exist_ok=True)

# Plot the graphs
for title in data:
    create_graph(title, data[title])