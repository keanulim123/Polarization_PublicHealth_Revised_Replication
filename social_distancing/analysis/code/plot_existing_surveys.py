"""
This file contains the main run for plotting the relevant descriptive figures.
"""

import sys
sys.path.insert(1, 'code/helpers')
import plot_existing_surveys_helpers
import matplotlib.pyplot as plt
plt.style.use('seaborn-paper')


if __name__ == '__main__':

    # 0) back-up option: plot existing figure: "concern over the spread of coronavirus"
    plot_existing_surveys_helpers.Helper.plot_dems_vs_reps(filename='input/concern_over_spread_poll.csv',
                                                         save_to_name='output/concern over spread of coronavirus.pdf',
                                                         # plot_title='Concern over the spread of coronavirus',
                                                         y_label='Share who said they are concerned')

    # other data sources
    # 1) (a) gallup polls: percentagge of Americans who have avoided public places due to the coronavirus
    plot_existing_surveys_helpers.Helper.plot_dems_vs_reps(filename='input/avoiding_public_places.csv',
                                                         save_to_name='output/gallup avoiding public places.pdf',
                                                         # plot_title='Self-reported avoidance of public places due to the coronavirus',
                                                         y_label='Share of responses',
                                                         rotate_axis_ticks=False)

    # 1) (b) gallup polls: percentagge of Americans who have avoided small gatherings due to the coronavirus
    plot_existing_surveys_helpers.Helper.plot_dems_vs_reps(filename='input/avoiding_small_gatherings.csv',
                                                         save_to_name='output/gallup avoiding small gatherings.pdf',
                                                         # plot_title='Self-reported avoidance of small gatherings due to the coronavirus',
                                                         y_label='Share of responses',
                                                         rotate_axis_ticks=False)

    # 2) NPR/PBS/Marist March polls: Have you or anyone in your household _____ due to the coronavirus, or not?
    # (i) decided to eat at home more often
    # (ii) stocked up on food or supplies
    # (iii) changed travel plans
    # (iv) cancelled plans to avoid crowds
    plot_existing_surveys_helpers.Helper.plot_dems_vs_reps_bar_graph(filename='input/marist_march_polls.csv',
                                                                   save_to_name='output/marist march behavior change polls.pdf',
                                                                   # plot_title='Self-reported behavior changes due to coronavirus in March',
                                                                   y_label='Share responding \"yes\"')