"""
This file contains the Helper class,
with useful helper functions for generating the descriptive figures.
"""

from datetime import datetime
from matplotlib import pyplot as plt
import os
import pandas as pd
import numpy as np


class Helper:
    """
    Public helper that parses dates from strings into date-time.
    Takes as input a list of dates (strings).
    Returns a list of dates (date-time format).
    """
    @staticmethod
    def parse_datetime(dates):
        dates_to_return = []
        for date in dates:
            date_to_append = datetime.strptime(date, '%b%d%Y')  # e.g. dates of the form Mar072020
            dates_to_return.append(date_to_append)
        return dates_to_return

    """
        Simple function that plots survey results of democrat & republican fractions over time. 
        Requires as input the name of a .csv file in the "data" folder, as well as a file to save out to.
        Optionally takes as input plot title, and x- and y-label names. 
        Plots and saves result. 
        """
    @staticmethod
    def plot_dems_vs_reps(filename, save_to_name, plot_title='', x_label='', y_label='', rotate_axis_ticks=True):
        fig, ax = plt.subplots()

        df_to_plot = pd.read_csv(filename, sep=',')

        # dates = Helper.parse_datetime(df_to_plot['date'].tolist())  # convert to datetime
        dates = df_to_plot['date']
        dems = df_to_plot['dems']
        reps = df_to_plot['reps']

        plt.plot(dates, dems,
                 marker='o', color='mediumblue', linewidth=1, label='Democrats')
        plt.plot(dates, reps,
                 marker='s', color='red', linewidth=1, label='Republicans')

        # add labels and formatting
        ax.set_title(plot_title)
        ax.set_xticks(dates)
        if rotate_axis_ticks:
            ax.set_xticklabels(dates, rotation=-60, ha='center')
        else:
            ax.set_xticklabels(dates, ha='center')
        plt.ylabel(y_label)
        # plt.tight_layout()
        lgd = plt.legend(bbox_to_anchor=(1.04, 0.5), loc='center left')
        # save plot
        plt.savefig(save_to_name, bbox_extra_artists=(lgd,), bbox_inches='tight')

    """
    Method that plots dems vs. reps bar graph.
    Requires as input the name of a .csv file in the "data" folder, as well as a file to save out to.
    Optionally takes as input plot title and y-label. 
    Plots and saves result. 
    """
    @staticmethod
    def plot_dems_vs_reps_bar_graph(filename, save_to_name, plot_title='', y_label=''):
        df_to_plot = pd.read_csv(filename, sep=',')
        labels = df_to_plot['labels'].tolist()
        dems = df_to_plot['dems'].tolist()
        reps = df_to_plot['reps'].tolist()

        x = np.arange(len(labels))  # label locations
        width = 0.35  # width of the bars

        fig, ax = plt.subplots()
        rects1 = ax.bar(x - width / 2, dems, width, label='Democrats', color='mediumblue')
        rects2 = ax.bar(x + width / 2, reps, width, label='Republicans', color='red')

        # add labels and formatting
        ax.set_title(plot_title)
        ax.set_ylabel(y_label)
        ax.set_xticks(x)
        ax.set_xticklabels(labels, rotation=-40, ha='left')
        ax.legend(bbox_to_anchor=(1.04, 0.5), loc="center left", borderaxespad=0)

        # label rectangles with values
        # Helper._autolabel(ax, rects1)
        # Helper._autolabel(ax, rects2)

        fig.tight_layout()
        # save plot
        plt.savefig(save_to_name)

    """
    Private helper that attaches a text label above each rectangular bar, displaying the percentage value.
    """
    @staticmethod
    def _autolabel(ax, rects):
        for rect in rects:
            height = rect.get_height()
            ax.annotate('{}'.format(height),
                        xy=(rect.get_x() + rect.get_width() / 2, height),
                        xytext=(0, 3),  # 3 points vertical offset
                        textcoords="offset points",
                        ha='center', va='bottom')
