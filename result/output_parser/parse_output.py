import pandas as pd
import re
from glob import glob
import numpy as np
from datetime import datetime
import os

fname_template = r'.*data-apc-(?P<network>[^-]+)-(?P<nspecies>\d+)-species-30k-depth-(?P<method>[^-]+)-result-G-(?P<filter>[^-]*).txt'


def calc_polarized_score_old(df):
    row_idx = [x for x in [-1, 1] if x in df.index]
    col_idx = [x for x in ['-1', '1'] if x in df.columns]
    submat = df.loc[row_idx , col_idx]
    print(submat)

    ntot = submat.values.sum()
    ntp = sum(df.loc[int(x), x] for x in col_idx)

    return np.nan if ntot == 0 else ntp / ntot


def calc_polarized_score(df):
    if len(df) == 2:
        print(df)
        submat = df.loc[:, ['1']]
        print(submat)

        ntot = submat.values.sum()
        ntp = submat.loc[1, '1']
        return np.nan if ntot == 0 else ntp / ntot
    else:
        row_idx = [x for x in [-1, 0, 1] if x in df.index]
        col_idx = [x for x in ['-1', '1'] if x in df.columns]
        submat = df.loc[row_idx , col_idx]
        print(submat)

        #ntot = df.loc[:, col_idx].values.sum()
        ntot = submat.values.sum()
        ntp = sum(df.loc[int(x), x] for x in col_idx)

        return np.nan if ntot == 0 else ntp / ntot





def calc_full_score(df):
    col_idx = [x for x in ['-1', '0', '1'] if x in df.columns]

    ntot = df.values.sum()
    ntp = sum(df.loc[int(x), x] for x in col_idx)
    return np.nan if ntot == 0 else ntp / ntot




def process_file(fpath):
    meta_data = re.match(fname_template, fpath).groupdict()

    if not meta_data:
        print(f'--> failed to extract meta data from {fpath}, skipping...')

    nrows_df = 3 if meta_data['method'] != 'mic' else 2

    df = pd.read_table(fpath, skiprows=3 ,nrows=nrows_df, index_col=0, sep=r'\s+')

    meta_data['polarized_score'] = calc_polarized_score(df)
    meta_data['full_score'] = calc_full_score(df)

    if not meta_data['filter']:
        meta_data['filter'] = 'all'

    return meta_data



if __name__ == '__main__':
    dataframe_columns = ['network', 'nspecies', 'method', 'filter', 'polarized_score', 'full_score']
    timestamp = datetime.now().strftime("%Y%m%dT%H%M%S")
    output_csv_fname = f'aggregated-result-{timestamp}.csv'
    output_latest_symlink = 'aggregated-result-latest.csv'

    raw_files = glob('../../gen_data/data-apc-*.txt')
    res_lines = [process_file(fpath) for fpath in raw_files]

    aggr_df = pd.DataFrame(res_lines, columns=dataframe_columns)

    aggr_df = aggr_df.astype({'nspecies': int})
    aggr_df.sort_values(by=['method', 'nspecies', 'network', 'filter', 'polarized_score', 'full_score'], inplace=True)
    aggr_df.to_csv(output_csv_fname, index=False)
    os.remove(output_latest_symlink)
    os.symlink(output_csv_fname, output_latest_symlink)

