import polars as pl
from glob import glob
from os import path
import logging

logging.basicConfig(format='%(asctime)s %(message)s')
logger = logging.getLogger(__file__)
logger.setLevel(logging.INFO)

def pivot_result_dataframe(df):
    df = df.drop('').with_columns(
        [
            pl.col('method').cast(pl.Categorical).cat.set_ordering('lexical'),
        ]
    )
    logger.info(f'df has columns {df.columns}')
    df_pivoted = df.pivot(
        values=['predicted', 'reference'],
        index=['network', 'ntaxa', 'filter', 'abundance.file', 'mat.cell.id'],
        columns=['method'],
        aggregate_function='mean',
    )
    return df_pivoted


def pivot_csv_files(path_in, root_out='.'):
    df = pl.read_csv(path_in)
    path_out = path.join(root_out, f'{path.basename(path_in).rstrip(".csv")}_pivoted.csv')
    logger.info(f'saved output to {path_out}')
    pivot_result_dataframe(df).write_csv(path_out)


if __name__ == '__main__':
    input_glob = "../output_aggregated/*.csv"
    output_root = "../output_aggregated_pivoted"
    for file_in in glob(input_glob):
        logger.info(f'--> Processing {file_in}...')
        pivot_csv_files(file_in, root_out=output_root)

