import pandas as pd
from glob import glob
from os import path

def pivot_result_dataframe(df):
    df['method'] = df['method'].astype('category')
    df['filter'] = df['filter'].astype('str')
    df.drop(df.filter(regex=r'Unnamed: \d+').columns, axis=1, inplace=True)
    df_pivoted = pd.pivot_table(
        df.drop(['fpath'], axis=1),
        index=['network', 'ntaxa', 'filter', 'abundance.file'],
        columns=['method'],
        values=['predicted', 'reference'],
    )
    df_pivoted.columns = ['.'.join(names) for names in df_pivoted.columns]
    df_pivoted.reset_index(inplace=True)
    return df_pivoted


def pivot_csv_files(path_in, root_out='.'):
    df = pd.read_csv(path_in)
    path_out = path.join(root_out, f'{path.basename(path_in).rstrip(".csv")}_pivoted.csv')
    print(path_out)
    pivot_result_dataframe(df).to_csv(path_out, index=False)


if __name__ == '__main__':
    input_glob = "../output_aggregated/*.csv"
    output_root = "../output_aggregated_pivoted"
    for file_in in glob(input_glob):
        pivot_csv_files(file_in, root_out=output_root)

