rule df_remove_spikes:
    input:
        r_script = "code/df_cleaning.R",
        prep_data = "data/raw/Preprocessing data.csv"
    output:
        "data/processed/df.csv"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """
        
rule df_fill_missing:
    input:
        r_script = "code/df_cleaning.R",
        prep_data = "data/processed/df.csv"
    output:
        "data/processed/data.csv"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """
rule plot_ratio_of_pm_by_sites:
    input:
        r_script = "code/plot_ratio_of_pm_by_sites.R",
        prep_data = "data/ghcnd_tidy.tsv.gz",
        station_data = "data/raw/preprocessed_data.csv"
    output:
        "visuals/plot_ratio_of_pm_by_sites.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """
