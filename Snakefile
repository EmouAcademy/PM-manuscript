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
        r_script = "notebook/data_filling.Rmd",
        prep_data = "data/processed/df.csv"
    output:
        "data/processed/df_filled.csv"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """
rule plt1_comparisons_of_pm_by_sites:
    input:
        r_script = "code/3.1 chart, Comparisons of PMs among sites.R",
        prep_data = "data/processed/df.csv"
    output:
        "visuals/fig3_1_comparison_site.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """
rule plt2_annual_variations_of_pm_by_sites:
    input:
        r_script = "code/3.1 chart, Annual variation of PMs.R",
        prep_data = "data/processed/df.csv"
    output:
        "visuals/fig3_1_annual_variations.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """
rule plt3_daily_variations_of_pm_by_sites:
    input:
        r_script = "code/3.1 chart, Hourly variations of PMs.R",
        prep_data = "data/processed/df.csv"
    output:
        "visuals/fig3_2_hourly.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """

rule plt4_relations_meteorology_and_pm_by_sites:
    input:
        r_script = "code/3.2 plot, Relations between meteorology and PMs.R",
        prep_data = "data/processed/df.csv"
    output:
        "visuals/fig3_2_relations.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """
rule plt5_pca_analysis_of_pm_by_sites:
    input:
        r_script = "code/3.2 pca, Analysis of 4 sites.R",
        prep_data = "data/processed/df.csv"
    output:
        "visuals/fig3_2_pca_a.png",
        "visuals/fig3_2_pca_b.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """

rule plt6_trend_analysis_of_pm_by_sites:
    input:
        r_script = "code/3.3 time-series, PMs.R",
        prep_data = "data/processed/df.csv",
        filled_data = "data/processed/df_filled.csv"
    output:
        "visuals/fig3_3_trends.png"
    conda:
        "environment.yml"
    shell:
        """
        {input.r_script}
        """