# Positive Results in Standard vs Registered Reports in Psychology

### Abstract
Selectively publishing results that support the tested hypotheses ('positive' results) distorts the available evidence for scientific claims. For the past decade, psychological scientists have been increasingly concerned about the degree of such distortion in their literature. A new publication format has been developed to prevent selective reporting: In Registered Reports, peer review and the decision to publish take place before results are known. We compared the results in published Registered Reports (N = 71 as of November 2018) with a random sample of hypothesis-testing studies from the standard literature (N = 152) in Psychology. Analysing the first hypothesis of each paper, we found 96% positive results in standard reports, but only 44% positive results in Registered Reports. We discuss possible explanations for this large difference and suggest that a plausible factor is the reduction of publication bias and/or Type-1 error inflation in the Registered-Reports literature.

### Content of the repository
This repository contains all data, code, and materials to reproduce our analyses and, we hope, to replicate our method. **Please note that we use an [OSF repository](https://osf.io/dbhgr/) as the point of access to the files contained here.**

* **Appendix**: The folder [`appendix/`](https://github.com/amscheel/positive_result_rates/tree/master/appendix) contains 
    + the [Appendix document](https://github.com/amscheel/positive_result_rates/blob/master/appendix/positive_results_SRs_RRs_appendix.pdf) with additional information about the data collection and coding procedure, a project timeline, and a robustness analysis
    + the [RMarkdown code](https://github.com/amscheel/positive_result_rates/blob/master/appendix/positive_results_SRs_RRs_appendix.Rmd) used to produce the document
    + an [annotated version of our preregistration document](https://github.com/amscheel/positive_result_rates/blob/master/appendix/preregistration_annotated.docx) with comments detailing all deviations from the preregistration (in order to view the annotations, the document must be downloaded and opened with MS Word or a comparable word processor)
* **Data**: The folder [`raw_data/`](https://github.com/amscheel/positive_result_rates/tree/master/raw_data) contains 
    + the full dataset, as an [Excel file](https://github.com/amscheel/positive_result_rates/blob/master/raw_data/positive_results_in_registered_reports_data.xlsx) (this file also contains a second sheet with an extensive data dictionary), as a [TSV file](https://github.com/amscheel/positive_result_rates/blob/master/raw_data/positive_results_in_registered_reports_data.tsv), and as an [R object](https://github.com/amscheel/positive_result_rates/blob/master/raw_data/positive_results_in_registered_reports_data.rds)
    + an [HTML codebook](https://github.com/amscheel/positive_result_rates/blob/master/raw_data/positive_results_in_registered_reports_codebook.html)
    + **Source data**: The subfolder [`raw_data/source_data/`](https://github.com/amscheel/positive_result_rates/tree/master/raw_data/source_data) contains files with the data we used to determine our sample of standard reports (including our Web of Science [search query](https://github.com/amscheel/positive_result_rates/blob/master/raw_data/source_data/SR_search_query_WoS.txt)) and the population of Registered Reports. Please consult the [Appendix](https://github.com/amscheel/positive_result_rates/blob/master/appendix/positive_results_SRs_RRs_appendix.pdf) for a detailed explanation.
* **Code**: The folder [`analysis/`](https://github.com/amscheel/positive_result_rates/tree/master/analysis) contains four analysis scripts that were used to compute all results presented in the manuscript.
* **Manuscript**: 
    + The folder [`preprint/`](https://github.com/amscheel/positive_result_rates/tree/master/preprint) contains
        - the preprint as a [PDF document](https://github.com/amscheel/positive_result_rates/blob/master/preprint/positive_results_SRs_RRs.pdf) (Note that the preprint document has been updated to the final accepted version. The initial version of the preprint can be viewed in the version history.)
        - the [RMarkdown code](https://github.com/amscheel/positive_result_rates/blob/master/preprint/positive_results_SRs_RRs.Rmd) used to produce the preprint, along with two .bib files and several auxiliary files
        - Figure 1 from the manuscript as a [draw.io](https://github.com/amscheel/positive_result_rates/blob/master/preprint/sampling_process_flowchart.drawio) and [PNG](https://github.com/amscheel/positive_result_rates/blob/master/preprint/sampling_process_flowchart.png) file
    + The folder [`submission1/`](https://github.com/amscheel/positive_result_rates/tree/master/submission1) contains the documents of our first (unsuccessful) submission to *Nature Human Behaviour*
    + The folder [`submission2/`](https://github.com/amscheel/positive_result_rates/tree/master/submission2) contains the documents of our second (successful) submission to *Advances in Methods and Practices in Psychological Science*
* "Old files": The folder [`old_files/`](https://github.com/amscheel/positive_result_rates/tree/master/old_files) contains files to reproduce the analyses reported in MS' BEP dissertation. These are partially outdated and not necessary to reproduce the content of our manuscript.

