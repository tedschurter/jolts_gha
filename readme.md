### About


##### Work in progress. This repository is intended to automatically download and create a series of visualizations of U.S. Bureau of Labor Statistics Job Openings and Labor Turnover Survey (JOLTS) data. Using Github Actions, the project compares existing JOLTS data on job openings, total separations, hires, quits, layoffs and discharges with data available via the BLS API. When new data is detected, it is downloaded, aggregated, visualized and published automatically. The new data is then used as the benchmark to compare against new data releases. 

<br><br>

**Script** | **Description** | **Output** 
:---|:---|:---|
[20230921_jolts_base_data.R](https://github.com/tedschurter/bls_jolts/blob/main/scripts/20230921_jolts_base_data.R)|Downloads JOLTS data on job openings, total separations, hires, quits, layoffs and discharges, converts it to long format, adds date column and generates mean and median for each series. |[jolts.csv](https://github.com/tedschurter/bls_jolts/blob/main/clean_data/jolts.csv)|
[20230921_jolts_plots.R](https://github.com/tedschurter/bls_jolts/blob/main/scripts/20230921_jolts_plots.R)|R script to create plots of JOLTS data.|Saving plots is commented out by default.|
[index.Rmd](https://github.com/tedschurter/bls_jolts/blob/main/docs/index.Rmd) |Script designed to run in Github Actions and check existing JOLTS data file with current data avialble on BLS API for the same series. If the data hasn't changed, the script stops; if new data is available, the script continues, generating a new index.html file that is pushed to the repository.|[Index.html](https://github.com/tedschurter/bls_jolts/blob/main/docs/index.html)
