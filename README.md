# obesity-estimation

Predict a person's risk for obesity.

Made with R and Shiny.

Dataset sourced from the [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/Estimation+of+obesity+levels+based+on+eating+habits+and+physical+condition+).

## Requirements

R 4.1.2 - Get it [here](https://cran.r-project.org/).

Required R packages
- bslib
- rpart
- rpart.plot
- shiny
- shinyWidgets

## Usage

0\. Install required packages if missing.

```r
install.packages("package-name")
```

1\. Set working directory.

```r
setwd("path/obesity-estimation/src")
```

2\. Launch the app.

```r
source("obesity.r")
```

## Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License

[MIT](https://choosealicense.com/licenses/mit/)