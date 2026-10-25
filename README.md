# Google AI Overview collector for R (macOS)

A small example of using **RStudio + Google Chrome + AppleScript** to collect Google AI Overview text for your own questions. This is a public, generic version of an exploratory browser workflow. It contains no client data, original experiment questions, or personal paths.

## Setup

1. On macOS, install Google Chrome and R; run the script from RStudio.
2. Download `chrome_ai_overview_public.R` and `questions_template.csv` into your RStudio working directory.
3. Rename `questions_template.csv` to `questions.csv`; replace the examples with your own questions (columns `id,question`, unique IDs).
4. In Chrome enable **View → Developer → Allow JavaScript from Apple Events**. Approve macOS automation permissions for RStudio as needed.
5. In RStudio run:

```r
install.packages("jsonlite") # once
source("chrome_ai_overview_public.R")
```

The script saves `google_ai_overview_responses.csv` with each question, status, extracted AI Overview text, linked source URLs, and collection time. Existing question IDs are skipped on reruns; rename output to start a new collection.

## Limitations

This is not an official Google API. Google may change its search interface or restrict automated access; some searches have no AI Overview. The extracted text can contain source-card artifacts, so **review the captured responses manually**. The script stops on detected challenges rather than bypassing them; delays are courtesy pacing, not a guarantee against rate limits. This example has not been independently tested across Chrome versions.

Do not put sensitive data into search questions. Use the script responsibly and comply with applicable service terms.
