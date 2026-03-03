# UK House Purchase Affordability Ballparker

A Shiny web application designed to give prospective UK homebuyers a realistic estimate of their actual purchasing power. 

Instead of just calculating maximum loan sizes, this tool works backwards from a user's current savings to determine the maximum property price they can afford after all mandatory taxes and moving costs are paid.

## The Problem

Standard mortgage calculators tell you what a bank will lend you, but they often ignore the reality of moving. Your available savings don't just go towards your deposit; they also have to cover:
* Stamp Duty Land Tax (SDLT)
* Conveyancing and legal fees
* Property surveys
* Moving and logistics costs

If a buyer assumes all their savings can go towards a deposit, they will severely overestimate what they can afford. This app bridges that gap to provide a realistic, safe "ballpark" figure.

## What It Does

The app takes the user's current savings, available mortgage loan, and buyer status, and instantly calculates the maximum property price they can afford. It does this by testing thousands of potential property prices in the background to find the exact point where the user's savings can comfortably cover the deposit, tax, and associated fees.

**Key Features:**
* **Up-to-Date Stamp Duty:** Calculates exact SDLT based on current UK rules, adjusting automatically for First-Time Buyers, Home Movers, and Second Home/Investment surcharges.
* **Dynamic Fee Estimation:** Automatically scales estimated conveyancing, survey, and moving costs against the value of the property, preventing buyers from underestimating fees on larger homes. Users can also override this with custom exact quotes.
* **Mortgage Metrics:** Provides a standard amortisation estimate for monthly mortgage repayments based on user-defined interest rates and terms.
* **LTV Tracking:** Calculates the actual Loan-to-Value (LTV) percentage so users know which mortgage product tiers they will qualify for.

## How to Run Locally

To run this application on your local machine, you will need R installed along with the following packages:

```R
install.packages(c("shiny", "bslib", "shinyWidgets", "scales"))
```

Once the dependencies are installed, you can launch the app by running the script in RStudio or via the R console:

```R
shiny::runApp()
```

## Disclaimer

This application is designed to provide a ballpark estimate to assist with initial property searches. It does not constitute formal financial or legal advice. Users should always consult a qualified mortgage broker or solicitor for exact figures tailored to their specific circumstances.