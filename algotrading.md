## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1.  **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2.  **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3.  **Customize Trading Period:** Choose your entry and exit dates.

4.  **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5.  **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI.

6.  **Discussion:** Summarise your finding.

## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates.

```r

# Load data from CSV file
amd_df <- read.csv("AMD.csv")

# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)

amd_df <- amd_df[, c("date", "close")]
```

##Plotting the Data Plot the closing prices over time to visualize the price movement.

```r
plot(amd_df$date, amd_df$close,'l')
```

## Step 2: Trading Algorithm

Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

-   Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
-   Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
    -   If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
    -   Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
    -   You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
    -   If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.

```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA  # Corrected column name
amd_df$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Loop through everyday of trading
for (i in 1:nrow(amd_df)) {
  current_price = amd_df$close[i]
  if(previous_price == 0){
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
  }
  else if(current_price < previous_price){
    amd_df$trade_type[i] <- 'buy'
    amd_df$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
  }
  else if(i == nrow(amd_df)){ # Last day: sell everything
    amd_df$trade_type[i] <- 'sell'
    amd_df$costs_proceeds[i] <- current_price * accumulated_shares
    accumulated_shares <- 0
  }
  
  previous_price <- current_price
  amd_df$accumulated_shares[i] <- accumulated_shares
}

```

## Step 3: Customize Trading Period

-   Define a trading period you wanted in the past five years

I chose the period between Jan 1st 2020 to Jan 1st 2023 because it is a period where the stock had both increased and decreased drastically, which might show more about the effectiveness of my trading strategy to handle both situations.

```r
start_period <- as.Date("2020-01-01")
end_period <- as.Date("2023-01-01")
amd_df_period = amd_df[amd_df$date>=start_period & amd_df$date<=end_period, ]
row.names(amd_df_period) <- NULL
plot(amd_df_period$date, amd_df_period$close, 'l')
```

## Step 4: Run Your Algorithm and Analyze Results

After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

-   Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
-   Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
-   ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
amd_df_period$trade_type <- 'no trade'
amd_df_period$costs_proceeds <- 0  # Corrected column name
amd_df_period$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0

# Loop through everyday of trading
for (i in 1:nrow(amd_df_period)) {
  current_price = amd_df_period$close[i]
  
  if(i == nrow(amd_df_period)){ # Last day: sell everything
    amd_df_period$trade_type[i] <- 'sell'
    amd_df_period$costs_proceeds[i] <- current_price * accumulated_shares
    accumulated_shares <- 0
    break
  }
  
  if(previous_price == 0){
    amd_df_period$trade_type[i] <- 'buy'
    amd_df_period$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
  }
  else if(current_price < previous_price){
    amd_df_period$trade_type[i] <- 'buy'
    amd_df_period$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
  }
  
  previous_price <- current_price
  amd_df_period$accumulated_shares[i] <- accumulated_shares
}

# Calculating results over period
total_return1 = sum(amd_df_period$costs_proceeds)
invested_capital1 = -sum(amd_df_period[amd_df_period$trade_type == 'buy', ]$costs_proceeds)
roi1 = total_return1 / invested_capital1 * 100
```

```r
cat(sprintf("The profit received from my strategy is $%.2f\n", total_return1))
cat(sprintf("The ROI of my strategy is %.2f%%\n", roi1))

```

## Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)

-   Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
-   Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.

To calculate the average purchase price, I keep track of the total_purchase_price variable which stores the sum of the bought share price x share size of each buy order. Then, when I need to make stop-loss sells, I would half the accumulated shares and then update the new total_purchase_price by subtracting away the average purchase price multiplied by the new accumulated shares.  

```r
amd_df_period$trade_type <- 'no trade'
amd_df_period$costs_proceeds <- 0  # Corrected column name
amd_df_period$accumulated_shares <- 0  # Initialize if needed for tracking

# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
total_purchase_price <- 0

# Loop through every day of the period
for (i in 1:nrow(amd_df_period)) {
  current_price = amd_df_period$close[i]
  
  avg_purchase_price <- total_purchase_price / accumulated_shares
  
  if(i == nrow(amd_df_period)){ # Last day: sell everything
    amd_df_period$trade_type[i] <- 'sell'
    amd_df_period$costs_proceeds[i] <- current_price * accumulated_shares
    accumulated_shares <- 0
  }
  else if(previous_price == 0){ # First day or when previous price is 0, then buy
    amd_df_period$trade_type[i] <- 'buy'
    amd_df_period$costs_proceeds[i] <- -(current_price * share_size)
    amd_df_period$active_shares[i] <- 100
    accumulated_shares <- accumulated_shares + share_size
    total_purchase_price <- total_purchase_price + current_price * share_size
  }
  else if(current_price < 0.90*avg_purchase_price){ # Stop-loss sell when the current price falls below 90% of average purchase price
    amd_df_period$trade_type[i] <- 'sell'
    
    total_purchase_price <- total_purchase_price - avg_purchase_price * (accumulated_shares / 2)
    
    amd_df_period$costs_proceeds[i] <- current_price * (accumulated_shares / 2)
    
    accumulated_shares <- accumulated_shares / 2 # sell half of shares
  }
  else if(current_price < previous_price){ # Only check current price less than previous if none of the other 3 conditions satisfied
    amd_df_period$trade_type[i] <- 'buy'
    amd_df_period$costs_proceeds[i] <- -(current_price * share_size)
    accumulated_shares <- accumulated_shares + share_size
    total_purchase_price <- total_purchase_price + current_price * share_size
  }
  
  previous_price <- current_price
  
  # Assign accumulated_shares and avg_purchase_price to their columns in the dataframe for record-keeping
  # This makes it easier to check the program is running correctly
  amd_df_period$accumulated_shares[i] <- accumulated_shares
  amd_df_period$avg_purchase_price[i] <- avg_purchase_price
}

# Calculating results over period
total_return2 = sum(amd_df_period$costs_proceeds) # Sum of all cost proceeds from both buy and sell trade types
invested_capital2 = -sum(amd_df_period[amd_df_period$trade_type == 'buy', ]$costs_proceeds) # Sum up all the buy order's cost proceeds
roi2 = total_return2 / invested_capital2 * 100 # ROI calculated as percentage
```

```r
cat(sprintf("The profit received from my stop-loss strategy is $%.2f\n", total_return2))
cat(sprintf("The ROI of my stop-loss strategy is %.2f%%\n", roi2))
```

## Step 6: Summarize Your Findings

-   Did your P/L and ROI improve over your chosen period?
-   Relate your results to a relevant market event and explain why these outcomes may have occurred.

Discussion:

During my chosen period between Jan 2020 to Jan 2023, the AMD stock was quite volatile and had frequent reductions in stock price. Until the start of 2022, there was an overall increasing trend in AMD share price. However, from 2022 to 2023 the company's shares saw a significant drop which resulted in the price at the end of the period being only slightly higher than the start. This allowed my second strategy which had a ROI of -11.18% using the stop-loss mechanism to outperform the initial strategy which had a ROI of -24.09%. There are two examples that highlight the effectiveness of the stop-loss mechanism.

First, during Feb-Mar 2022 when the COVID-19 outburst occurred, AMD's stock price dropped from nearly 60 dollars per share to 38 dollars. This was more than a third of decrease in price. My stop-loss strategy identified this severe drop and started selling my shares as opposed to the default strategy which kept buying because the price was lower each day. Clearly, during the period of a pandemic, a more clever trading strategy is to not hold a long position as there is great uncertainty to what would unfold for the global economy. This shows the advantage of using the stop-loss trading strategy.

Moreover, on October 7th 2022, AMD's stock dropped significantly after the company slashed its revenue forecast for Q3 2022, citing a weaker PC market. As this was a substantial revision from previous expectations, the stock price decreased drastically on that day, dropping 13.8%. Prior to that day, my stop-loss strategy has already been selling shares due to the price being lower than my average purchase price. However, my initial strategy not only didn't sell, but rather continued to buy shares during the few days leading up for its prices were lower than previous days'. Thus, my initial strategy had to withstand the \~14% fall which again shows an advantageous side of using the stop-loss mechanism.
