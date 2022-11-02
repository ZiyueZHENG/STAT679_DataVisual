import pandas as pd
import yfinance as yf
data = yf.download("^GSPC",start="2012-10-01",end="2022-11-01")
data = pd.DataFrame(data)
data.to_csv('data.csv')
