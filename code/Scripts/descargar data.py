import yfinance as yf
import os

ticker = yf.Ticker("AAPL")
df = ticker.history(period="max")

df = df.reset_index()
df["Date"] = df["Date"].dt.tz_localize(None)

carpeta = os.getcwd().split("\\")[-1].upper()
while carpeta != "202610_DERIVADOS_TALLER1":
    os.chdir("..")
    carpeta = os.getcwd().split("\\")[-1].upper()

output_dir = os.path.join(os.getcwd(), "data", "raw")
os.makedirs(output_dir, exist_ok=True)

output_path = os.path.join(output_dir, "AAPL_price.xlsx")
df.to_excel(output_path, index=False)
