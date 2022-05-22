import pandas
import plotly.express as px

df = pandas.read_json("./src/Fei.Backend.MetricCollection/Output/metrics-2022-05-20-03-59-54.json")

fig = px.line(df, x="NumOfEntries", y="Wsc", color="Phase", title="WSC in each phase of the algorithm")
fig.show()

fig = px.line(df, x="NumOfEntries", y="NumOfRules", color="Phase", title="Number of rules in each phase of the algorithm")
fig.show()

fig = px.line(df, x="NumOfRules", y="Wsc", color="Phase", title="WSC/Number of rules relationship in each phase of the algorithm")
fig.show()