import streamlit as st
import requests
import os

API_BASE = os.getenv("API_URL", "http://localhost:8000")

@st.cache_data(show_spinner=False)
def fetch_levels():
    return requests.get(f"{API_BASE}/levels", timeout=10).json()

def predict(payload):
    r = requests.post(f"{API_BASE}/predict", json=payload, timeout=15)
    r.raise_for_status()
    pred = r.json()["predicted_pedestrians"]
    if isinstance(pred, list):
        pred = pred[0]
    return float(pred)

st.title("Brooklyn Bridge Pedestrian Predictor")
st.caption("Uses an R linear regression model via a local plumber API.")
st.caption("The linear regression model used for this project explains 85.3 percent of the variation between selected variables and amount of pedestrians.")
st.caption("Note: The data used for this project is from NYC Open Data and is open source.")


levels = fetch_levels()

# MAIN PAGE INPUTS
with st.form("predict_form"):
    c1, c2, c3 = st.columns(3)

    with c1:
        weather = st.selectbox("Weather summary", levels["weather_summary"])
        weekday = st.selectbox("Weekday", levels["weekday"])

    with c2:
        month = st.selectbox("Month", levels["month"])
        hour = st.selectbox("Hour", levels["hour"])

    with c3:
        temperature = st.number_input("Temperature", value=50.0)
        precipitation = st.number_input("Precipitation", min_value=0.0, value=0.0)
        has_event = st.selectbox("Has event?", [0, 1])

    submitted = st.form_submit_button("Predict")

if submitted:
    payload = {
        "weather_summary": weather,
        "temperature": temperature,
        "precipitation": precipitation,
        "has_event": has_event,
        "hour": int(hour),
        "weekday": weekday,
        "month": month,
    }

    with st.spinner("Predicting..."):
        pred = predict(payload)

    st.metric("Predicted pedestrians", f"{pred:,.0f}")
    st.write("Raw prediction:", pred)
