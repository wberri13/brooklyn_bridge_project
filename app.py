import os
import requests
import streamlit as st


# ---------- Config ----------
st.set_page_config(page_title="Brooklyn Bridge Pedestrian Predictor", layout="centered")

API_BASE = os.getenv("API_URL", "http://localhost:8000")  # local default


# ---------- Helpers ----------
@st.cache_data(show_spinner=False)
def fetch_levels():
    """Fetch factor levels from the R plumber API."""
    url = f"{API_BASE}/levels"
    resp = requests.get(url, timeout=10)
    resp.raise_for_status()
    return resp.json()


def post_predict(payload: dict) -> float:
    """Send payload to /predict and return a single numeric prediction."""
    url = f"{API_BASE}/predict"
    resp = requests.post(url, json=payload, timeout=15)
    resp.raise_for_status()
    data = resp.json()

    # Your API returns: {"predicted_pedestrians": [1184.9133]} OR sometimes a single number.
    pred = data.get("predicted_pedestrians", None)
    if pred is None:
        raise ValueError(f"API response missing 'predicted_pedestrians': {data}")

    if isinstance(pred, list):
        if len(pred) == 0:
            raise ValueError(f"Empty prediction list returned: {data}")
        pred = pred[0]

    return float(pred)


# ---------- UI ----------
st.title("Brooklyn Bridge Pedestrian Predictor")
st.caption("Uses your R linear regression model via a local plumber API.")

# Try to load levels; if API isn't running, show a helpful error
try:
    levels = fetch_levels()
except Exception as e:
    st.error(
        "Couldn't reach the R API. Make sure plumber is running at "
        f"{API_BASE} and that you have a GET /levels endpoint.\n\n"
        f"Error: {e}"
    )
    st.stop()

# Expected keys from /levels:
# weather_summary, weekday, month, hour
weather_levels = levels.get("weather_summary", [])
weekday_levels = levels.get("weekday", [])
month_levels = levels.get("month", [])
hour_levels = levels.get("hour", [])

# Sidebar inputs
st.sidebar.header("Inputs")

weather_summary = st.sidebar.selectbox("Weather summary", weather_levels)
weekday = st.sidebar.selectbox("Weekday", weekday_levels)
month = st.sidebar.selectbox("Month", month_levels)

# hours may come back as strings like "0", "1", ..., "23"
hour_choice = st.sidebar.selectbox("Hour", hour_levels)

temperature = st.sidebar.number_input("Temperature", value=50.0)
precipitation = st.sidebar.number_input("Precipitation", min_value=0.0, value=0.0)
has_event = st.sidebar.selectbox("Has event?", [0, 1])

# Predict button
if st.button("Predict pedestrians"):
    payload = {
        "weather_summary": weather_summary,
        "temperature": temperature,
        "precipitation": precipitation,
        "has_event": has_event,
        "hour": int(hour_choice),  # convert "12" -> 12
        "weekday": weekday,
        "month": month
    }

    with st.spinner("Predicting..."):
        try:
            pred = post_predict(payload)
            st.success("Prediction complete")

            # Display
            st.metric("Predicted pedestrians", f"{pred:,.0f}")
            st.write("Raw prediction:", pred)

            # Optional: show the payload for debugging
            with st.expander("Payload sent to API"):
                st.json(payload)

        except Exception as e:
            st.error(f"Prediction failed: {e}")
            st.write("If you see NA issues, it usually means a factor-level mismatch.")
