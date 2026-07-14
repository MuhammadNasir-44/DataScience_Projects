"""
Customer Churn Prediction — Telco dataset
=========================================

End-to-end supervised-learning project: predict which telecom customers are
likely to cancel ("churn"), so the business can intervene before they leave.

Pipeline
--------
1. Load & clean the data
2. Exploratory analysis (who churns, and why)
3. Preprocess (encode categoricals, scale)
4. Train two models — Logistic Regression (baseline) and Random Forest
5. Evaluate on a held-out test set (ROC-AUC, accuracy, precision/recall)
6. Interpret — which factors drive churn

Run:  python churn_analysis.py
Outputs metrics to the console and saves charts to ./images/.

Author: Muhammad Nasiruddin
Dataset: IBM Telco Customer Churn (public).
"""

from __future__ import annotations

from pathlib import Path

import matplotlib
matplotlib.use("Agg")  # render charts to file without a display
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import (
    classification_report,
    confusion_matrix,
    roc_auc_score,
    roc_curve,
)
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler

DATA_PATH = Path(__file__).parent / "data" / "telco_customer_churn.csv"
IMG_DIR = Path(__file__).parent / "images"
GREEN, BLUE, GREY = "#2a9d8f", "#264653", "#adb5bd"


def load_and_clean() -> pd.DataFrame:
    """Load the raw CSV and fix the known data-quality issues."""
    df = pd.read_csv(DATA_PATH)

    # TotalCharges arrives as text and has blank strings for brand-new customers.
    df["TotalCharges"] = pd.to_numeric(df["TotalCharges"], errors="coerce")
    df["TotalCharges"] = df["TotalCharges"].fillna(0)

    # Target to 0/1.
    df["Churn"] = (df["Churn"] == "Yes").astype(int)

    df = df.drop(columns=["customerID"])
    return df


def explore(df: pd.DataFrame) -> None:
    """Print headline facts and save exploratory charts."""
    churn_rate = df["Churn"].mean()
    print(f"Rows: {len(df):,}   Features: {df.shape[1] - 1}")
    print(f"Overall churn rate: {churn_rate:.1%}\n")

    # 1) Churn by contract type — usually the strongest single signal.
    by_contract = df.groupby("Contract")["Churn"].mean().sort_values()
    fig, ax = plt.subplots(figsize=(6, 3.5))
    by_contract.plot(kind="barh", color=GREEN, ax=ax)
    ax.set_xlabel("Churn rate")
    ax.set_title("Churn rate by contract type")
    ax.xaxis.set_major_formatter(lambda x, _: f"{x:.0%}")
    fig.tight_layout()
    fig.savefig(IMG_DIR / "churn_by_contract.png", dpi=110)
    plt.close(fig)

    # 2) Churn by tenure — new customers churn most.
    bins = pd.cut(df["tenure"], [0, 6, 12, 24, 48, 72], include_lowest=True)
    by_tenure = df.groupby(bins, observed=True)["Churn"].mean()
    fig, ax = plt.subplots(figsize=(6, 3.5))
    by_tenure.plot(kind="bar", color=BLUE, ax=ax)
    ax.set_xlabel("Tenure (months)")
    ax.set_ylabel("Churn rate")
    ax.set_title("Churn rate by customer tenure")
    ax.yaxis.set_major_formatter(lambda x, _: f"{x:.0%}")
    plt.xticks(rotation=0)
    fig.tight_layout()
    fig.savefig(IMG_DIR / "churn_by_tenure.png", dpi=110)
    plt.close(fig)

    print("Churn rate by contract:")
    print(by_contract.apply(lambda v: f"{v:.1%}").to_string(), "\n")


def preprocess(df: pd.DataFrame):
    """One-hot encode categoricals and return X, y."""
    y = df["Churn"]
    X = pd.get_dummies(df.drop(columns=["Churn"]), drop_first=True)
    return X, y


def evaluate(name: str, y_true, proba, ax) -> float:
    """Print metrics for a model and add its ROC curve to `ax`."""
    auc = roc_auc_score(y_true, proba)
    preds = (proba >= 0.5).astype(int)
    print(f"--- {name} ---")
    print(f"ROC-AUC: {auc:.3f}")
    print(classification_report(y_true, preds, target_names=["Stay", "Churn"], digits=3))
    fpr, tpr, _ = roc_curve(y_true, proba)
    ax.plot(fpr, tpr, label=f"{name} (AUC {auc:.2f})")
    return auc


def main() -> None:
    df = load_and_clean()
    explore(df)

    X, y = preprocess(df)
    X_train, X_test, y_train, y_test = train_test_split(
        X, y, test_size=0.25, random_state=42, stratify=y
    )

    scaler = StandardScaler()
    X_train_s = scaler.fit_transform(X_train)
    X_test_s = scaler.transform(X_test)

    fig, ax = plt.subplots(figsize=(6, 5))
    ax.plot([0, 1], [0, 1], "--", color=GREY, label="Random guess")

    logreg = LogisticRegression(max_iter=1000, class_weight="balanced")
    logreg.fit(X_train_s, y_train)
    evaluate("Logistic Regression", y_test, logreg.predict_proba(X_test_s)[:, 1], ax)

    rf = RandomForestClassifier(
        n_estimators=300, max_depth=8, min_samples_leaf=20, random_state=42
    )
    rf.fit(X_train, y_train)
    rf_proba = rf.predict_proba(X_test)[:, 1]
    evaluate("Random Forest", y_test, rf_proba, ax)

    ax.set_xlabel("False positive rate")
    ax.set_ylabel("True positive rate")
    ax.set_title("ROC curve — churn models")
    ax.legend(loc="lower right")
    fig.tight_layout()
    fig.savefig(IMG_DIR / "roc_curve.png", dpi=110)
    plt.close(fig)

    # Which features drive churn?
    importances = (
        pd.Series(rf.feature_importances_, index=X.columns)
        .sort_values(ascending=False)
        .head(10)
    )
    fig, ax = plt.subplots(figsize=(6, 4))
    importances.iloc[::-1].plot(kind="barh", color=GREEN, ax=ax)
    ax.set_title("Top 10 churn drivers (Random Forest)")
    ax.set_xlabel("Importance")
    fig.tight_layout()
    fig.savefig(IMG_DIR / "feature_importance.png", dpi=110)
    plt.close(fig)

    print("Top churn drivers:")
    print(importances.round(3).to_string())

    print("\nConfusion matrix (Random Forest, threshold 0.5):")
    print(confusion_matrix(y_test, (rf_proba >= 0.5).astype(int)))


if __name__ == "__main__":
    main()
