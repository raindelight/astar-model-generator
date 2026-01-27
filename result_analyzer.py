import os
import glob
import yaml
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import re
import math
from pathlib import Path

# --- KONFIGURACJA ŚCIEŻEK ---
PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))

# Ścieżka do katalogu z wynikami analizy (.analyze.csv)
RESULTS_DIR = os.path.join(PROJECT_ROOT, "models", "mznc2024_probs", "accap", "analyze")

# Katalog z konfiguracjami (pliki .yaml)
CONFIGS_DIR = os.path.join(PROJECT_ROOT, "generated_configs_ablation")

# Plik wyjściowy z podsumowaniem
SUMMARY_CSV = os.path.join(PROJECT_ROOT, "ablation_summary.csv")

# Wzorzec plików
CSV_PATTERN = "**/*.analyze.csv"

def extract_metadata_from_filename(filename):
    """Parsuje nazwę pliku, np. Accap_Drop2_Sample1... -> (2, 1)"""
    match = re.search(r"Drop[_]?(\d+)_Sample[_]?(\d+)", filename, re.IGNORECASE)
    if match:
        return int(match.group(1)), int(match.group(2))
    return None, None

def load_config_mutations(drop_k, sample_i):
    """Ładuje odpowiedni plik YAML i zwraca zestaw włączonych mutacji."""
    expected_name = f"config_drop_{drop_k}_sample_{sample_i}.yaml"
    config_path = os.path.join(CONFIGS_DIR, expected_name)

    if not os.path.exists(config_path):
        candidates = glob.glob(os.path.join(CONFIGS_DIR, f"*rop*{drop_k}*ample*{sample_i}*.yaml"))
        if candidates:
            config_path = candidates[0]
        else:
            return None

    try:
        with open(config_path, 'r') as f:
            data = yaml.safe_load(f)
            return set([m['type'] for m in data.get('mutations', [])])
    except Exception:
        return None

def collect_data_from_files():
    """Skanuje RESULTS_DIR, paruje pliki z configami i tworzy DataFrame."""
    print(f"--- Skanowanie wyników w: {RESULTS_DIR} ---")

    if not os.path.exists(RESULTS_DIR):
        print(f"BŁĄD: Katalog {RESULTS_DIR} nie istnieje!")
        return pd.DataFrame()

    csv_files = glob.glob(os.path.join(RESULTS_DIR, CSV_PATTERN), recursive=True)
    print(f"Znaleziono {len(csv_files)} plików .analyze.csv.")

    if not csv_files:
        return pd.DataFrame()

    data_rows = []
    all_known_mutations = set()

    for filepath in csv_files:
        filename = os.path.basename(filepath)
        drop_k, sample_i = extract_metadata_from_filename(filename)

        if drop_k is None:
            continue

        active_mutations = load_config_mutations(drop_k, sample_i)
        if active_mutations is None:
            continue

        all_known_mutations.update(active_mutations)

        try:
            df = pd.read_csv(filepath)
        except Exception:
            continue

        # --- Obliczanie metryk ---
        avg_complexity = df['complexity'].mean() if 'complexity' in df.columns else 0

        sim_cols = [c for c in df.columns if c.startswith("Constraint_") or "target" in c.lower()]
        if not sim_cols:
            sim_cols = df.select_dtypes(include=['float', 'int']).columns.tolist()
            if 'complexity' in sim_cols: sim_cols.remove('complexity')

        if sim_cols:
            df['best_match'] = df[sim_cols].max(axis=1)
            max_similarity = df['best_match'].max()
        else:
            max_similarity = 0.0

        data_rows.append({
            "File": filename,
            "Drop_Count": drop_k,
            "Sample_ID": sample_i,
            "Active_Mutations": active_mutations,
            "Max_Similarity": max_similarity,
            "Avg_Complexity": avg_complexity
        })

    results_df = pd.DataFrame(data_rows)

    if results_df.empty:
        return results_df

    def get_disabled(row):
        return list(all_known_mutations - row['Active_Mutations'])

    results_df['Disabled_List'] = results_df.apply(get_disabled, axis=1)
    # results_df['Disabled_Mutations_Str'] = results_df['Disabled_List'].apply(lambda x: ", ".join(sorted(x)))

    results_df.sort_values(by=['Drop_Count', 'Sample_ID'], inplace=True)
    return results_df

def calculate_global_importance(df):
    """
    Oblicza 'Feature Importance' dla mutacji.
    ZMIANA: Bierze pod uwagę średnią z TOP 5% wyników.
    """
    all_mutations = set()
    for sublist in df['Disabled_List']:
        all_mutations.update(sublist)

    if not all_mutations:
        return pd.DataFrame()

    # Funkcja pomocnicza do liczenia średniej z top 5%
    def get_top_5_percent_mean(series):
        if series.empty: return 0.0
        # Sortujemy malejąco (najlepsze wyniki na górze)
        sorted_vals = series.sort_values(ascending=False)
        count = len(sorted_vals)
        if count == 0: return 0.0

        # Bierzemy top 5%
        cutoff = math.ceil(count * 0.01)
        # Zawsze bierzemy przynajmniej 1 wynik, nawet przy małej próbie
        if cutoff < 1: cutoff = 1

        top_subset = sorted_vals.iloc[:cutoff]
        return top_subset.mean()

    impact_data = []
    for mut in all_mutations:
        disabled_mask = df['Disabled_List'].apply(lambda x: mut in x)
        enabled_mask = ~disabled_mask

        # Używamy nowej funkcji uśredniającej TOP 5%
        mean_disabled = get_top_5_percent_mean(df[disabled_mask]['Max_Similarity'])
        mean_enabled = get_top_5_percent_mean(df[enabled_mask]['Max_Similarity'])

        if pd.isna(mean_disabled): mean_disabled = 0
        if pd.isna(mean_enabled): mean_enabled = 0

        impact = mean_enabled - mean_disabled
        short_name = mut.split(".")[-1]

        impact_data.append({
            "Mutation": short_name,
            "Impact": impact
        })

    return pd.DataFrame(impact_data).sort_values(by="Impact", ascending=False)

def create_plots(df):
    sns.set_theme(style="whitegrid")
    os.makedirs(PROJECT_ROOT, exist_ok=True)

    # 1. Globalny Ranking (Barplot) - teraz na podstawie top 5%
    importance_df = calculate_global_importance(df)
    if not importance_df.empty:
        plt.figure(figsize=(12, 8))
        sns.barplot(x="Impact", y="Mutation", data=importance_df, palette="coolwarm")
        plt.title('Wpływ mutacji na jakość (Top 5% wyników)\n(Impact > 0: Mutacja pomaga)')
        plt.xlabel('Zysk Max Similarity (Włączona - Wyłączona)')
        plt.axvline(0, color='black', linewidth=1)
        plt.tight_layout()
        plt.savefig(os.path.join(PROJECT_ROOT, "plot_global_mutation_importance.png"))
        print("Zapisano: plot_global_mutation_importance.png")

    # 2. Similarity vs Drop Count (Boxplot)
    plt.figure(figsize=(10, 6))
    sns.boxplot(x='Drop_Count', y='Max_Similarity', data=df, palette="viridis")
    plt.title('Spadek jakości przy wyłączaniu mutacji')
    plt.tight_layout()
    plt.savefig(os.path.join(PROJECT_ROOT, "plot_similarity_box.png"))
    print("Zapisano: plot_similarity_box.png")

    # 3. Complexity (Log Scale Boxplot)
    plt.figure(figsize=(10, 6))
    sns.boxplot(x='Drop_Count', y='Avg_Complexity', data=df, palette="rocket")
    plt.yscale("log")
    plt.title('Złożoność kodu (skala logarytmiczna)')
    plt.tight_layout()
    plt.savefig(os.path.join(PROJECT_ROOT, "plot_complexity_box_log.png"))
    print("Zapisano: plot_complexity_box_log.png")

    # 4. Trade-off Scatter
    plt.figure(figsize=(10, 8))
    sns.scatterplot(
        data=df, x='Avg_Complexity', y='Max_Similarity',
        hue='Drop_Count', palette="deep", size='Drop_Count', sizes=(20, 200), alpha=0.7
    )
    plt.title('Jakość vs Złożoność')
    plt.tight_layout()
    plt.savefig(os.path.join(PROJECT_ROOT, "plot_tradeoff_scatter.png"))
    print("Zapisano: plot_tradeoff_scatter.png")

if __name__ == "__main__":
    df = collect_data_from_files()

    if not df.empty:
        save_df = df.drop(columns=['Active_Mutations', 'Disabled_List'])
        save_df.to_csv(SUMMARY_CSV, index=False)
        print(f"Dane zapisano do: {SUMMARY_CSV}")
        create_plots(df)
    else:
        print("Nie znaleziono danych lub wystąpił błąd podczas skanowania.")