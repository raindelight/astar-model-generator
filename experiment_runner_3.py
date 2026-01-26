import subprocess
import os
import yaml  # Wymaga: pip install pyyaml
from pathlib import Path

# --- KONFIGURACJA ŚCIEŻEK ---
PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
RESULTS_DIR = os.path.join(PROJECT_ROOT, "experiment_results_mutations")
GENERATED_CONFIGS_DIR = os.path.join(PROJECT_ROOT, "generated_configs")

TEMPLATE_FILE = os.path.join(PROJECT_ROOT, "config_template.yaml")

USER_HOME = str(Path.home())
GUROBI_LICENSE = os.path.join(USER_HOME, "development", "gurobi.lic")

ITERATIONS = [100, 500, 1000]
HEURISTIC = "avg"

# Definicje scenariuszy mutacji.
# Klucz: Nazwa scenariusza (używana w nazwach plików).
# Wartość: Lista ciągów znaków. Jeśli typ mutacji (z pola 'type' w yaml) zawiera
# którykolwiek z tych ciągów, mutacja zostaje włączona.
MUTATION_SETS = {
    # 1. Wszystkie mutacje dostępne w szablonie (w tym Crossover, jeśli jest w configu)
    "All": ["ALL"],

    # 2. Brak mutacji (tylko generacja początkowa + ewentualnie Crossover, jeśli nie jest traktowany jako Mutation w YAML)
    # Jeśli Crossover jest na liście mutacji w YAML, to też zostanie wyłączony.
    "None": [],

    # 3. Zmiany tylko na poziomie liści (zmienne i stałe)
    "Leaf_Tweaks": [
        "ChangeVariable",
        "TransformVariableToConstant",
        "TransformConstantToVariable"
    ],

    # 4. Zmiany strukturalne
    "Structural_Changes": [
        "ReplaceOperator",
        "ReplaceSubtree"
    ],

    # 5. Bez niszczenia poddrzew (Bez ReplaceSubtree)
    "No_Random_Subtree": [
        "ChangeVariable",
        "TransformVariableToConstant",
        "TransformConstantToVariable",
        "ReplaceOperator",
        "GenerateAllDiffn"
    ],

    # 6. Specyficzne dla problemów geometrycznych/pakowania (jeśli dotyczy)
    #"Diffn_Focus": ["GenerateAllDiffn"]
}

PROBLEMS = {
    "Accap": {
        "model": "models/mznc2024_probs/accap/accap.mzn",
        "data": "models/accap_a10_f80_t50.json",
        "sols": "models/accap_sols_a10.csv"
    },
    "Community": {
        "model": "models/mznc2024_probs/community-detection/community-detection.mzn",
        "data": "models/mznc2024_probs/community-detection/Zakhary.s12.k3.dzn",
        "sols": "models/community.s12.k3.csv"
    },
}

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def generate_config_file(scenario_name, allowed_keys):
    ensure_dir(GENERATED_CONFIGS_DIR)

    if not os.path.exists(TEMPLATE_FILE):
        print(f"CRITICAL ERROR: Nie znaleziono pliku {TEMPLATE_FILE}!")
        print("Skopiuj swój 'config.yaml' jako 'config_template.yaml' przed uruchomieniem.")
        exit(1)

    with open(TEMPLATE_FILE, 'r') as f:
        config_data = yaml.safe_load(f)

    if "ALL" in allowed_keys:
        pass
    else:
        original_mutations = config_data.get("mutations", [])
        filtered_mutations = []

        for m in original_mutations:
            m_type = m.get("type", "")

            if any(key in m_type for key in allowed_keys):
                filtered_mutations.append(m)

        config_data["mutations"] = filtered_mutations

    output_filename = f"config_{scenario_name}.yaml"
    output_path = os.path.join(GENERATED_CONFIGS_DIR, output_filename)

    with open(output_path, 'w') as f:
        yaml.dump(config_data, f, default_flow_style=False)

    return output_path, len(config_data.get("mutations", []))

def run_experiment():
    ensure_dir(RESULTS_DIR)

    total_runs = len(PROBLEMS) * len(ITERATIONS) * len(MUTATION_SETS)
    current_run = 0

    print(f"#### rozpoczynam eksperyment 3: {total_runs} przebiegów ####")

    for prob_name, paths in PROBLEMS.items():

        for mut_set_name, allowed_keys in MUTATION_SETS.items():

            config_path, mut_count = generate_config_file(mut_set_name, allowed_keys)

            for iter_count in ITERATIONS:
                current_run += 1

                base_name = f"{prob_name}_{mut_set_name}_i{iter_count}"
                output_csv = os.path.join(RESULTS_DIR, f"{base_name}.csv")
                picked_csv = os.path.join(RESULTS_DIR, f"{base_name}_picked.csv")
                checkpoint = os.path.join(RESULTS_DIR, f"{base_name}.bin")

                print(f"\n[{current_run}/{total_runs}] {prob_name} | Mut: {mut_set_name} (n={mut_count}) | Iter: {iter_count}")
                print(f"   Config: {config_path}")

                sbt_args = (
                    f'run {paths["model"]} '
                    f'-D {paths["data"]} '
                    f'-s {paths["sols"]} '
                    f'-i {iter_count} '
                    f'-e {HEURISTIC} '
                    f'-o "{output_csv}" '
                    f'-p "{picked_csv}" '
                    f'-c "{checkpoint}" '
                    f'--config "{config_path}" '
                    f'--gurobi-license "{GUROBI_LICENSE}"'
                )

                full_command = f'sbt "{sbt_args}"'

                try:
                    subprocess.run(full_command, shell=True, cwd=PROJECT_ROOT, check=True)
                    print("#### pomyślne zakończenie eksperymentu ####")
                except subprocess.CalledProcessError:
                    print(f"błąd: eksperyment {base_name} zakończony niepowodzeniem")

if __name__ == "__main__":
    run_experiment()