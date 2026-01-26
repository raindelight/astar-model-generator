import subprocess
import os
import yaml
import random
from pathlib import Path

PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
RESULTS_DIR = os.path.join(PROJECT_ROOT, "results_ablation_random")
GENERATED_CONFIGS_DIR = os.path.join(PROJECT_ROOT, "generated_configs_ablation")
TEMPLATE_FILE = os.path.join(PROJECT_ROOT, "config_accap.yaml")

USER_HOME = str(Path.home())
GUROBI_LICENSE = os.path.join(USER_HOME, "development", "gurobi.lic")

ITERATIONS = [500]
HEURISTIC = "avg"
SAMPLES_PER_LEVEL = 2

PROBLEMS = {
    "Accap": {
        "model": "models/mznc2024_probs/accap/accap.mzn",
        "data": [
            "models/mznc2024_probs/accap/accap_a3_f20_t10.json",
            "models/mznc2024_probs/accap/accap_a8_f50_t30.json",
            "models/mznc2024_probs/accap/accap_a10_f80_t50.json",
            "models/mznc2024_probs/accap/accap_a20_f160_t90.json",
            "models/mznc2024_probs/accap/accap_a30_f300_t120.json"
        ],
        "sols": "models/accap_sols_a10.csv"
    },
    "Community": {
        "model": "models/mznc2024_probs/community-detection/community-detection.mzn",
        "data": [
            "models/mznc2024_probs/community-detection/Zakhary.s12.k3.dzn"
        ],
        "sols": "models/community.s12.k3.csv"
    }
}

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def get_all_mutations_from_template():
    if not os.path.exists(TEMPLATE_FILE):
        print(f"CRITICAL: Nie znaleziono {TEMPLATE_FILE}")
        exit(1)

    with open(TEMPLATE_FILE, 'r') as f:
        data = yaml.safe_load(f)

    mutations = data.get("mutations", [])
    return [m["type"] for m in mutations]

def generate_subset_config(drop_count, sample_id, allowed_types):
    ensure_dir(GENERATED_CONFIGS_DIR)

    with open(TEMPLATE_FILE, 'r') as f:
        config_data = yaml.safe_load(f)

    original_mutations = config_data.get("mutations", [])
    filtered_mutations = []

    for m in original_mutations:
        if m["type"] in allowed_types:
            filtered_mutations.append(m)

    config_data["mutations"] = filtered_mutations

    filename = f"config_drop_{drop_count}_sample_{sample_id}.yaml"
    path = os.path.join(GENERATED_CONFIGS_DIR, filename)

    with open(path, 'w') as f:
        yaml.dump(config_data, f, default_flow_style=False)

    return path, len(filtered_mutations)

def run_experiment():
    ensure_dir(RESULTS_DIR)

    all_mutation_types = get_all_mutations_from_template()
    total_mutations = len(all_mutation_types)

    print(f"=== START LOSOWEGO WYŁĄCZANIA MUTACJI ===")

    for prob_name, paths in PROBLEMS.items():
        print(f"\n>>> Problem: {prob_name}")

        for iter_count in ITERATIONS:
            max_drop = total_mutations - 1

            for drop_k in range(max_drop + 1):
                current_samples = 1 if drop_k == 0 else SAMPLES_PER_LEVEL

                for sample_i in range(current_samples):

                    disabled_types = random.sample(all_mutation_types, drop_k)
                    allowed_types = [t for t in all_mutation_types if t not in disabled_types]

                    config_path, active_count = generate_subset_config(drop_k, sample_i, allowed_types)

                    run_id = f"{prob_name}_Drop{drop_k}_Sample{sample_i}_i{iter_count}"

                    output_csv = os.path.join(RESULTS_DIR, f"{run_id}.csv")
                    picked_csv = os.path.join(RESULTS_DIR, f"{run_id}_picked.csv")
                    checkpoint = os.path.join(RESULTS_DIR, f"{run_id}.bin")

                    print(f"   [Drop {drop_k}/{total_mutations}] Sample {sample_i+1}: Active={active_count}")

                    data_files = paths["data"]
                    if isinstance(data_files, str):
                        data_files = [data_files]

                    data_args_str = " ".join([f'-d "{path}"' for path in data_files])

                    sbt_args = (
                        f'run {paths["model"]} '
                        f'{data_args_str} '
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
                    except subprocess.CalledProcessError:
                        print(f"!!! BŁĄD w przebiegu {run_id} !!!")

if __name__ == "__main__":
    run_experiment()