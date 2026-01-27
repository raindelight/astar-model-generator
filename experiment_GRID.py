import subprocess
import os
from pathlib import Path
import datetime
import itertools # <--- Required for Grid Search

PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))
RESULTS_DIR = os.path.join(PROJECT_ROOT, "experiment_results_GRID_3") # Changed folder name
CONFIG_DIR = os.path.join(PROJECT_ROOT, "experiment_configs")
CONFIG_TEMPLATE_FILE = os.path.join(PROJECT_ROOT, "config_accap_t.yaml")

USER_HOME = str(Path.home())
GUROBI_LICENSE = os.path.join(USER_HOME, "development", "gurobi.lic")

# 1. Setup
ITERATIONS = [1000, 3000]
HEURISTICS = ["avg"] # "min" is often noisy; "avg" is usually better for thesis baselines.

# 2. GRID SEARCH DEFINITION
GRID_SEARCH_RANGES = {
    # SCENARIO 1: PARSIMONY PRESSURE
    # 0.1 = Almost no penalty (allow bloat)
    # 2.0 = Moderate penalty
    # 10.0 = High penalty (force very short, simple logic)
    "heuristic_penalty_complexity": [0.1, 2.0, 10.0],

    # SCENARIO 2: DIVERSITY PRESSURE
    # 0.0 = No pressure (standard GP, risk of identical population)
    # 100.0 = Strong pressure (forces exploration)
    "heuristic_penalty_identical":  [0.0, 100.0],

    # SCENARIO 3: ALGORITHMIC PREFERENCE
    # 0.5 = Loops are 50% cheaper (encourages using loops)
    # 1.0 = Loops cost same as other nodes (neutral)
    "heuristic_loop_discount_g":    [0.5, 1.0],

    # CONSTANT (Control Variable)
    # Fix this to reduce the combinatorial explosion.
    # 1000.0 is usually sufficient precision.
    "heuristic_scaling":            [1000.0]
}

# Parameters that stay constant for all runs
BASE_PARAMS = {
    "heuristic_penalty_frequency": 5.0,
    "heuristic_loop_discount_h": 0.3,
}


PROBLEMS = {
    "Accap": {
        "model": "models/mznc2024_probs/accap/accap.mzn",
        "data": ["models/accap_a10_f80_t50.json", "models/accap_a3_f20_t10.json"],
        "sols": ["models/accap_sols_a10.csv", "models/accap.a3.csv"]
    }
}

# 3. Load Template
if os.path.exists(CONFIG_TEMPLATE_FILE):
    with open(CONFIG_TEMPLATE_FILE, 'r') as f:
        CONFIG_TEMPLATE = f.read()
else:
    # Fallback if file not found (for safety)
    CONFIG_TEMPLATE = "algorithm:\n  expression-weights:\n{weights_block}"

def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def create_config_file(run_id, params):
    """Generates a config file for a specific grid combination."""
    ensure_dir(CONFIG_DIR)

    # Merge Base Params with the Current Grid Combination
    # Grid values overwrite base values if keys overlap
    final_params = {**BASE_PARAMS, **params}

    # Format into YAML
    weights_block = ""
    for key, value in final_params.items():
        weights_block += f"  {key}: {value}\n"

    content = CONFIG_TEMPLATE.replace("{weights_block}", weights_block)

    filename = os.path.join(CONFIG_DIR, f"config_{run_id}.yaml")
    with open(filename, "w") as f:
        f.write(content)

    return filename

def generate_short_id(params):
    """Creates a readable filename ID from the changing parameters."""
    # Abbreviations to keep filenames short
    abbr = {
        "heuristic_penalty_complexity": "C",
        "heuristic_penalty_identical": "I",
        "heuristic_loop_discount_g": "LG",
        "heuristic_scaling": "S"
    }

    parts = []
    for k, v in params.items():
        key_short = abbr.get(k, k)
        parts.append(f"{key_short}{v}")
    return "_".join(parts)

def run_experiment():
    ensure_dir(RESULTS_DIR)

    # --- GENERATE COMBINATIONS ---
    keys, values = zip(*GRID_SEARCH_RANGES.items())
    # itertools.product creates every possible combination of the lists
    combinations = [dict(zip(keys, v)) for v in itertools.product(*values)]

    total_experiments = len(PROBLEMS) * len(ITERATIONS) * len(HEURISTICS) * len(combinations)
    current_experiment = 0

    print(f"#### Starting Grid Search: {len(combinations)} parameter sets x {len(PROBLEMS)} problems = {total_experiments} runs ####")
    print(f"#### Variations: {keys} ####")

    for prob_name, paths in PROBLEMS.items():
        for iter_count in ITERATIONS:
            for heuristic in HEURISTICS:
                for param_set in combinations:
                    current_experiment += 1

                    # Create a unique ID for this specific combination
                    # e.g., "C5.0_I200.0_LG0.5_S1000.0"
                    param_id = generate_short_id(param_set)
                    run_id = f"{prob_name}_{heuristic}_{param_id}"

                    config_path = create_config_file(run_id, param_set)

                    # Construct file paths
                    base_name = f"{run_id}_i{iter_count}"
                    output_csv_path = os.path.join(RESULTS_DIR, f"{base_name}.csv")
                    picked_csv_path = os.path.join(RESULTS_DIR, f"{base_name}_picked.csv")
                    checkpoint_path = os.path.join(RESULTS_DIR, f"{base_name}.bin")

                    print(f"\n[{current_experiment}/{total_experiments}] {base_name}")
                    print(f"   > Params: {param_set}")

                    sbt_args = \
                        f'run {paths["model"]} ' + \
                        " ".join([f'-D {x} ' for x in paths["data"]]) + \
                        " ".join([f'-s {x} ' for x in paths["sols"]]) + \
                        f'-i {iter_count} ' + \
                        f'-e {heuristic} ' + \
                        f'-o "{output_csv_path}" ' + \
                        f'-p "{picked_csv_path}" ' + \
                        f'-c "{checkpoint_path}" ' + \
                        f'-C "{config_path}" ' + \
                        f'--gurobi-license "{GUROBI_LICENSE}"'

                    full_command = f'sbt "{sbt_args}"'

                    try:
                        subprocess.run(full_command, shell=True, cwd=PROJECT_ROOT, check=True)
                    except subprocess.CalledProcessError as e:
                        print(f"   !!! Error: Experiment {base_name} failed")
                        print(e)

    print("\n#### Grid Search Completed ####")

if __name__ == "__main__":
    run_experiment()