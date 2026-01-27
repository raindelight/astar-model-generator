import subprocess
import os
from pathlib import Path
import datetime

PROJECT_ROOT = os.path.dirname(os.path.abspath(__file__))

RESULTS_DIR = os.path.join(PROJECT_ROOT, "experiment_results")

USER_HOME = str(Path.home())
GUROBI_LICENSE = os.path.join(USER_HOME, "development", "gurobi.lic")

ITERATIONS = [1000, 10000]
HEURISTICS = ["avg", "min", "mse", "var"]

PROBLEMS = {
    "Accap": {
        "model": "models/mznc2024_probs/accap/accap.mzn",
        "data": ["models/accap_a10_f80_t50.json", "models/accap_a3_f20_t10.json", "models/accap_a8_f50_t30.json"],
        "sols": ["models/accap_sols_a10.csv", "models/accap.a3.csv", "models/accap.a8.csv"],
        "config_path": "config_accap.yaml"
    }
}
'''
    "Network": {
        "model": "models/mznc2024_probs/network_50_cstr/efm_cstr.mzn",
        "data": "models/mznc2024_probs/network_50_cstr/MODEL1507180015.dzn",
        "sols": "models/network-50.MODEL1507180015.csv"
    },
    "Community": {
        "model": "models/mznc2024_probs/community-detection/community-detection.mzn",
        "data": "models/mznc2024_probs/community-detection/Zakhary.s12.k3.dzn",
        "sols": "models/community.s12.k3.csv"
    },
    "Concert": {
        "model": "models/mznc2024_probs/concert-hall-cap/concert-hall-cap.mzn",
        "data": "models/mznc2024_probs/concert-hall-cap/concert_60_50.json",
        "sols": "models/concert.60_50.csv"
    },
    "Hoist": {
        "model": "models/mznc2024_probs/hoist-benchmark/hoist-benchmark.mzn",
        "data": "models/mznc2024_probs/hoist-benchmark/PU_2_2_3.dzn",
        "sols": "models/hoist.2_2_3.csv"
    }
}

'''
def ensure_dir(directory):
    if not os.path.exists(directory):
        os.makedirs(directory)

def run_experiment():
    ensure_dir(RESULTS_DIR)

    total_experiments = len(PROBLEMS) * len(ITERATIONS) * len(HEURISTICS)
    current_experiment = 0

    print(f"#### rozpoczynam testy: {total_experiments} przebiegów #### {datetime.datetime.now()} ####")
    print(f"katalog domowy: {USER_HOME}")
    print(f"licencja Gurobi: {GUROBI_LICENSE}")

    for prob_name, paths in PROBLEMS.items():
        for iter_count in ITERATIONS:
            for heuristic in HEURISTICS:
                current_experiment += 1

                base_name = f"{prob_name}_{heuristic}_i{iter_count}"

                output_csv_path = os.path.join(RESULTS_DIR, f"{base_name}.csv")

                picked_csv_path = os.path.join(RESULTS_DIR, f"{base_name}_picked.csv")

                checkpoint_path = os.path.join(RESULTS_DIR, f"{base_name}.bin")

                print(f"\n[{current_experiment}/{total_experiments}] Problem: {prob_name} | Iter: {iter_count} | Heur: {heuristic}")

                sbt_args = \
                    f'run {paths["model"]} ' + \
                    " ".join([f'-D {x} ' for x in paths["data"]]) + \
                    " ".join([f'-s {x} ' for x in paths["sols"]]) + \
                    f'-i {iter_count} ' + \
                    f'-e {heuristic} ' + \
                    f'-o "{output_csv_path}" ' + \
                    f'-p "{picked_csv_path}" ' + \
                    f'-c "{checkpoint_path}" ' + \
                    f'-C "{paths["config_path"]}" ' + \
                    f'--gurobi-license "{GUROBI_LICENSE}"'

                full_command = f'sbt "{sbt_args}"'

                try:
                    subprocess.run(full_command, shell=True, cwd=PROJECT_ROOT, check=True)
                except subprocess.CalledProcessError as e:
                    print(f"error: eksperyment {base_name} nie powiódł się")
                    print(e)

    print("\n#### wszystkie eksperymenty zakończone ####")

if __name__ == "__main__":
    run_experiment()