#!/usr/bin/env python3
"""Clone reform data for taxmax combos.

All current worker types earn below the taxmax, so taxmax reforms produce
results identical to current law. This script:
  - For standalone taxmax combos: copies baseline male_single data (birth years 1970+)
  - For cross-category combos with taxmax: copies from the non-taxmax equivalent

Updates all 3 JSON files per worker type: cohort, individual, nmtr.
"""

import json
import copy
import os

DATA_DIR = "docs/data"
REFORM_DIR = os.path.join(DATA_DIR, "reform")

WORKER_TYPES = ["very_low", "low", "medium", "high", "max", "custom_50k"]
TAXMAX_REFORMS = ["taxmax_90_pct", "eliminate_taxmax", "eliminate_taxmax_no_credit"]

# Category order for combo keys (must match JS getActiveComboKey)
CATEGORY_ORDER = ["pia", "nra", "cola", "taxmax"]

# Birth years for standalone taxmax combos (skip 1940, 1950, 1960)
TAXMAX_BIRTH_YEARS = [1970, 1980, 1990, 2000, 2010]
# NMTR uses same birth years as combo phase (whatever exists in baseline)
NMTR_BIRTH_YEARS_FILTER = {str(y) for y in TAXMAX_BIRTH_YEARS}


def load_json(path):
    with open(path) as f:
        return json.load(f)


def write_json(path, data):
    with open(path, "w") as f:
        json.dump(data, f, separators=(",", ":"))
    print(f"  Written: {path} ({os.path.getsize(path) // 1024} KB)")


def get_baseline_cohort(baseline_data, birth_years):
    """Extract male_single cohort data filtered to given birth years."""
    src = baseline_data["data"]["male_single"]
    indices = [i for i, by in enumerate(src["birth_years"]) if by in birth_years]
    return {
        "birth_years": [src["birth_years"][i] for i in indices],
        "monthly_benefit": [src["monthly_benefit"][i] for i in indices],
        "pv_benefits": [src["pv_benefits"][i] for i in indices],
        "pv_taxes": [src["pv_taxes"][i] for i in indices],
        "ratio": [src["ratio"][i] for i in indices],
        "irr": [src["irr"][i] for i in indices],
        "repl_rate": [src["repl_rate"][i] for i in indices],
        "initial_real_benefit": [src["initial_real_benefit"][i] for i in indices],
        "death_age": [src["death_age"][i] for i in indices],
    }


def get_baseline_individual(baseline_data, birth_years):
    """Extract male_single individual data filtered to given birth years."""
    src = baseline_data["data"]["male_single"]
    return {str(by): copy.deepcopy(src[str(by)])
            for by in birth_years if str(by) in src}


def get_baseline_nmtr(baseline_data, birth_years_filter):
    """Extract male_single NMTR data filtered to given birth years."""
    src = baseline_data["data"]["male_single"]
    return {by: copy.deepcopy(src[by])
            for by in src if by in birth_years_filter}


def strip_taxmax_from_key(combo_key):
    """Remove the taxmax reform from a combo key, returning the non-taxmax equivalent."""
    parts = combo_key.split("+")
    non_taxmax = [p for p in parts if p not in TAXMAX_REFORMS]
    return "+".join(non_taxmax) if non_taxmax else None


def generate_taxmax_combo_keys(existing_keys):
    """Generate all new combo keys that include a taxmax reform."""
    new_keys = []
    # Standalone taxmax combos
    for tr in TAXMAX_REFORMS:
        new_keys.append(tr)
    # Cross-category: append each taxmax reform to each existing combo
    for existing in existing_keys:
        for tr in TAXMAX_REFORMS:
            new_keys.append(f"{existing}+{tr}")
    return new_keys


def main():
    for wtype in WORKER_TYPES:
        print(f"\nProcessing {wtype}...")

        # Load existing reform data
        cohort_path = os.path.join(REFORM_DIR, "cohort", f"{wtype}.json")
        ind_path = os.path.join(REFORM_DIR, "individual", f"{wtype}.json")
        nmtr_path = os.path.join(REFORM_DIR, "individual", f"{wtype}_nmtr.json")

        cohort = load_json(cohort_path)
        individual = load_json(ind_path)
        nmtr = load_json(nmtr_path)

        # Load baseline data
        baseline_cohort = load_json(os.path.join(DATA_DIR, "cohort", f"{wtype}.json"))
        baseline_ind = load_json(os.path.join(DATA_DIR, "individual", f"{wtype}_benefits.json"))
        baseline_nmtr = load_json(os.path.join(DATA_DIR, "individual", f"{wtype}_nmtr.json"))

        # Get existing combo keys
        existing_keys = list(cohort["data"].keys())

        # Precompute baseline data for standalone taxmax combos
        bl_cohort = get_baseline_cohort(baseline_cohort, TAXMAX_BIRTH_YEARS)
        bl_ind = get_baseline_individual(baseline_ind, TAXMAX_BIRTH_YEARS)
        bl_nmtr = get_baseline_nmtr(baseline_nmtr, NMTR_BIRTH_YEARS_FILTER)

        # Generate new combo keys
        new_keys = generate_taxmax_combo_keys(existing_keys)
        print(f"  Adding {len(new_keys)} taxmax combos to {len(existing_keys)} existing")

        for new_key in new_keys:
            source_key = strip_taxmax_from_key(new_key)

            if source_key is None:
                # Standalone taxmax combo — use baseline
                cohort["data"][new_key] = copy.deepcopy(bl_cohort)
                individual["data"][new_key] = copy.deepcopy(bl_ind)
                nmtr["data"][new_key] = copy.deepcopy(bl_nmtr)
            else:
                # Cross-category combo — clone from non-taxmax equivalent
                if source_key in cohort["data"]:
                    cohort["data"][new_key] = copy.deepcopy(cohort["data"][source_key])
                if source_key in individual["data"]:
                    individual["data"][new_key] = copy.deepcopy(individual["data"][source_key])
                if source_key in nmtr["data"]:
                    nmtr["data"][new_key] = copy.deepcopy(nmtr["data"][source_key])

        # Update metadata
        all_keys = existing_keys + new_keys
        cohort["meta"]["categories"] = CATEGORY_ORDER
        cohort["meta"]["combo_keys"] = all_keys

        # Write updated files
        write_json(cohort_path, cohort)
        write_json(ind_path, individual)
        write_json(nmtr_path, nmtr)

    print(f"\nDone. Added {len(new_keys)} taxmax combos per worker type.")
    print(f"Total combos per worker: {len(existing_keys) + len(new_keys)}")


if __name__ == "__main__":
    os.chdir(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
    main()
