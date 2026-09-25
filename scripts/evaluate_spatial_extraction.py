"""Evaluate paired synthetic interactive and flattened extraction routes."""

from __future__ import annotations

import argparse
import csv
from collections import Counter, defaultdict
from pathlib import Path

import sys

try:
    sys.stdout.reconfigure(errors="backslashreplace")
except AttributeError:
    pass


def normalize(value):
    if value is None:
        return None
    value = str(value).strip()
    return value[1:] if value.startswith("/") else value


def read_truth(path):
    with path.open(newline="", encoding="utf-8-sig") as stream:
        return {row["field"]: {**row, "value_normalized": normalize(row["value"]), "field_type": normalize(row["field_type"])} for row in csv.DictReader(stream)}


def field_map(rows):
    return {row["field"]: row for row in rows}


def compare(truth, observed):
    exact = []
    wrong = []
    missed = []
    for field, expected in truth.items():
        actual = observed.get(field)
        actual_value = normalize(actual.get("value") if actual else None)
        if actual_value == expected["value_normalized"]:
            exact.append(field)
        elif actual is None or not actual.get("is_populated", False):
            missed.append(field)
        else:
            wrong.append((field, expected["value_normalized"], actual_value))
    false_positive = [
        (field, row.get("value"))
        for field, row in observed.items()
        if row.get("is_populated", False) and field not in truth
    ]
    by_type = {}
    for field_type in sorted({row["field_type"] for row in truth.values()}):
        fields = {field: row for field, row in truth.items() if row["field_type"] == field_type}
        by_type[field_type] = {"truth": len(fields), "exact": sum(field in exact for field in fields), "missed": sum(field in missed for field in fields), "wrong": sum(field in {item[0] for item in wrong} for field in fields)}
    return {"truth": len(truth), "exact": len(exact), "wrong": len(wrong), "missed": len(missed), "false_positive": len(false_positive), "by_type": by_type, "wrong_fields": wrong, "missed_fields": missed, "false_positive_fields": false_positive}


def write_csv(path, rows, columns):
    with path.open("w", newline="", encoding="utf-8") as stream:
        writer = csv.DictWriter(stream, fieldnames=columns)
        writer.writeheader()
        writer.writerows(rows)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--input-dir", default="local/synthetic_form_testset_20260924")
    parser.add_argument("--template-root", default="inst/extdata/templates")
    parser.add_argument("--output-dir", default="local/spatial_evaluation")
    args = parser.parse_args()
    input_dir = Path(args.input_dir)
    template_root = Path(args.template_root)
    output_dir = Path(args.output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    sys.path.insert(0, "inst/python")
    import acroform
    import spatial_pdf

    cases = [
        ("initial_epi", "synthetic_epi_interactive.pdf", "synthetic_epi_print.pdf", "synthetic_epi_ground_truth.csv", template_root / "initial_epi" / "2024-05-28"),
        ("bcap", "synthetic_bcap_audit_interactive.pdf", "synthetic_bcap_audit_print.pdf", "synthetic_bcap_audit_ground_truth.csv", template_root / "bcap" / "2025-12-08"),
    ]
    summary_rows = []
    report_lines = ["# Synthetic spatial extraction evaluation", "", "The interactive route uses the existing AcroForm extractor; the printed route uses the versioned spatial-template extractor.", ""]
    for family, interactive_name, printed_name, truth_name, template in cases:
        truth = read_truth(input_dir / truth_name)
        interactive = acroform.extract_form(str(input_dir / interactive_name))
        spatial = spatial_pdf.extract_spatial(str(input_dir / printed_name), str(template))
        interactive_comparison = compare(truth, field_map(interactive["fields"]))
        spatial_comparison = compare(truth, field_map(spatial["fields"]))
        blank = spatial_pdf.extract_spatial(str(template / "blank_printed.pdf"), str(template))
        blank_false_positive = sum(row.get("is_populated", False) for row in blank["fields"])
        blank_selected = sum(row.get("field_type") == "Btn" and normalize(row.get("value")) not in (None, "Off") for row in blank["fields"])
        report_lines.extend([
            f"## {family}",
            "",
            f"- Canonical fields: {spatial['number_of_fields']}",
            f"- Ground-truth populated fields: {len(truth)}",
            f"- Interactive exact normalized agreement: {interactive_comparison['exact']}/{len(truth)}",
            f"- Flattened exact normalized agreement: {spatial_comparison['exact']}/{len(truth)}",
            f"- Flattened missed populated fields: {spatial_comparison['missed']}",
            f"- Flattened incorrect populated values: {spatial_comparison['wrong']}",
            f"- Flattened false-positive populated fields: {spatial_comparison['false_positive']}",
            f"- Registration: {spatial['registration_quality']}; anchor agreement {spatial['anchor_fraction']:.6f}; residual {spatial['registration_residual_pt']:.6f} pt",
            f"- Blank printed false-positive populated fields: {blank_false_positive}; selected controls: {blank_selected}",
            "",
            "Field-type agreement:",
            "",
        ])
        for field_type, counts in spatial_comparison["by_type"].items():
            report_lines.append(f"- {field_type}: {counts['exact']}/{counts['truth']} exact; missed={counts['missed']}; wrong={counts['wrong']}")
        report_lines.extend(["", "Missed fields:", "", "```text", ", ".join(spatial_comparison["missed_fields"]) or "(none)", "```", "", "Incorrect fields:", "", "```text", repr(spatial_comparison["wrong_fields"]), "```", ""])
        for route, comparison in (("interactive", interactive_comparison), ("spatial", spatial_comparison)):
            for field_type, counts in comparison["by_type"].items():
                summary_rows.append({"form_family": family, "route": route, "field_type": field_type, **counts})
    write_csv(output_dir / "agreement_by_type.csv", summary_rows, ["form_family", "route", "field_type", "truth", "exact", "missed", "wrong"])
    (output_dir / "evaluation_report.md").write_text("\n".join(report_lines), encoding="utf-8")
    print("\n".join(report_lines))


if __name__ == "__main__":
    main()
