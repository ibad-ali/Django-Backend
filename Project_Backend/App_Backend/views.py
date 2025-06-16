from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.parsers import MultiPartParser
import tempfile
import os
import subprocess
import pandas as pd
import json
import numpy as np
import math
from scipy.stats import zscore

def parse_column_list(value):
    if not value:
        return []
    if isinstance(value, list):
        return value
    try:
        # Try parsing as JSON array string
        return json.loads(value)
    except json.JSONDecodeError:
        # Fallback: comma-separated string
        return [v.strip() for v in value.split(',') if v.strip()]

def make_json_serializable(obj):
    """Recursively make an object JSON serializable by handling all edge cases"""
    if obj == "null":  # Handle string "null" values
        return None
    elif isinstance(obj, (int, float)):
        if math.isinf(obj) or math.isnan(obj):
            return None
        return int(obj) if isinstance(obj, float) and obj.is_integer() else obj
    elif isinstance(obj, dict):
        return {k: make_json_serializable(v) for k, v in obj.items()}
    elif isinstance(obj, (list, tuple)):
        return [make_json_serializable(item) for item in obj]
    elif pd.isna(obj):
        return None
    elif isinstance(obj, str) and obj.lower() in ['nan', 'inf', '-inf', 'null']:
        return None
    else:
        return obj


class UploadFileView(APIView):
    parser_classes = [MultiPartParser]

    def post(self, request):
        file_obj = request.FILES.get("file")

        if not file_obj:
            return Response({"error": "No file uploaded"}, status=400)

        # Determine file extension
        file_ext = os.path.splitext(file_obj.name)[-1].lower()
        if file_ext not in ['.xlsx', '.xls', '.csv']:
            return Response({"error": "Unsupported file type"}, status=400)

        # Save uploaded file temporarily
        suffix = file_ext
        with tempfile.NamedTemporaryFile(delete=False, suffix=suffix) as tmp:
            for chunk in file_obj.chunks():
                tmp.write(chunk)
            tmp_path = tmp.name

        try:
            # Prepare the R script path
            r_script_path = os.path.join("R Functions", "classify_columns.R")

            # Call R script using subprocess
            result = subprocess.run(
                ["Rscript", "--vanilla", r_script_path, tmp_path],
                capture_output=True,
                text=True
            )

            # Check for errors
            if result.returncode != 0:
                error_msg = result.stderr if result.stderr else "Unknown R script error"
                return Response({"error": error_msg}, status=500)

            # Parse the JSON output from R
            response_data = json.loads(result.stdout)

            return Response(response_data)

        except json.JSONDecodeError as e:
            return Response({"error": f"Failed to parse R output: {str(e)}"}, status=500)
        except Exception as e:
            return Response({"error": str(e)}, status=500)
        finally:
            os.remove(tmp_path)


class DatasetSummaryView(APIView):
    parser_classes = [MultiPartParser]

    def post(self, request):
        file_obj = request.FILES.get("file")
        qualitative = parse_column_list(request.data.get("qualitative_columns", []))
        quantitative = parse_column_list(request.data.get("quantitative_columns", []))

        if not file_obj:
            return Response({"error": "No file uploaded"}, status=400)

        file_ext = os.path.splitext(file_obj.name)[-1].lower()
        if file_ext not in ['.csv', '.xlsx', '.xls']:
            return Response({"error": "Unsupported file type"}, status=400)

        with tempfile.NamedTemporaryFile(delete=False, suffix=file_ext) as tmp:
            for chunk in file_obj.chunks():
                tmp.write(chunk)
            tmp_path = tmp.name

        try:
            # Load file and filter columns
            if file_ext == '.csv':
                df = pd.read_csv(tmp_path)
            else:
                df = pd.read_excel(tmp_path)

            selected_columns = qualitative + quantitative
            if not selected_columns:
                return Response({"error": "Please select at least one qualitative or quantitative column."}, status=400)

            # sub_df = df[selected_columns].copy()

            # Clean the data
            for col in df.columns:
                if pd.api.types.is_numeric_dtype(df[col]):
                    df[col] = df[col].replace([np.inf, -np.inf], np.nan)
                df[col] = df[col].apply(lambda x: None if isinstance(x, str) and x.lower() in ['null', 'nan', 'inf', '-inf'] else x)

            sub_df_json = df.to_dict(orient='list')
            sub_df_json = {k: [make_json_serializable(v) for v in vals] for k, vals in sub_df_json.items()}

            # Call the R script using subprocess
            r_script_path = "R Functions/get_dataset_summary.R"
            cmd = ["Rscript", r_script_path, tmp_path]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise Exception(result.stderr)

            # Parse JSON output
            r_output = json.loads(result.stdout)

            return Response({
                "summary": r_output.get("summary"),
                "columns": r_output.get("columns"),
                "frequency_data": r_output.get("frequency_data"),
                "correlation_matrix": r_output.get("correlation_matrix"),
                "filtered_data": sub_df_json
            })

        except Exception as e:
            print("ERROR:", str(e))
            return Response({"error": str(e)}, status=500)
        finally:
            os.remove(tmp_path)


class HandleMissingValuesView(APIView):
    def post(self, request):
        selected_column = request.data.get("selectedColumn")
        missing_method = request.data.get("missingMethod")
        summary_data = request.data.get("summaryData")

        if not selected_column or not missing_method or not summary_data:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            if selected_column not in df.columns:
                return Response({"error": f"Column '{selected_column}' not found in data."}, status=400)

            if missing_method == "mean":
                if pd.api.types.is_numeric_dtype(df[selected_column]):
                    mean_val = df[selected_column].mean()
                    df[selected_column].fillna(mean_val, inplace=True)
                else:
                    return Response({"error": "Mean imputation requires a numeric column."}, status=400)

            elif missing_method == "median":
                if pd.api.types.is_numeric_dtype(df[selected_column]):
                    median_val = df[selected_column].median()
                    df[selected_column].fillna(median_val, inplace=True)
                else:
                    return Response({"error": "Median imputation requires a numeric column."}, status=400)

            elif missing_method == "mode":
                mode_series = df[selected_column].mode()
                if not mode_series.empty:
                    mode_val = mode_series[0]
                    df[selected_column].fillna(mode_val, inplace=True)
                else:
                    return Response({"error": "Could not compute mode."}, status=400)

            elif missing_method == "remove_row":
                df.dropna(subset=[selected_column], inplace=True)

            elif missing_method == "remove_col":
                df.drop(columns=[selected_column], inplace=True)

            else:
                return Response({"error": f"Invalid missing method '{missing_method}'"}, status=400)

            # Update filtered data
            sub_df_json = df.to_dict(orient='list')
            sub_df_json = {k: [make_json_serializable(v) for v in vals] for k, vals in sub_df_json.items()}

            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_file:
                df.to_csv(temp_file.name, index=False)
                temp_file_path = temp_file.name

            r_script_path = "R Functions/get_dataset_summary.R"
            cmd = ["Rscript", r_script_path, temp_file_path]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise Exception(result.stderr)

            # Parse JSON output
            r_output = json.loads(result.stdout)

            return Response({
                "summary": r_output.get("summary"),
                "columns": r_output.get("columns"),
                "frequency_data": r_output.get("frequency_data"),
                "correlation_matrix": r_output.get("correlation_matrix"),
                "filtered_data": sub_df_json
            })

        except Exception as e:
            return Response({"error": str(e)}, status=500)

class DetectOutliersView(APIView):
    def post(self, request):
        selected_column = request.data.get("selectedOutlierColumn")
        outlier_method = request.data.get("outlierMethod")
        summary_data = request.data.get("summaryData")

        if not selected_column or not outlier_method or not summary_data:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            if selected_column not in df.columns:
                return Response({"error": f"Column '{selected_column}' not found in data."}, status=400)

            data = df[selected_column]
            outlier_count = 0

            if outlier_method == "zscore":
                z_scores = zscore(data)
                threshold = 3
                outlier_count = np.sum(np.abs(z_scores) > threshold)

            elif outlier_method == "iqr":
                Q1 = data.quantile(0.25)
                Q3 = data.quantile(0.75)
                IQR = Q3 - Q1
                lower_bound = Q1 - 1.5 * IQR
                upper_bound = Q3 + 1.5 * IQR
                outlier_count = np.sum((data < lower_bound) | (data > upper_bound))

            elif outlier_method == "percentile":
                lower_percentile = data.quantile(0.01)
                upper_percentile = data.quantile(0.99)
                outlier_count = np.sum((data < lower_percentile) | (data > upper_percentile))

            else:
                return Response({"error": f"Invalid missing method '{outlier_method}'"}, status=400)

            return Response({"outlier_count": int(outlier_count)})

        except Exception as e:
            return Response({"error": str(e)}, status=500)

class HandleOutliersView(APIView):
    def post(self, request):
        selected_column = request.data.get("selectedOutlierColumn")
        outlier_method = request.data.get("outlierMethod")
        outlier_replace_method = request.data.get("outlierReplaceMethod")
        summary_data = request.data.get("summaryData")

        if not selected_column or not outlier_method or not summary_data or not outlier_replace_method:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            if selected_column not in df.columns:
                return Response({"error": f"Column '{selected_column}' not found in data."}, status=400)

            # Convert to numeric for outlier calculations
            df[selected_column] = pd.to_numeric(df[selected_column], errors='coerce')
            data = df[selected_column]

            # Identify outliers mask
            if outlier_method == "zscore":
                z_scores = zscore(data.dropna())
                outliers = pd.Series(False, index=data.index)
                outliers[data.dropna().index] = np.abs(z_scores) > 3

            elif outlier_method == "iqr":
                Q1 = data.quantile(0.25)
                Q3 = data.quantile(0.75)
                IQR = Q3 - Q1
                lower_bound = Q1 - 1.5 * IQR
                upper_bound = Q3 + 1.5 * IQR
                outliers = (data < lower_bound) | (data > upper_bound)

            elif outlier_method == "percentile":
                lower = data.quantile(0.01)
                upper = data.quantile(0.99)
                outliers = (data < lower) | (data > upper)

            else:
                return Response({"error": f"Invalid outlier method '{outlier_method}'"}, status=400)

            # Handle outliers
            if outlier_replace_method == "mean":
                replacement_value = data.mean()
                df.loc[outliers, selected_column] = replacement_value

            elif outlier_replace_method == "median":
                replacement_value = data.median()
                df.loc[outliers, selected_column] = replacement_value

            elif outlier_replace_method == "mode":
                replacement_value = data.mode().iloc[0] if not data.mode().empty else data.median()
                df.loc[outliers, selected_column] = replacement_value

            elif outlier_replace_method == "remove_row":
                df = df[~outliers]

            else:
                return Response({"error": f"Invalid replace method '{outlier_replace_method}'"}, status=400)

            # Update filtered data
            sub_df_json = df.to_dict(orient='list')
            sub_df_json = {k: [make_json_serializable(v) for v in vals] for k, vals in sub_df_json.items()}

            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_file:
                df.to_csv(temp_file.name, index=False)
                temp_file_path = temp_file.name

            r_script_path = "R Functions/get_dataset_summary.R"
            cmd = ["Rscript", r_script_path, temp_file_path]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise Exception(result.stderr)

            # Parse JSON output
            r_output = json.loads(result.stdout)

            return Response({
                "summary": r_output.get("summary"),
                "columns": r_output.get("columns"),
                "frequency_data": r_output.get("frequency_data"),
                "correlation_matrix": r_output.get("correlation_matrix"),
                "filtered_data": sub_df_json
            })

        except Exception as e:
            return Response({"error": str(e)}, status=500)