from rest_framework.views import APIView
from rest_framework.response import Response
from rest_framework.parsers import MultiPartParser
from django.http import HttpResponse
from io import BytesIO
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
    
import math
import pandas as pd
import numpy as np

def make_json_serializable_variable(obj):
    """Recursively make an object JSON serializable by handling all edge cases"""
    
    # Handle exact null-like strings
    if isinstance(obj, str) and obj.strip().lower() in ['nan', 'inf', '-inf', 'null']:
        return None

    # Handle NaNs and Infs from float or numpy
    if isinstance(obj, (float, np.floating)):
        if math.isnan(obj) or math.isinf(obj):
            return None
        return int(obj) if obj.is_integer() else obj

    # Handle pandas/numpy NaNs or missing values
    if pd.isna(obj):
        return None

    # Recursively process dicts
    if isinstance(obj, dict):
        return {k: make_json_serializable(v) for k, v in obj.items()}

    # Recursively process lists/tuples
    if isinstance(obj, (list, tuple)):
        return [make_json_serializable(item) for item in obj]

    # Convert numpy int/float types
    if isinstance(obj, (np.integer, np.int64)):
        return int(obj)
    if isinstance(obj, (np.floating, np.float64)):
        return float(obj)

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
        
class RemoveDuplicatedView(APIView):
    def post(self, request):
        summary_data = request.data.get("summaryData")

        if not summary_data:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            # remove duplicates
            df = df.drop_duplicates()

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

class HandleInconsistenciesView(APIView):
    def post(self, request):
        inconsistencies_checks = request.data.get("inconsistencies")
        summary_data = request.data.get("summaryData")

        if not summary_data or not inconsistencies_checks:
            return Response({"error": "Missing required parameters"}, status=400)


        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            # Apply inconsistency handling
            if inconsistencies_checks.get('strip'):
                df = df.applymap(lambda x: x.strip() if isinstance(x, str) else x)

            if inconsistencies_checks.get('case'):
                df = df.applymap(lambda x: x.lower() if isinstance(x, str) else x)

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

class DownloadOutputView(APIView):
    def post(self, request):
        summary_data = request.data.get("summaryData")

        if not summary_data:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            if not preview_data:
                return Response({"error": "No data available to download."}, status=400)

            df = pd.DataFrame(preview_data)

            # Create in-memory Excel file
            output = BytesIO()
            with pd.ExcelWriter(output, engine='openpyxl') as writer:
                df.to_excel(writer, index=False, sheet_name='Sheet1')
            output.seek(0)

            filename = f"output-data-file.xlsx"
            response = HttpResponse(
                output,
                content_type='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
            )
            response['Content-Disposition'] = f'attachment; filename="{filename}"'

            return response

        except Exception as e:
            return Response({"error": str(e)}, status=500)
        
class NormalizeDataView(APIView):
    def post(self, request):
        selected_column = request.data.get("selectedColumn")
        normalization_method = request.data.get("normalizationMethod")
        summary_data = request.data.get("summaryData")

        if not selected_column or not normalization_method or not summary_data:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            if selected_column not in df.columns:
                return Response({"error": f"Column '{selected_column}' not found in data."}, status=400)

            # Save DataFrame to CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_input:
                df.to_csv(temp_input.name, index=False)
                input_path = temp_input.name

            # Prepare output path for normalization
            with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as temp_output:
                normalized_output_path = temp_output.name

            # R Script call for normalization
            r_script_path = "R Functions/Transformation/normalize_column.R"
            cmd = [
                "Rscript",
                r_script_path,
                input_path,
                selected_column,
                normalization_method,
                normalized_output_path
            ]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise Exception(f"R Script Error during normalization: {result.stderr}")

            with open(normalized_output_path, "r") as f:
                normalized_data = json.load(f)

            # Save the normalized DataFrame to a new temporary CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_normalized_file:
                normalized_df = pd.DataFrame(normalized_data)
                normalized_df.to_csv(temp_normalized_file.name, index=False)
                normalized_csv_path = temp_normalized_file.name

            # Call R script to generate summary based on normalized data
            summary_r_script_path = "R Functions/get_dataset_summary.R"
            summary_cmd = ["Rscript", summary_r_script_path, normalized_csv_path]
            summary_result = subprocess.run(summary_cmd, capture_output=True, text=True)

            if summary_result.returncode != 0:
                raise Exception(f"R Script Error during summary calculation: {summary_result.stderr}")

            r_output = json.loads(summary_result.stdout)

            return Response({
                "summary": r_output.get("summary"),
                "columns": r_output.get("columns"),
                "frequency_data": r_output.get("frequency_data"),
                "correlation_matrix": r_output.get("correlation_matrix"),
                "filtered_data": normalized_data
            })

        except Exception as e:
            return Response({"error": str(e)}, status=500)
        
        
class EncodingView(APIView):
    def post(self, request):
        selected_column = request.data.get("selectedColumn")
        encoding_method = request.data.get("encodingMethod")
        summary_data = request.data.get("summaryData")

        if not selected_column or not encoding_method or not summary_data:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            if selected_column not in df.columns:
                return Response({"error": f"Column '{selected_column}' not found in data."}, status=400)

            # Save DataFrame to CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_input:
                df.to_csv(temp_input.name, index=False)
                input_path = temp_input.name

            # Prepare output path for Encoding
            with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as temp_output:
                encoding_output_path = temp_output.name

            # R Script call for Encoding
            r_script_path = "R Functions/Transformation/encoding.R"
            cmd = [
                "Rscript",
                r_script_path,
                input_path,
                selected_column,
                encoding_method,
                encoding_output_path
            ]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise Exception(f"R Script Error during Encoding: {result.stderr}")

            with open(encoding_output_path, "r") as f:
                encoded_data = json.load(f)

            # Save the encoded DataFrame to a new temporary CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_encoded_file:
                encoded_df = pd.DataFrame(encoded_data)
                encoded_df.to_csv(temp_encoded_file.name, index=False)
                encoded_csv_path = temp_encoded_file.name

            # Call R script to generate summary based on encoded data
            summary_r_script_path = "R Functions/get_dataset_summary.R"
            summary_cmd = ["Rscript", summary_r_script_path, encoded_csv_path]
            summary_result = subprocess.run(summary_cmd, capture_output=True, text=True)

            if summary_result.returncode != 0:
                raise Exception(f"R Script Error during summary calculation: {summary_result.stderr}")

            r_output = json.loads(summary_result.stdout)

            return Response({
                "summary": r_output.get("summary"),
                "columns": r_output.get("columns"),
                "frequency_data": r_output.get("frequency_data"),
                "correlation_matrix": r_output.get("correlation_matrix"),
                "filtered_data": encoded_data
            })

        except Exception as e:
            return Response({"error": str(e)}, status=500)
        
class SkewnessView(APIView):
    def post(self, request):
        selected_column = request.data.get("selectedColumn")
        skewness_method = request.data.get("skewnessMethod")
        summary_data = request.data.get("summaryData")

        if not selected_column or not skewness_method or not summary_data:
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            if selected_column not in df.columns:
                return Response({"error": f"Column '{selected_column}' not found in data."}, status=400)

            # Save DataFrame to CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_input:
                df.to_csv(temp_input.name, index=False)
                input_path = temp_input.name

            # Prepare output path for Skewness
            with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as temp_output:
                skewness_output_path = temp_output.name

            # R Script call for Skewness
            r_script_path = "R Functions/Transformation/skewness.R"
            cmd = [
                "Rscript",
                r_script_path,
                input_path,
                selected_column,
                skewness_method,
                skewness_output_path
            ]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise Exception(f"R Script Error during Skewness: {result.stderr}")

            with open(skewness_output_path, "r") as f:
                skewed_data = json.load(f)

            # Save the skewed_data DataFrame to a new temporary CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_skewed_file:
                skewed_df = pd.DataFrame(skewed_data)
                skewed_df.to_csv(temp_skewed_file.name, index=False)
                skewed_csv_path = temp_skewed_file.name

            # Call R script to generate summary based on skewed data
            summary_r_script_path = "R Functions/get_dataset_summary.R"
            summary_cmd = ["Rscript", summary_r_script_path, skewed_csv_path]
            summary_result = subprocess.run(summary_cmd, capture_output=True, text=True)

            if summary_result.returncode != 0:
                raise Exception(f"R Script Error during summary calculation: {summary_result.stderr}")

            r_output = json.loads(summary_result.stdout)

            return Response({
                "summary": r_output.get("summary"),
                "columns": r_output.get("columns"),
                "frequency_data": r_output.get("frequency_data"),
                "correlation_matrix": r_output.get("correlation_matrix"),
                "filtered_data": skewed_data
            })

        except Exception as e:
            return Response({"error": str(e)}, status=500)
        
class AggregationView(APIView):
    def post(self, request):
        group_vars = request.data.get("groupVars")
        agg_var = request.data.get("aggVar")
        agg_func = request.data.get("aggFunc")
        summary_data = request.data.get("summaryData")

        if not group_vars or not agg_func or not summary_data or (agg_func != "count" and not agg_var):
            return Response({"error": "Missing required parameters"}, status=400)

        try:
            preview_data = summary_data.get("filtered_data", [])
            df = pd.DataFrame(preview_data)

            # Validate group_vars and agg_var columns
            group_vars_list = group_vars.split(",")
            for var in group_vars_list:
                if var not in df.columns:
                    return Response({"error": f"Group column '{var}' not found in data."}, status=400)

            if agg_func != "count" and agg_var not in df.columns:
                return Response({"error": f"Aggregation column '{agg_var}' not found in data."}, status=400)

            # Save DataFrame to CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_input:
                df.to_csv(temp_input.name, index=False)
                input_path = temp_input.name

            # Prepare output path for Aggregation
            with tempfile.NamedTemporaryFile(suffix=".json", delete=False) as temp_output:
                aggregation_output_path = temp_output.name

            # R Script call for Aggregation
            r_script_path = "R Functions/Transformation/aggregation.R"
            cmd = [
                "Rscript",
                r_script_path,
                input_path,
                group_vars,
                agg_var if agg_func != "count" else "",
                agg_func,
                aggregation_output_path
            ]
            result = subprocess.run(cmd, capture_output=True, text=True)

            if result.returncode != 0:
                raise Exception(f"R Script Error during Aggregation: {result.stderr}")

            with open(aggregation_output_path, "r") as f:
                aggregated_data = json.load(f)

            # Save the aggregated DataFrame to a new temporary CSV
            with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_aggregated_file:
                aggregated_df = pd.DataFrame(aggregated_data)
                aggregated_df.to_csv(temp_aggregated_file.name, index=False)
                aggregated_csv_path = temp_aggregated_file.name

            # Call R script to generate summary based on aggregated data
            summary_r_script_path = "R Functions/get_dataset_summary.R"
            summary_cmd = ["Rscript", summary_r_script_path, aggregated_csv_path]
            summary_result = subprocess.run(summary_cmd, capture_output=True, text=True)

            if summary_result.returncode != 0:
                raise Exception(f"R Script Error during summary calculation: {summary_result.stderr}")

            r_output = json.loads(summary_result.stdout)

            return Response({
                "summary": r_output.get("summary"),
                "columns": r_output.get("columns"),
                "frequency_data": r_output.get("frequency_data"),
                "correlation_matrix": r_output.get("correlation_matrix"),
                "filtered_data": aggregated_data
            })

        except Exception as e:
            return Response({"error": str(e)}, status=500)
        
        

class VariableConstruction(APIView):
    def post(self, request):
        summary_data = request.data.get("summaryData")
        preview_data = summary_data.get("filtered_data", [])
        df = pd.DataFrame(preview_data)
        
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
        # print(result)
        # df = pd.DataFrame(result)
        
        # # Update filtered data
        # result = df.to_dict(orient='list')
        # result = {k: [make_json_serializable_variable(v) for v in vals] for k, vals in result.items()}
        
        # if not result:
        #     return Response({"error": "Missing required parameters"}, status=400)

        # try:
        #     # Save the skewed_data DataFrame to a new temporary CSV
        #     with tempfile.NamedTemporaryFile(suffix=".csv", delete=False) as temp_skewed_file:
        #         result = pd.DataFrame(result)
        #         result.to_csv(temp_skewed_file.name, index=False)
        #         skewed_csv_path = temp_skewed_file.name

        #     # Call R script to generate summary based on skewed data
        #     summary_r_script_path = "R Functions/get_dataset_summary.R"
        #     summary_cmd = ["Rscript", summary_r_script_path, skewed_csv_path]
        #     summary_result = subprocess.run(summary_cmd, capture_output=True, text=True)

        #     if summary_result.returncode != 0:
        #         raise Exception(f"R Script Error during summary calculation: {summary_result.stderr}")

        #     r_output = json.loads(summary_result.stdout)

        #     return Response({
        #         "summary": r_output.get("summary"),
        #         "columns": r_output.get("columns"),
        #         "frequency_data": r_output.get("frequency_data"),
        #         "correlation_matrix": r_output.get("correlation_matrix"),
        #         "filtered_data": result
        #     })

        # except Exception as e:
        #     return Response({"error": str(e)}, status=500)