from django.urls import path
from .views import *

urlpatterns = [
    path('upload/', UploadFileView.as_view(), name='upload-file'),
    path('summary/', DatasetSummaryView.as_view(), name='dataset-summary'),
    path('handle-missing/', HandleMissingValuesView.as_view(), name='handle-missing'),
    path('handle-smoothing/', HandleSmoothingView.as_view(), name='handle-smoothing'),
    path('handle-outliers/', HandleOutliersView.as_view(), name='handle-outliers'),
    path('detect-outliers/', DetectOutliersView.as_view(), name='detect-outliers'),
    path('remove-duplicates/', RemoveDuplicatedView.as_view(), name='remove-duplicates'),
    path('handle-inconsistencies/', HandleInconsistenciesView.as_view(), name='handle-inconsistencies'),
    path('download-output/', DownloadOutputView.as_view(), name='download-output'),
    path('normalize-column/', NormalizeDataView.as_view(), name = 'normalize-column'),
    path('encoding/', EncodingView.as_view(), name = 'encoding'),
    path('skewness/', SkewnessView.as_view(), name = 'skewness'),
    path('aggregate/', AggregationView.as_view(), name = 'aggregate'),
    path('variable-construction/', VariableConstructionView.as_view(), name='variable-construction'),
    path('handle-discretize/', HandleDiscretizationView.as_view(), name='handle-discretize'),
    path('handle-dimensionality-reduction/', DimensionalityReductionView.as_view(), name='handle-dimensionality-reduction'),
    path('handle-multicollinearity/', HandleMulticollinearityView.as_view(), name='handle-multicollinearity'),
    path('delete-columns/', DeleteColumnsView.as_view(), name='delete-columns'),
]