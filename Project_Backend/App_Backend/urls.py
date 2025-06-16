from django.urls import path
from .views import *

urlpatterns = [
    path('upload/', UploadFileView.as_view(), name='upload-file'),
    path('summary/', DatasetSummaryView.as_view(), name='dataset-summary'),
    path('handle-missing/', HandleMissingValuesView.as_view(), name='handle-missing'),
    path('handle-outliers/', HandleOutliersView.as_view(), name='handle-outliers'),
    path('detect-outliers/', DetectOutliersView.as_view(), name='detect-outliers'),
]