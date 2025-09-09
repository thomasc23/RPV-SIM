# Package setup for rpvsimulator

.onLoad = function(libname, pkgname) {
  # Set up Python environment on package load
  setup_python_environment()
}

# Set up Python environment
setup_python_environment = function() {
  # Try to set up Python environment
  if (requireNamespace("reticulate", quietly = TRUE)) {
    # Use reticulate to set up Python environment
    reticulate::configure_environment(pkgname)
  }
}
