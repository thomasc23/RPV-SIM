# Setup Python environment for redistricting analysis

message("Setting up Python environment...")

# Create environment directory 
env_dir = file.path(getwd(), "Python", "Env", "recom_env")
if (!dir.exists(env_dir)) {
  dir.create(env_dir, recursive = TRUE)
  
  # Create virtual environment
  if (.Platform$OS.type == "windows") {
    message("Creating Python virtual environment on Windows...")
    system2("python", c("-m", "venv", shQuote(env_dir)))
  } else {
    message("Creating Python virtual environment on Unix-like system...")
    system2("python3", c("-m", "venv", shQuote(env_dir)))
  }
  
  # Install required packages
  if (.Platform$OS.type == "windows") {
    pip_path = file.path(env_dir, "Scripts", "pip.exe")
  } else {
    pip_path = file.path(env_dir, "bin", "pip")
  }
  
  message("Installing required Python packages...")
  system2(pip_path, c("install", "gerrychain", "geopandas", "pandas", "numpy", "matplotlib", "tqdm", "sklearn"))
  # system2(shQuote(pip_path), c("install", "gerrychain", "geopandas", "pandas", "numpy", "matplotlib"))
  message("Python packages installed successfully.")
}

# Set the Python path 
if (.Platform$OS.type == "windows") {
  python_env_path = file.path(getwd(), "Python", "Env", "recom_env", "Scripts", "python.exe")
} else {
  python_env_path = file.path(getwd(), "Python", "Env", "recom_env", "bin", "python")
}

# Load reticulate and use the environment
library(reticulate)
use_python(python_env_path)

message(paste("Python environment configured. Using:", python_env_path))
