use std::path::Path;

use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    pub package: PackageInfo,
    pub dependencies: Vec<Dependency>,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    pub authors: Vec<String>,
    pub description: String,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub name: String,
    pub version: String,
}

impl PackageMetadata {
    pub fn from_toml(project_dir: &Path) -> Result<Self, std::io::Error> {
        let toml_path = project_dir.join("Package.toml");
        let toml_str = std::fs::read_to_string(toml_path)?;
        toml::from_str(&toml_str).map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
    }
}
