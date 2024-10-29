use std::{collections::HashMap, path::Path};

use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct PackageMetadata {
    #[serde(rename = "Package")]
    pub package: PackageInfo,
    #[serde(rename = "Dependencies")]
    pub dependencies: HashMap<String, DependencyInfo>,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct PackageInfo {
    pub name: String,
    pub version: String,
    pub authors: Vec<String>,
    pub description: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum DependencyInfo {
    Simple(String), // Only version is specified
    Detailed {
        version: String,
        #[serde(default)]
        features: Vec<String>,
        // Add other fields as needed
        // For example:
        // optional: bool,
        // path: String,
    },
}

impl PackageMetadata {
    pub fn from_toml(project_dir: &Path) -> Result<Self, std::io::Error> {
        let toml_path = project_dir.join("Package.toml");
        let toml_str = std::fs::read_to_string(toml_path)?;
        toml::from_str(&toml_str).map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e))
    }
}
