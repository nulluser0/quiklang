use std::env;
use std::fs;
use std::path::PathBuf;
use toml::Value;

pub fn add_env() {
    // Instruct Cargo to rerun the build script if build.rs changes
    println!("cargo:rerun-if-changed=build.rs");

    // Get the directory containing this crate's Cargo.toml
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not set");

    let mut path = PathBuf::from(&manifest_dir);

    // Initialize variable to hold workspace Cargo.toml path
    let mut workspace_cargo_toml = None;

    // Traverse upwards to find workspace Cargo.toml
    loop {
        let cargo_toml_path = path.join("Cargo.toml");
        if cargo_toml_path.exists() {
            // Read Cargo.toml
            let cargo_toml = fs::read_to_string(&cargo_toml_path).expect(&format!(
                "Failed to read Cargo.toml at {}",
                cargo_toml_path.display()
            ));

            // Parse Cargo.toml
            let cargo: Value = toml::from_str(&cargo_toml).expect("Failed to parse Cargo.toml");

            // Check if it's the workspace Cargo.toml by looking for [workspace] section
            if cargo.get("workspace").is_some() {
                // Set the workspace Cargo.toml path
                workspace_cargo_toml = Some(cargo_toml_path.clone());

                // Access workspace.metadata
                if let Some(metadata) = cargo
                    .get("workspace")
                    .and_then(|workspace| workspace.get("metadata"))
                {
                    if let Some(custom_variable) = metadata.get("quiklang-vm-version") {
                        let custom_variable = custom_variable
                            .as_integer()
                            .expect("Invalid quiklang-vm-version, should be integer");
                        println!("cargo:rustc-env=QUIKLANG_VM_VERSION={}", custom_variable);
                    }
                }
                break; // Exit loop after finding workspace Cargo.toml
            }
        }

        // Move up one directory
        if !path.pop() {
            panic!("Could not find workspace Cargo.toml with [workspace.metadata]");
        }
    }

    // Optionally, watch the workspace Cargo.toml for changes to trigger rebuilds
    if let Some(cargo_toml_path) = workspace_cargo_toml {
        // Compute path relative to CARGO_MANIFEST_DIR for portability
        let relative_path = pathdiff::diff_paths(&cargo_toml_path, &manifest_dir)
            .expect("Failed to compute relative path to workspace Cargo.toml");
        let relative_path_str = relative_path.to_str().expect("Path contains invalid UTF-8");

        println!("cargo:rerun-if-changed={}", relative_path_str);
    }
}
