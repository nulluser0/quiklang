use std::fs;

pub fn add_env() {
    println!("cargo:rerun-if-changed=build.rs");

    // Read Cargo.toml
    let cargo_toml = fs::read_to_string("Cargo.toml").expect("Failed to read Cargo.toml");

    // Parse Cargo.toml using toml crate
    let cargo: toml::Value = toml::from_str(&cargo_toml).expect("Failed to parse Cargo.toml");

    // Access the custom variables
    if let Some(metadata) = cargo.get("package").and_then(|pkg| pkg.get("metadata")) {
        if let Some(custom_variable) = metadata.get("quiklang-vm-version") {
            let custom_variable = custom_variable
                .as_integer()
                .expect("Invalid quiklang-vm-version, should be integer");
            println!("cargo:rustc-env=QUIKLANG_VM_VERSION={}", custom_variable);
        }
    }
}
