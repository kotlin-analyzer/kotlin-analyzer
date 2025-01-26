#![allow(unused)]
pub mod java;

use classy::ClassFile;
use jars::JarOptionBuilder;
use java::{decompile_class, JClass};
use std::{collections::HashMap, io::BufReader, path::Path};

pub enum Error {
    JarError(std::io::Error),
}

pub fn generate_definitions(jar_file: &Path) -> Result<HashMap<String, JClass>, Error> {
    let jar = jars::jar(jar_file, JarOptionBuilder::builder().ext("class").build())
        .map_err(Error::JarError)?;

    let mut class_definitions = HashMap::<String, JClass>::new();

    for (file_path, content) in jar.files {
        if !file_path.ends_with(".class") {
            continue;
        }

        let reader = BufReader::new(content.as_slice());
        match classy::read_class(reader) {
            Ok(class_file) => {
                if let Ok(j_class) = decompile_class(class_file) {
                    class_definitions.insert(file_path, j_class);
                }
            }
            Err(err) => {
                println!("Skipping class file {} due to error: {}", file_path, err);
            }
        }
    }
    Ok(class_definitions)
}

#[cfg(test)]
mod tests {
    use crate::{generate_definitions, java::JClass};
    use classpath_resolver::ClasspathResolver;

    #[test]
    fn test_generate_definitions() {
        let path = std::path::Path::new("/Users/benjamin/.m2/repository/org/springframework/boot/spring-boot/3.2.4/spring-boot-3.2.4.jar");
        if let Ok(defs) = generate_definitions(path) {
            let syntax = defs.values().nth(0).map(JClass::syntax).expect("ERR");
            println!("{}", syntax);
        }
    }

    // #[test]
    fn test_generate_definitions_from_resolver() {
        let path = std::path::Path::new("/Users/benjamin/Programs/kotlin/mvnproject");

        let resolver = ClasspathResolver::new(path);

        for cp in resolver.classpath {
            if let Ok(defs) = generate_definitions(&cp) {}
        }
    }
}
