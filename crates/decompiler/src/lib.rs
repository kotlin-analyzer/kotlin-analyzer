use classy::ClassFile;
use jars::JarOptionBuilder;
use std::{collections::HashMap, io::BufReader, path::Path};

pub enum Error {
    JarError(std::io::Error),
}

fn generate_definitions(jar_file: &Path) -> Result<HashMap<String, ClassFile>, Error> {
    let jar = jars::jar(jar_file, JarOptionBuilder::builder().ext("class").build()).map_err(Error::JarError)?;

    let mut class_definitions = HashMap::<String, ClassFile>::new();

    for (file_path, content) in jar.files {
        if !file_path.ends_with(".class") {
            continue;
        }

        let reader = BufReader::new(content.as_slice());
        match classy::read_class(reader) {
            Ok(class_file) => {
                class_definitions.insert(file_path, class_file);
            },
            Err(err) => {
                println!("Skipping class file {} due to error: {}", file_path, err);
            }
        }
    }
    Ok(class_definitions)
}

// #[cfg(test)]
// mod tests {
//     use std::path::Path;

//     use crate::generate_definitions;


//     #[test]
//     fn test_generate_definitions() {
//         let path = Path::new("/Users/benjamin/.m2/repository/org/springframework/boot/spring-boot/3.2.4/spring-boot-3.2.4.jar");
//         let _ = generate_definitions(path);
//     }
// }