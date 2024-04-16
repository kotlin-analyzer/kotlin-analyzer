use std::{
    fs,
    io::{self, Write},
    path::{Path, PathBuf},
    process::Command,
    result,
};

#[derive(Debug)]
pub enum WorkspaceType {
    Maven,
    Gradle,
}

pub struct ClasspathResolver {
    workspace_type: Option<WorkspaceType>,
    workspace_root: PathBuf,
    classpath: Vec<PathBuf>,
}

impl ClasspathResolver {
    pub fn new_with_type(
        workspace_root: PathBuf,
        workspace_type: Option<WorkspaceType>,
    ) -> ClasspathResolver {
        let classpath = match workspace_type {
            Some(WorkspaceType::Maven) => resolve_maven_classpath(&workspace_root),
            Some(WorkspaceType::Gradle) => resolve_gradle_classpath(&workspace_root),
            None => Ok(vec![]),
        };

        ClasspathResolver {
            workspace_type,
            workspace_root,
            classpath: classpath.unwrap_or(vec![]),
        }
    }

    pub fn new(workspace_root: PathBuf) -> ClasspathResolver {
        let workspace_type = detect_workspace_type(&workspace_root);

        ClasspathResolver::new_with_type(workspace_root, workspace_type)
    }
}

fn detect_workspace_type(workspace_root: &PathBuf) -> Option<WorkspaceType> {
    if workspace_root.join("pom.xml").exists() {
        return Some(WorkspaceType::Maven);
    } else if workspace_root.join("build.gradle").exists()
        || workspace_root.join("build.gradle.kts").exists()
    {
        return Some(WorkspaceType::Gradle);
    }

    None
}

fn resolve_maven_classpath(workspace_root: impl AsRef<Path>) -> Result<Vec<PathBuf>> {
    // Resolve the classpath for a Maven workspace
    Ok(vec![])
}

fn resolve_gradle_classpath(workspace_root: impl AsRef<Path>) -> Result<Vec<PathBuf>> {
    let init_script = get_init_script_location();
    let args: [&str; 3] = ["-I", &init_script, "classpaths"];

    let output = run_shell(
        gradle_command(),
        &args,
        workspace_root,
    )?;

    let classpaths = output
        .lines()
        .collect::<Vec<&str>>()
        .into_iter()
        .filter(|entry| entry.starts_with("gradle-classpath-resolver "))
        .map(|entry| {
            let path = entry.replace("gradle-classpath-resolver ", "");
            PathBuf::from(path)
        })
        .collect::<Vec<PathBuf>>();

    Ok(classpaths)
}

fn get_init_script_location() -> String {
    let script = PathBuf::from("classpath.gradle");

    fs::canonicalize(script)
        .unwrap()
        .to_str()
        .unwrap()
        .to_string()
}

fn gradle_command() -> &'static str {
    if cfg!(windows) {
        "gradlew.bat"
    } else {
        "./gradlew"
    }
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
enum Error {
    Io(io::Error),
    Utf8(std::string::FromUtf8Error),
    CommandFailed,
}

fn run_shell<T: AsRef<Path>>(command: &str, args: &[&str], cwd: T) -> Result<String> {
    let output = Command::new(command)
        .args(args)
        .current_dir(cwd)
        .output()
        .map_err(Error::Io)?;

    if !output.status.success() {
        return Err(Error::CommandFailed);
    }

    String::from_utf8(output.stdout).map_err(Error::Utf8)
}
