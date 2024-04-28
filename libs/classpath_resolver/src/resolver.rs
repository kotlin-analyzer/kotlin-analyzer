use std::{
    fs,
    io::{self},
    path::{Path, PathBuf},
    process::Command,
    result,
};

#[derive(Debug)]
pub enum WorkspaceType {
    Maven,
    Gradle,
}

#[allow(dead_code)]
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
    } else if workspace_root.join("settings.gradle").exists()
        || workspace_root.join("settings.gradle.kts").exists()
    {
        return Some(WorkspaceType::Gradle);
    }

    None
}

fn resolve_maven_classpath(workspace_root: &Path) -> Result<Vec<PathBuf>> {
    let args = ["dependency:build-classpath"];

    let output = run_shell(maven_command(), &args, workspace_root)?;

    let output_buffer = output.lines().collect::<Vec<&str>>();
    let classpath_header = output_buffer
        .iter()
        .position(|line| line.starts_with("[INFO] Dependencies classpath:"));

    if let Some(index) = classpath_header {
        // The classpath is on the next line
        let path_str = output_buffer[index + 1];

        return Ok(path_str
            .split(":")
            .map(PathBuf::from)
            .filter(|path| path.exists())
            .collect::<Vec<PathBuf>>());
    }

    Ok(vec![])
}

fn resolve_gradle_classpath(workspace_root: &Path) -> Result<Vec<PathBuf>> {
    let init_script = get_init_script_location();
    let args = ["-I", &init_script, "classpaths"];

    let output = run_shell(gradle_command(), &args, workspace_root)?;

    let classpaths = output
        .lines()
        .collect::<Vec<&str>>()
        .into_iter()
        .filter(|entry| entry.starts_with("gradle-classpath-resolver "))
        .map(|entry| {
            let path = entry.replace("gradle-classpath-resolver ", "");
            PathBuf::from(path)
        })
        .filter(|cp| cp.exists())
        .collect::<Vec<PathBuf>>();

    Ok(classpaths)
}

fn get_init_script_location() -> String {
    let script = PathBuf::from("classpath.gradle");

    fs::canonicalize(script)
        .expect("classpath.gradle not found!")
        .to_str()
        .expect("classpath.gradle path is not valid UTF-8!")
        .to_string()
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
#[allow(dead_code)]
enum Error {
    Io(io::Error),
    Utf8(std::string::FromUtf8Error),
    CommandFailed,
}

fn run_shell(command: &str, args: &[&str], cwd: &Path) -> Result<String> {
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

fn gradle_command() -> &'static str {
    if cfg!(windows) {
        "gradlew.bat"
    } else {
        "./gradlew"
    }
}

fn maven_command() -> &'static str {
    if cfg!(windows) {
        "mvnw.cmd"
    } else {
        "./mvnw"
    }
}
