use std::path::PathBuf;

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
            None => vec![],
        };

        ClasspathResolver {
            workspace_type,
            workspace_root,
            classpath,
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

fn resolve_maven_classpath(workspace_root: &PathBuf) -> Vec<PathBuf> {
    // Resolve the classpath for a Maven workspace
    vec![]
}

fn resolve_gradle_classpath(workspace_root: &PathBuf) -> Vec<PathBuf> {
    // Resolve the classpath for a Gradle workspace
    vec![]
}