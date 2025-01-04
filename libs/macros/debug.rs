#[derive(PartialEq, Eq, Hash, Clone)]
pub enum AssignmentAndOperatorKind {
    AddAssignment(AddAssignment),
    SubAssignment(SubAssignment),
    MultAssignment(MultAssignment),
    DivAssignment(DivAssignment),
    ModAssignment(ModAssignment),
}
impl AssignmentAndOperator {
    pub fn kind(&self) -> AssignmentAndOperatorKind {
        AddAssignment::cast(self.0.clone())
            .map(AssignmentAndOperatorKind::AddAssignment)
            .or(
                SubAssignment::cast(self.0.clone())
                    .map(AssignmentAndOperatorKind::SubAssignment),
            )
            .or(
                MultAssignment::cast(self.0.clone())
                    .map(AssignmentAndOperatorKind::MultAssignment),
            )
            .or(
                DivAssignment::cast(self.0.clone())
                    .map(AssignmentAndOperatorKind::DivAssignment),
            )
            .or(
                ModAssignment::cast(self.0.clone())
                    .map(AssignmentAndOperatorKind::ModAssignment),
            )
            .unwrap()
    }
}
