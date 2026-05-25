#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum KtVersion {
    V1_3,
    V1_4,
    V1_5,
    V1_6,
    V1_7,
    V1_8,
    V1_9,
    V2_0,
    V2_1,
    V2_2,
    V2_3,
    V2_4,
}

impl KtVersion {
    pub const DEFAULT: KtVersion = KtVersion::V1_8;
    pub const LATEST: KtVersion = KtVersion::V2_4;
    pub const CURRENT: KtVersion = KtVersion::V2_4;
}
