use ::std::fmt;

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

#[derive(Debug)]
pub struct ParseKtVersionError {
    invalid_input: String,
}

impl std::error::Error for ParseKtVersionError {}
impl fmt::Display for ParseKtVersionError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "invalid version: {:?}", self.invalid_input)
    }
}

impl std::str::FromStr for KtVersion {
    type Err = ParseKtVersionError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let res = match s {
            "V1.8" => KtVersion::V1_8,
            "V1.9" => KtVersion::V1_9,
            "V2.0" => KtVersion::V2_0,
            "V2.1" => KtVersion::V2_1,
            "V2.2" => KtVersion::V2_2,
            "V2.3" => KtVersion::V2_3,
            "V2.4" => KtVersion::V2_4,
            _ => return Err(ParseKtVersionError { invalid_input: s.to_owned() }),
        };
        Ok(res)
    }
}
