#![allow(unused)]
use std::fmt::Display;

use classy::{Attribute, ClassFile, Constant, FieldInfo, MethodInfo};

const ACC_PUBLIC: u16 = 0x0001;
const ACC_PRIVATE: u16 = 0x0002;
const ACC_PROTECTED: u16 = 0x0004;
const ACC_STATIC: u16 = 0x0008;
const ACC_FINAL: u16 = 0x0010;
const ACC_VOLATILE: u16 = 0x0040;
const ACC_TRANSIENT: u16 = 0x0080;
const ACC_INTERFACE: u16 = 0x0200;
const ACC_SYNTHETIC: u16 = 0x1000;
const ACC_VARARGS: u16 = 0x0080;
const ACC_NATIVE: u16 = 0x0100;
const ACC_STRICT: u16 = 0x0800;
const ACC_ENUM: u16 = 0x4000;
const ACC_ABSTRACT: u16 = 0x0400;
const ACC_SYNCHRONIZED: u16 = 0x0020;
const ACC_BRIDGE: u16 = 0x0040;
const ACC_MANDATED: u16 = 0x8000;
const ACC_MODULE: u16 = 0x8000;

type DecompilationResult<T> = Result<T, DecompilationError>;

#[derive(Debug, PartialEq)]
pub enum DecompilationError {
    MissingField,
    MissingFieldDescriptor,
    InvalidFieldDescriptor,
    MissingInterface,
    MissingMethod,
    MissingMethodDescriptor,
    InvalidMethodDescriptor,
    InvalidObjectDescriptor,
    MissingClassName,
}

#[derive(Debug)]
pub struct JClass {
    name: String,
    is_public: bool,
    is_final: bool,
    is_abstract: bool,
    is_interface: bool,
    is_enum: bool,
    is_module: bool,
    implements: Vec<String>,
    extends: Option<String>,
    fields: Vec<JField>,
    methods: Vec<JMethod>,
}

impl JClass {
    pub fn syntax(&self) -> String {
        let mut syntax = "".to_string();

        if self.is_public {
            syntax.push_str("public ");
        }

        if self.is_final {
            syntax.push_str("final ");
        }

        if self.is_abstract && !self.is_interface {
            syntax.push_str("abstract ");
        }

        if self.is_enum {
            syntax.push_str("enum ");
        }

        if self.is_interface {
            syntax.push_str("interface ");
        }

        if !self.is_enum && !self.is_interface {
            syntax.push_str("class ");
        }

        syntax.push_str(&format!("{} ", &self.name.replace("/", "."),));

        if let Some(extends) = &self.extends {
            syntax.push_str(&format!("extends {} ", extends.replace("/", ".")));
        }

        if !self.implements.is_empty() {
            syntax.push_str(&format!(
                "implements {} ",
                self.implements.join(", ").replace("/", ".")
            ));
        }

        syntax.push_str("{\n");

        if !self.fields.is_empty() {
            syntax.push('\n');

            for field in &self.fields {
                syntax.push('\t');
                syntax.push_str(&field.syntax());
                syntax.push('\n');
            }
        }

        if !self.methods.is_empty() {
            syntax.push('\n');

            for method in &self.methods {
                syntax.push('\t');
                syntax.push_str(&method.syntax());
                syntax.push('\n');
            }
        }

        syntax.push_str("\n}");

        syntax
    }
}

#[derive(Debug, PartialEq)]
pub enum JVisibility {
    Public,
    Private,
    Protected,
}

impl Display for JVisibility {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Public => f.write_str("public "),
            Self::Private => f.write_str("private "),
            Self::Protected => f.write_str("protected "),
        }
    }
}

#[derive(Debug)]
pub struct JField {
    field_name: String,
    java_type: JType,
    visibility: JVisibility,
    is_static: bool,
    is_final: bool,
    is_enum_field: bool,
    is_volatile: bool,
    is_transient: bool,
    is_synthetic: bool,
    initial_value: Option<String>,
}

impl JField {
    pub fn syntax(&self) -> String {
        let mut syntax = "".to_string();

        if self.visibility != JVisibility::Public {
            syntax.push_str(&self.visibility.to_string());
        }

        if self.is_static {
            syntax.push_str("static ");
        }

        if self.is_final {
            syntax.push_str("final ");
        }

        syntax.push_str(&format!("{} {}", self.java_type, self.field_name));

        if let Some(value) = &self.initial_value {
            syntax.push_str(&format!(" = {}", value));
        }

        syntax.push_str(";\n");

        syntax
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum JType {
    Byte,
    Char,
    Double,
    Float,
    Int,
    Long,
    Short,
    Bool,
    Object(String),
    Array(Box<JType>),
    Void,
}

impl Display for JType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Byte => write!(f, "byte"),
            Self::Char => write!(f, "char"),
            Self::Double => write!(f, "double"),
            Self::Float => write!(f, "float"),
            Self::Int => write!(f, "int"),
            Self::Long => write!(f, "long"),
            Self::Short => write!(f, "short"),
            Self::Bool => write!(f, "boolean"),
            Self::Object(custom) => write!(f, "{}", custom.replace("/", ".")),
            Self::Void => write!(f, "void"),
            Self::Array(j_type) => write!(f, "{}[]", j_type),
        }
    }
}

#[derive(Debug)]
pub struct JMethod {
    method_name: String,
    return_type: JType,
    args: Vec<JArg>,
    visibility: JVisibility,
    is_static: bool,
    is_final: bool,
    is_synthetic: bool,
    is_synchronized: bool,
    is_abstract: bool,
    is_native: bool,
    has_varargs: bool,
    is_bridge: bool,
    is_strict: bool,
}

impl JMethod {
    pub fn syntax(&self) -> String {
        let mut syntax = "".to_string();

        if self.visibility != JVisibility::Public {
            syntax.push_str(&self.visibility.to_string());
        }

        if self.is_static {
            syntax.push_str("static ");
        }

        if self.is_final {
            syntax.push_str("final ");
        }

        syntax.push_str(&format!("{} {}(", self.return_type, self.method_name));

        let arguments = self
            .args
            .iter()
            .map(JArg::syntax)
            .collect::<Vec<String>>()
            .join(", ");

        syntax.push_str(&arguments);

        syntax.push(')');

        if self.is_abstract {
            syntax.push(';');
        } else {
            syntax.push_str(" {");
            syntax.push('\n');
            syntax.push('\t');
            syntax.push('}');
            syntax.push('\n');
        }

        syntax
    }
}

#[derive(Debug, PartialEq)]
pub struct MethodDescriptor {
    parameters: Vec<JType>,
    return_type: JType,
}

#[derive(Debug)]
pub struct JArg {
    arg_name: Option<String>,
    java_type: JType,
    is_final: bool,
    is_synthetic: bool,
    is_mandated: bool,
}

impl JArg {
    pub fn syntax(&self) -> String {
        let mut syntax = "".to_string();

        if self.is_final {
            syntax.push_str("final ");
        }
        syntax.push_str(&self.java_type.to_string());

        if let Some(name) = &self.arg_name {
            syntax.push_str(&format!(" {}", name));
        }

        syntax
    }
}

pub fn decompile_class(class_file: ClassFile) -> DecompilationResult<JClass> {
    let this_class_info = class_file.constant_pool[class_file.this_class as usize - 1].clone();

    let super_class_info = match class_file.super_class {
        0 => None,
        _ => Some(class_file.constant_pool[class_file.super_class as usize - 1].clone()),
    };

    let is_public = matches_mask(class_file.access_flags, ACC_PUBLIC);
    let is_final = matches_mask(class_file.access_flags, ACC_FINAL);
    let is_abstract = matches_mask(class_file.access_flags, ACC_ABSTRACT);
    let is_interface = matches_mask(class_file.access_flags, ACC_INTERFACE);
    let is_enum = matches_mask(class_file.access_flags, ACC_ENUM);
    let is_module = matches_mask(class_file.access_flags, ACC_MODULE);

    let mut name: Option<String> = None;
    if let Constant::ClassInfo { name_index } = this_class_info {
        name = class_file
            .get_constant_utf8(name_index)
            .ok()
            .map(String::from);
    }

    let mut parent_name: Option<String> = None;
    if let Some(Constant::ClassInfo { name_index }) = super_class_info {
        if name_index != 0 {
            parent_name = class_file
                .get_constant_utf8(name_index)
                .ok()
                .map(String::from);
        }
    }

    let mut interfaces: Vec<String> = vec![];
    for index in class_file.interfaces.as_slice() {
        let interface_info = class_file.constant_pool[*index as usize - 1].clone();
        if let Constant::ClassInfo { name_index } = interface_info {
            interfaces.push(
                class_file
                    .get_constant_utf8(name_index)
                    .ok()
                    .map(String::from)
                    .ok_or(DecompilationError::MissingInterface)?,
            );
        }
    }

    let mut fields: Vec<JField> = vec![];
    for field in class_file.field_info.as_slice() {
        let is_private = matches_mask(field.access_flags, ACC_PRIVATE);
        let is_protected = matches_mask(field.access_flags, ACC_PROTECTED);
        let is_static = matches_mask(field.access_flags, ACC_STATIC);
        let is_final = matches_mask(field.access_flags, ACC_FINAL);
        let is_enum_field = matches_mask(field.access_flags, ACC_ENUM);
        let is_volatile = matches_mask(field.access_flags, ACC_VOLATILE);
        let is_transient = matches_mask(field.access_flags, ACC_TRANSIENT);
        let is_synthetic = matches_mask(field.access_flags, ACC_SYNTHETIC);

        let FieldInfo {
            name_index,
            descriptor_index,
            attributes,
            ..
        } = field;

        let initial_value = attributes
            .iter()
            .filter_map(|attr| match attr {
                Attribute::ConstantValue(index) => Some(class_file.get_constant(*index)),
                _ => None,
            })
            .next()
            .map(ToOwned::to_owned);

        let mut visibility = JVisibility::Public;

        if is_private {
            visibility = JVisibility::Private
        }

        if is_protected {
            visibility = JVisibility::Protected
        }

        let field_name = class_file
            .get_constant_utf8(*name_index)
            .ok()
            .map(String::from)
            .ok_or(DecompilationError::MissingField)?;

        let descriptor = class_file
            .get_constant_utf8(*descriptor_index)
            .ok()
            .map(String::from)
            .ok_or(DecompilationError::MissingFieldDescriptor)?;

        fields.push(JField {
            field_name,
            java_type: parse_descriptor(&descriptor)
                .map_err(|_| DecompilationError::InvalidFieldDescriptor)?,
            visibility,
            is_static,
            is_final,
            is_enum_field,
            is_synthetic,
            is_transient,
            is_volatile,
            initial_value: match initial_value {
                Some(value) => match value {
                    Constant::String { string_index } => class_file
                        .get_constant_utf8(string_index)
                        .ok()
                        .map(|v| format!("\"{}\"", v)),
                    Constant::Integer(v) => Some(v.to_string()),
                    Constant::Float(v) => Some(f32::from_bits(v).to_string()),
                    Constant::Long(high, low) => {
                        Some(format!("{}L", (((high as u64) << 32) + (low as u64))))
                    }
                    Constant::Double(high, low) => {
                        Some(f64::from_bits(((high as u64) << 32) + (low as u64)).to_string())
                    }
                    _ => None,
                },
                None => None,
            },
        });
    }

    let mut methods: Vec<JMethod> = vec![];
    for method in class_file.method_info.as_slice() {
        let is_private = matches_mask(method.access_flags, ACC_PRIVATE);
        let is_protected = matches_mask(method.access_flags, ACC_PROTECTED);
        let is_static = matches_mask(method.access_flags, ACC_STATIC);
        let is_final = matches_mask(method.access_flags, ACC_FINAL);
        let is_synthetic = matches_mask(method.access_flags, ACC_SYNTHETIC);
        let is_synchronized = matches_mask(method.access_flags, ACC_SYNCHRONIZED);
        let is_abstract = matches_mask(method.access_flags, ACC_ABSTRACT);
        let is_native = matches_mask(method.access_flags, ACC_NATIVE);
        let is_strict = matches_mask(method.access_flags, ACC_STRICT);
        let is_bridge = matches_mask(method.access_flags, ACC_BRIDGE);
        let has_varargs = matches_mask(method.access_flags, ACC_VARARGS);

        let MethodInfo {
            name_index,
            descriptor_index,
            ..
        } = method;

        let mut visibility = JVisibility::Public;

        if is_private {
            visibility = JVisibility::Private
        }

        if is_protected {
            visibility = JVisibility::Protected
        }

        let method_name = class_file
            .get_constant_utf8(*name_index)
            .ok()
            .map(String::from)
            .ok_or(DecompilationError::MissingMethod)?;

        let descriptor = class_file
            .get_constant_utf8(*descriptor_index)
            .ok()
            .map(String::from)
            .ok_or(DecompilationError::MissingMethodDescriptor)?;

        let MethodDescriptor {
            return_type,
            parameters,
        } = parse_method_descriptor(&descriptor)?;

        let mut arguments: Vec<JArg> = vec![];

        for attribute in method.attributes.as_slice() {
            if let Attribute::MethodParameters(params) = attribute {
                for (counter, (name_index, access_flags)) in params.iter().enumerate() {
                    let is_synthetic = matches_mask(*access_flags, ACC_SYNTHETIC);
                    let is_mandated = matches_mask(*access_flags, ACC_MANDATED);
                    let is_final = matches_mask(*access_flags, ACC_FINAL);

                    let arg_name = match *name_index {
                        0 => None,
                        _ => class_file
                            .get_constant_utf8(*name_index)
                            .ok()
                            .map(String::from),
                    };

                    arguments.push(JArg {
                        is_final,
                        is_mandated,
                        is_synthetic,
                        arg_name,
                        java_type: parameters[counter].clone(),
                    });
                }
            }
        }

        methods.push(JMethod {
            method_name,
            return_type,
            visibility,
            is_static,
            is_final,
            is_abstract,
            is_native,
            is_bridge,
            is_strict,
            is_synchronized,
            is_synthetic,
            has_varargs,
            args: arguments,
        });
    }

    Ok(JClass {
        name: name.ok_or(DecompilationError::MissingClassName)?,
        extends: parent_name,
        fields,
        implements: interfaces,
        is_final,
        is_public,
        is_abstract,
        is_enum,
        is_interface,
        is_module,
        methods,
    })
}

fn parse_descriptor(t: &str) -> DecompilationResult<JType> {
    let (a, _) = parse_type_from(t, 0)?;
    Ok(a)
}

fn parse_type_from(t: &str, i: usize) -> DecompilationResult<(JType, usize)> {
    match t.chars().nth(i).unwrap() {
        'V' => Ok((JType::Void, i + 1)),
        'B' => Ok((JType::Byte, i + 1)),
        'C' => Ok((JType::Char, i + 1)),
        'D' => Ok((JType::Double, i + 1)),
        'F' => Ok((JType::Float, i + 1)),
        'S' => Ok((JType::Short, i + 1)),
        'I' => Ok((JType::Int, i + 1)),
        'J' => Ok((JType::Long, i + 1)),
        'Z' => Ok((JType::Bool, i + 1)),
        'L' => {
            let remaining = &t[i + 1..];
            let pos = remaining.find(';').unwrap();
            let class_name = remaining[0..pos].to_owned();
            Ok((JType::Object(class_name.replace('/', ".")), i + pos + 2))
        }
        '[' => {
            let (t, pos) = parse_type_from(t, i + 1)?;
            Ok((JType::Array(Box::new(t)), pos))
        }
        other => Err(DecompilationError::InvalidFieldDescriptor),
    }
}

fn parse_method_descriptor(descriptor: &str) -> Result<MethodDescriptor, DecompilationError> {
    let mut arguments = vec![];
    let mut processed_args = false;
    let mut i = 0;
    while i < descriptor.chars().count() {
        match descriptor.chars().nth(i).unwrap() {
            '(' => {
                assert_eq!(i, 0);
                i += 1;
            }
            ')' => {
                processed_args = true;
                i += 1;
            }
            _ => {
                let (java_type, offset) = parse_type_from(descriptor, i)?;
                i = offset;
                if processed_args {
                    return Ok(MethodDescriptor {
                        parameters: arguments,
                        return_type: java_type,
                    });
                } else {
                    arguments.push(java_type);
                }
            }
        }
    }
    unreachable!()
}

fn matches_mask(flags: u16, mask: u16) -> bool {
    (flags & mask) != 0x0
}

#[cfg(test)]
mod tests {
    use crate::java::{
        matches_mask, ACC_ENUM, ACC_FINAL, ACC_PRIVATE, ACC_PROTECTED, ACC_PUBLIC, ACC_STATIC,
    };

    use super::{parse_descriptor, parse_method_descriptor, JType, MethodDescriptor};

    #[test]
    fn test_parse_descriptor_regular_type() {
        let desc = "B".to_string();
        let result = parse_descriptor(&desc);

        assert_eq!(result, Ok(JType::Byte));
    }

    #[test]
    fn test_parse_descriptor_array_type() {
        let desc = "[Z".to_string();
        let result = parse_descriptor(&desc);

        assert_eq!(result, Ok(JType::Array(Box::new(JType::Bool))));
    }
    #[test]
    fn test_parse_descriptor_multi_dim_array_type() {
        let desc = "[[[Ljava.lang.Object;".to_string();
        let result = parse_descriptor(&desc);

        assert_eq!(
            result,
            Ok(JType::Array(Box::new(JType::Array(Box::new(
                JType::Array(Box::new(JType::Object("java.lang.Object".to_string())))
            )))))
        );
    }
    #[test]
    fn test_parse_method_descriptor_no_params() {
        let desc = "()V".to_string();
        let result = parse_method_descriptor(&desc);

        assert_eq!(
            result,
            Ok(MethodDescriptor {
                parameters: vec![],
                return_type: JType::Void
            })
        );
    }
    #[test]
    fn test_parse_method_descriptor_with_params() {
        let desc = "(BC[Z)I".to_string();
        let result = parse_method_descriptor(&desc);

        assert_eq!(
            result,
            Ok(MethodDescriptor {
                parameters: vec![
                    JType::Byte,
                    JType::Char,
                    JType::Array(Box::new(JType::Bool))
                ],
                return_type: JType::Int
            })
        );
    }
    #[test]
    fn test_access_flag_masks() {
        assert!(matches_mask(0x4001, ACC_PUBLIC));
        assert!(matches_mask(0x4002, ACC_PRIVATE));
        assert!(matches_mask(0x4004, ACC_PROTECTED));
        assert!(matches_mask(0x4004, ACC_ENUM));
        assert!(matches_mask(0x4008, ACC_STATIC));
        assert!(matches_mask(0x4010, ACC_FINAL));
    }
}
